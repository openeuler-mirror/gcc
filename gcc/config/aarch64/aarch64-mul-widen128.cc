/* AArch64 gimple pass: recognize a hand-written 64x64 -> 128 schoolbook
   multiply and rewrite it to the target's widening multiply.

   Copyright (C) 2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Code that needs the full 128-bit product of two 64-bit values, on a
   compiler or target where neither __int128 nor an intrinsic is available,
   writes it by hand: split both operands at 32 bits, form the four
   quadrant products, and reassemble them with shifts and carries.  This
   pass recognizes that shape and hands the work to the hardware instead.

   Four shapes are matched, sharing one structural front end - find the
   quadrant products of one operand pair, check the mask and shift
   constants exactly, and require unsigned 64-bit operands:

     - both halves, low half assembled by a bit splice
	 lo = (temp << 32) | (ll & 0xffffffff)
     - both halves, low half assembled by a wrapping addition, with the
       carries spelled as explicit comparisons
	 lo = ll + (mid << 32)
     - the high half alone, when the caller discards the low one
     - the low half alone, when the caller discards the high one

   The first three become

     prod = X w* Y                           ; WIDEN_MULT_EXPR<TI>(DI, DI)
     hi   = (uint64_t) (prod >> 64)
     lo   = (uint64_t) prod

   with the three high-half contributions the source added alongside HH
   overwritten with zero, since HH now carries the whole high half.  The
   TImode product is lowered by the existing backend to `mul + umulh`,
   giving the code the source would have got from __uint128_t.  The low
   half alone needs no widening at all and becomes a plain 64-bit
   multiply.

   The pass runs after FORWPROP4, which is where the idiom is at its most
   regular: carries written as `?:' selections have been flattened into
   arithmetic by PHIOPT2, the additions have been through REASSOC2 and
   DOM3, and UADDC recognition in WIDENING_MUL has not yet turned
   anything into .ADD_OVERFLOW.  Enable with -mmul-widen128.

   Known recognition gaps, each pinned by an xfail test - see the TODO
   above find_quadrant_mul.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "tree-ssa.h"

namespace {

/* Defining stmt of an SSA name, or NULL if T is not an SSA name with a real
   def.  */
gimple *
def_of (tree t)
{
  if (TREE_CODE (t) != SSA_NAME)
    return NULL;
  gimple *s = SSA_NAME_DEF_STMT (t);
  if (gimple_nop_p (s))
    return NULL;
  return s;
}

/* Return STMT's assignment code, or ERROR_MARK if STMT isn't a gimple
   assignment.  */
enum tree_code
assign_code (gimple *stmt)
{
  if (!stmt || !is_gimple_assign (stmt))
    return ERROR_MARK;
  return gimple_assign_rhs_code (stmt);
}

/* True if T is an unsigned 64-bit integer value.  The schoolbook pattern we
   rewrite is specifically uint64_t x uint64_t -> uint128_t; accepting signed
   or differently-sized values risks changing multiplication semantics.  */
bool
is_unsigned_64 (tree t)
{
  if (!t)
    return false;
  tree type = TREE_TYPE (t);
  return INTEGRAL_TYPE_P (type)
	 && TYPE_UNSIGNED (type)
	 && TYPE_PRECISION (type) == 64;
}

/* True if STMT is `R = X CODE VAL` with an integer constant VAL on the
   right; sets *SRC to X.  The constant is compared exactly: the rewrite is
   only valid for the 32/32 split, so a different mask or shift amount must
   not match.  */
bool
is_binop_const (gimple *stmt, enum tree_code code, unsigned HOST_WIDE_INT val,
		tree *src)
{
  gcc_checking_assert (TREE_CODE_LENGTH (code) == 2);
  if (assign_code (stmt) != code)
    return false;
  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (rhs2) != INTEGER_CST
      || !tree_fits_uhwi_p (rhs2)
      || tree_to_uhwi (rhs2) != val)
    return false;
  *src = gimple_assign_rhs1 (stmt);
  return true;
}

/* True if STMT is `R = X * Y`; sets *X_OUT, *Y_OUT.  */
bool
is_mul (gimple *stmt, tree *x_out, tree *y_out)
{
  if (assign_code (stmt) != MULT_EXPR)
    return false;
  *x_out = gimple_assign_rhs1 (stmt);
  *y_out = gimple_assign_rhs2 (stmt);
  return true;
}

/* Decompose OP into (root, is_low) where OP = root & 0xFFFFFFFF (low) or
   OP = root >> 32 (high).  */
bool
get_half (tree op, bool *is_low_out, tree *root_out)
{
  gimple *d = def_of (op);
  if (!d)
    return false;
  tree src;
  if (is_binop_const (d, BIT_AND_EXPR, 0xFFFFFFFFULL, &src))
    {
      *is_low_out = true;
      *root_out = src;
      return true;
    }
  if (is_binop_const (d, RSHIFT_EXPR, 32, &src))
    {
      *is_low_out = false;
      *root_out = src;
      return true;
    }
  return false;
}

/* Equality of two operands of the shapes this pass walks: SSA names, and
   the roots returned by get_half, which are is_gimple_val and so may be
   integer constants.  Identity is the whole test for both - distinct SSA
   names are never equal, and integer constants are interned by type and
   value.  */
bool
ssa_eq (tree a, tree b)
{
  return a == b;
}

/* Bundle of stmts identified by a successful schoolbook match.  */
struct schoolbook_match
{
  tree X, Y;                    /* DImode source operands.  */
  gimple *HH_stmt;              /* X_hi * Y_hi; becomes the high half */
  gimple *lo_out_stmt;          /* the statement producing the low half */
  gimple *mid_hi_stmt;          /* mid >> 32                         */
  gimple *ts_hi_stmt;           /* carry out of the low half          */
  gimple *carry32_stmt;         /* carry out of the middle sum        */
};

/* TODO: two recognition gaps, both recorded by xfail tests rather than
   hidden, both fixable inside this pass.

   1. Carries spelled as statements - `if (lo < a) hi += C' - reach this
      pass as PHI diamonds and are not matched; only the flattened
      spelling `hi += (uint64_t) (lo < a) << k' is.  An earlier revision
      covered the `if' spelling with a downstream match.pd pattern that
      flattened `c ? x + 2^k : x' for every pass and every target; that
      perturbed PRE and SLSR on code with no multiply in sight and
      turned three upstream tests red (gcc.dg/tree-ssa/loadpre19.c,
      slsr-35.c, slsr-36.c), so it was removed.  The fix that does not
      leak outside the idiom is local recognition: accept a carry
      contribution that arrives as PHI <hi + 2^k, hi> whose controlling
      comparison is the carry test this pass already checks.  Until
      then, bignum sources that write their carries as `if' statements
      keep their four-multiply schoolbook.
      Test: mul-widen128-addlo-1.c.
      Related nit: find_lo_carry takes the first comparison off LO's
      use list that passes its checks and never backtracks, so a
      duplicate of the carry comparison feeding something outside the
      high-half chain can eat the full match (it degrades to
      low-half-only; the values stay right - measured).

   2. Every matcher searches the block holding its anchor statement, the
      low half's block.  When the high half is stored on one path only
      or inside a loop, pass_sink_code - which runs earlier - moves the
      high-half statements next to their consumer, the full match fails,
      and the rewrite degrades to a low-half-only 64-bit multiply (the
      values stay right).  Re-anchoring the high-half searches at
      plus_chain_root's block would recover the conditional shapes.  It
      would not recover the zero-trip-loop shape: there the high half is
      stored twice, reassoc/PRE hoist the shared partial sum `hh +
      mid_hi' back into the low half's block with two uses, and
      same_plus_chain needs each contribution consumed exactly once.
      Tests: mul-widen128-sink-{cond,cond2,zerotrip}-1.c.  */

/* Find the DImode multiply stmt in BB that computes ROOT_X_HALF * ROOT_Y_HALF
   (modulo commute), where each operand is the (is_low? AND : RSHIFT) form
   over its respective root.

   The EXCLUDE arguments are defensive rather than load-bearing: a query
   for one quadrant cannot match a multiply already claimed as a
   different quadrant, since the two differ in at least one of the four
   (root, half) pairs being asked for.  Removing them changes nothing
   measurable; they stay so that a future caller reusing this for the
   same quadrant twice does not get the same statement back.  */
gimple *
find_quadrant_mul (basic_block bb, tree X, tree Y, bool x_is_low, bool y_is_low,
		   gimple *exclude1, gimple *exclude2, gimple *exclude3)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (stmt == exclude1 || stmt == exclude2 || stmt == exclude3)
	continue;
      if (assign_code (stmt) != MULT_EXPR)
	continue;
      tree a = gimple_assign_rhs1 (stmt);
      tree b = gimple_assign_rhs2 (stmt);
      bool a_low, b_low;
      tree a_root, b_root;
      if (!get_half (a, &a_low, &a_root) || !get_half (b, &b_low, &b_root))
	continue;
      /* Try (a~X, b~Y).  */
      if (ssa_eq (a_root, X) && a_low == x_is_low
	  && ssa_eq (b_root, Y) && b_low == y_is_low)
	return stmt;
      /* Try (a~Y, b~X) since multiplication is commutative.  */
      if (ssa_eq (a_root, Y) && a_low == y_is_low
	  && ssa_eq (b_root, X) && b_low == x_is_low)
	return stmt;
    }
  return NULL;
}

/* Return the final PLUS_EXPR in the single-use addition chain that consumes
   STMT's lhs.  Return NULL if STMT's lhs is not consumed by a PLUS_EXPR.

   The rewrite below replaces HH with the final high half and replaces the
   other three high-half contributions with zero.  This is only correct if all
   four contributions feed the same addition chain; otherwise unrelated sums
   such as `hh + z` and `(mid >> 32) + z` would be changed independently.  */
gimple *
plus_chain_root (gimple *s)
{
  tree lhs = gimple_assign_lhs (s);
  use_operand_p up;
  gimple *us;
  if (!single_imm_use (lhs, &up, &us))
    return NULL;
  if (!is_gimple_assign (us)
      || gimple_assign_rhs_code (us) != PLUS_EXPR)
    return NULL;

  gimple *root = us;
  while (true)
    {
      lhs = gimple_assign_lhs (root);
      if (!single_imm_use (lhs, &up, &us))
	return root;
      if (!is_gimple_assign (us)
	  || gimple_assign_rhs_code (us) != PLUS_EXPR)
	return root;
      root = us;
    }
}

/* True if the four high-half contribution statements all feed the same
   single-use addition chain.  */
bool
same_plus_chain (gimple *hh_stmt, gimple *mid_hi_stmt, gimple *ts_hi_stmt,
		 gimple *carry32_stmt)
{
  gimple *root = plus_chain_root (hh_stmt);
  return root
	 && plus_chain_root (mid_hi_stmt) == root
	 && plus_chain_root (ts_hi_stmt) == root
	 && plus_chain_root (carry32_stmt) == root;
}

/* Find a `SOURCE >> 32` immediate user in BB.  */
gimple *
find_rshift32_user (basic_block bb, gimple *source)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, gimple_assign_lhs (source))
    {
      gimple *u = USE_STMT (use_p);
      tree dummy;
      if (gimple_bb (u) == bb && is_binop_const (u, RSHIFT_EXPR, 32, &dummy))
	return u;
    }
  return NULL;
}

/* Find `temp_sum = (LL >> 32) + (mid & 0xffffffff)` in BB.  */
gimple *
find_temp_sum (basic_block bb, gimple *LL_stmt, gimple *mid_stmt)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *s = gsi_stmt (gsi);
      if (assign_code (s) != PLUS_EXPR)
	continue;
      for (int swap = 0; swap < 2; ++swap)
	{
	  tree A = swap ? gimple_assign_rhs2 (s) : gimple_assign_rhs1 (s);
	  tree B = swap ? gimple_assign_rhs1 (s) : gimple_assign_rhs2 (s);
	  gimple *A_def = def_of (A);
	  gimple *B_def = def_of (B);
	  tree A_src, B_src;
	  if (A_def && is_binop_const (A_def, RSHIFT_EXPR, 32, &A_src)
	      && B_def && is_binop_const (B_def, BIT_AND_EXPR, 0xFFFFFFFFULL, &B_src)
	      && ssa_eq (A_src, gimple_assign_lhs (LL_stmt))
	      && ssa_eq (B_src, gimple_assign_lhs (mid_stmt)))
	    return s;
	}
    }
  return NULL;
}

/* Find carry32 = ((uint64_t) (M > mid or mid < M)) << 32, where M is either
   of the two cross products.  */
gimple *
find_carry32 (basic_block bb, gimple *LH_stmt, gimple *HL_stmt,
	      gimple *mid_stmt)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *s = gsi_stmt (gsi);
      tree pre;
      if (!is_binop_const (s, LSHIFT_EXPR, 32, &pre))
	continue;
      gimple *pre_def = def_of (pre);
      if (!pre_def || !is_gimple_assign (pre_def)
	  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (pre_def)))
	continue;
      tree cmp_src = gimple_assign_rhs1 (pre_def);
      gimple *cmp_def = def_of (cmp_src);
      if (!cmp_def || !is_gimple_assign (cmp_def))
	continue;
      enum tree_code cc = gimple_assign_rhs_code (cmp_def);
      if (cc != GT_EXPR && cc != LT_EXPR)
	continue;
      tree ca = gimple_assign_rhs1 (cmp_def);
      tree cb = gimple_assign_rhs2 (cmp_def);
      tree m_hl = gimple_assign_lhs (HL_stmt);
      tree m_lh = gimple_assign_lhs (LH_stmt);
      tree m_mid = gimple_assign_lhs (mid_stmt);
      /* Both orders are accepted, but only the GT one is reachable from
	 source: tree_swap_operands_p rewrites `a < b' to `b > a' when a
	 was defined after b, and a sum is always defined after its
	 addends, so `mid < LH' arrives here as `LH > mid'.  Measured -
	 disabling the LT arm changes nothing in the suite, nor in a
	 generated corpus of every carry spelling.  It is kept as a
	 defence against a future canonicalization change, not as live
	 coverage; do not read it as tested.  */
      bool ok = false;
      if (cc == GT_EXPR)
	ok = (ssa_eq (ca, m_hl) && ssa_eq (cb, m_mid))
	     || (ssa_eq (ca, m_lh) && ssa_eq (cb, m_mid));
      else /* LT_EXPR */
	ok = (ssa_eq (ca, m_mid) && ssa_eq (cb, m_hl))
	     || (ssa_eq (ca, m_mid) && ssa_eq (cb, m_lh));
      if (ok)
	return s;
    }
  return NULL;
}

/* True if CAND_LH and CAND_HL are the two cross products for one uint64 pair.
   Sets X/Y roots so CAND_LH is X_lo * Y_hi and CAND_HL is X_hi * Y_lo.  */
bool
match_cross_pair (tree lh_a, tree lh_b, tree hl_a, tree hl_b,
		  tree *X_root, tree *Y_root)
{
  bool lh_a_low, lh_b_low;
  tree lh_a_root, lh_b_root;
  if (!get_half (lh_a, &lh_a_low, &lh_a_root)
      || !get_half (lh_b, &lh_b_low, &lh_b_root))
    return false;

  tree X = NULL_TREE, Y = NULL_TREE;
  if (lh_a_low && !lh_b_low)
    { X = lh_a_root; Y = lh_b_root; }
  else if (!lh_a_low && lh_b_low)
    { X = lh_b_root; Y = lh_a_root; }
  else
    return false;

  bool hl_a_low, hl_b_low;
  tree hl_a_root, hl_b_root;
  if (!get_half (hl_a, &hl_a_low, &hl_a_root)
      || !get_half (hl_b, &hl_b_low, &hl_b_root))
    return false;

  bool ok = (ssa_eq (hl_a_root, X) && !hl_a_low
	     && ssa_eq (hl_b_root, Y) && hl_b_low)
	    || (ssa_eq (hl_a_root, Y) && hl_a_low
		&& ssa_eq (hl_b_root, X) && !hl_b_low);
  if (!ok || !is_unsigned_64 (X) || !is_unsigned_64 (Y))
    return false;

  *X_root = X;
  *Y_root = Y;
  return true;
}

/* True if CX * CY is the requested low/high cross product for X_ROOT and
   Y_ROOT.  */
bool
verify_cross (tree cx, tree cy, tree X_root, tree Y_root,
	      bool need_x_low, bool need_y_low)
{
  bool cx_lo, cy_lo;
  tree cx_root, cy_root;
  if (!get_half (cx, &cx_lo, &cx_root))
    return false;
  if (!get_half (cy, &cy_lo, &cy_root))
    return false;
  if (ssa_eq (cx_root, X_root) && cx_lo == need_x_low
      && ssa_eq (cy_root, Y_root) && cy_lo == need_y_low)
    return true;
  if (ssa_eq (cx_root, Y_root) && cx_lo == need_y_low
      && ssa_eq (cy_root, X_root) && cy_lo == need_x_low)
    return true;
  return false;
}

/* Try to identify the schoolbook anchored at IOR_STMT.  */
bool
try_match (gimple *ior_stmt, schoolbook_match *m)
{
  if (assign_code (ior_stmt) != BIT_IOR_EXPR)
    return false;

  tree u = gimple_assign_rhs1 (ior_stmt);
  tree v = gimple_assign_rhs2 (ior_stmt);

  /* Either side is `<<32`, the other is `&0xFFFFFFFF`.  */
  gimple *u_def = def_of (u);
  gimple *v_def = def_of (v);
  tree ts_pre_lshift = NULL_TREE, ll_pre_and = NULL_TREE;
  if (u_def && v_def && is_binop_const (u_def, LSHIFT_EXPR, 32, &ts_pre_lshift)
      && is_binop_const (v_def, BIT_AND_EXPR, 0xFFFFFFFFULL, &ll_pre_and))
    ;
  else if (u_def && v_def && is_binop_const (u_def, BIT_AND_EXPR, 0xFFFFFFFFULL, &ll_pre_and)
	   && is_binop_const (v_def, LSHIFT_EXPR, 32, &ts_pre_lshift))
    ;
  else
    return false;

  /* `ll_pre_and` must be the LL multiply lhs.  */
  gimple *LL_stmt = def_of (ll_pre_and);
  if (!LL_stmt || assign_code (LL_stmt) != MULT_EXPR)
    return false;

  /* `ts_pre_lshift` should be `temp_sum = (LL>>32) + (mid&0xFFFFFFFF)`.  */
  gimple *temp_sum_stmt = def_of (ts_pre_lshift);
  if (assign_code (temp_sum_stmt) != PLUS_EXPR)
    return false;
  tree ll_via_rshift = NULL_TREE, mid_via_and = NULL_TREE;
  for (int swap = 0; swap < 2; ++swap)
    {
      tree A = swap ? gimple_assign_rhs2 (temp_sum_stmt)
		    : gimple_assign_rhs1 (temp_sum_stmt);
      tree B = swap ? gimple_assign_rhs1 (temp_sum_stmt)
		    : gimple_assign_rhs2 (temp_sum_stmt);
      gimple *A_def = def_of (A);
      gimple *B_def = def_of (B);
      tree A_src, B_src;
      if (A_def && is_binop_const (A_def, RSHIFT_EXPR, 32, &A_src)
	  && B_def && is_binop_const (B_def, BIT_AND_EXPR, 0xFFFFFFFFULL, &B_src))
	{
	  ll_via_rshift = A_src;
	  mid_via_and = B_src;
	  break;
	}
    }
  if (!ll_via_rshift || !mid_via_and)
    return false;
  if (!ssa_eq (ll_via_rshift, gimple_assign_lhs (LL_stmt)))
    return false;

  /* `mid_via_and` should be `mid = LH + HL` (commutative).  */
  gimple *mid_stmt = def_of (mid_via_and);
  if (assign_code (mid_stmt) != PLUS_EXPR)
    return false;
  tree m_a = gimple_assign_rhs1 (mid_stmt);
  tree m_b = gimple_assign_rhs2 (mid_stmt);
  gimple *ma_def = def_of (m_a);
  gimple *mb_def = def_of (m_b);
  tree ma_x, ma_y, mb_x, mb_y;
  if (!ma_def || !is_mul (ma_def, &ma_x, &ma_y)) return false;
  if (!mb_def || !is_mul (mb_def, &mb_x, &mb_y)) return false;

  /* Halves of LL: must be (X_lo, Y_lo) for some pair X, Y.  */
  tree ll_x = gimple_assign_rhs1 (LL_stmt);
  tree ll_y = gimple_assign_rhs2 (LL_stmt);
  bool llx_lo, lly_lo;
  tree X_root, Y_root;
  if (!get_half (ll_x, &llx_lo, &X_root)) return false;
  if (!get_half (ll_y, &lly_lo, &Y_root)) return false;
  if (!llx_lo || !lly_lo)
    return false;       /* LL must be lo*lo.  */
  if (!is_unsigned_64 (X_root) || !is_unsigned_64 (Y_root))
    return false;

  gimple *LH_stmt = NULL, *HL_stmt = NULL;
  /* (ma is LH, mb is HL)?  */
  if (verify_cross (ma_x, ma_y, X_root, Y_root, true, false)
      && verify_cross (mb_x, mb_y, X_root, Y_root, false, true))
    { LH_stmt = ma_def; HL_stmt = mb_def; }
  /* Or (ma is HL, mb is LH).  */
  else if (verify_cross (ma_x, ma_y, X_root, Y_root, false, true)
	   && verify_cross (mb_x, mb_y, X_root, Y_root, true, false))
    { LH_stmt = mb_def; HL_stmt = ma_def; }
  else
    return false;

  /* Find HH (X_hi * Y_hi).  */
  basic_block bb = gimple_bb (ior_stmt);
  gimple *HH_stmt = find_quadrant_mul (bb, X_root, Y_root,
				       /*x_low=*/false, /*y_low=*/false,
				       LL_stmt, LH_stmt, HL_stmt);
  if (!HH_stmt)
    return false;

  /* Locate carry-test plus-chain ingredients:
       mid_hi    = mid >> 32
       carry_bit = HL > mid  (or mid < HL)        -- we accept either
       carry64   = (uint64_t) carry_bit
       carry32   = carry64 << 32
       ts_hi     = temp_sum >> 32
     Then walk a single-use plus-chain rooted at HH_lhs that consumes
     {mid_hi, carry32, ts_hi} in any order.  */

  /* Find mid_hi = mid >> 32 in the BB by scanning users of mid_stmt's lhs.  */
  gimple *mid_hi_stmt = find_rshift32_user (bb, mid_stmt);
  if (!mid_hi_stmt)
    return false;

  /* Find ts_hi = temp_sum >> 32.  */
  gimple *ts_hi_stmt = find_rshift32_user (bb, temp_sum_stmt);
  if (!ts_hi_stmt)
    return false;

  gimple *carry32_stmt = find_carry32 (bb, LH_stmt, HL_stmt, mid_stmt);
  if (!carry32_stmt)
    return false;

  /* The four HI components must contribute to the same additive result.  */
  if (!same_plus_chain (HH_stmt, mid_hi_stmt, ts_hi_stmt, carry32_stmt))
    return false;

  /* All checks passed -- populate the match record.  */
  m->X = X_root;
  m->Y = Y_root;
  m->HH_stmt = HH_stmt;
  m->lo_out_stmt = ior_stmt;
  m->mid_hi_stmt = mid_hi_stmt;
  m->ts_hi_stmt = ts_hi_stmt;
  m->carry32_stmt = carry32_stmt;
  return true;
}

/* Try to identify a schoolbook whose low half has been DCE'd or was never
   materialized, anchored at `mid = LH + HL`.  This catches accumulator-style
   callers that only consume the high half.  */
bool
try_match_hi_only (gimple *mid_stmt, schoolbook_match *m)
{
  if (assign_code (mid_stmt) != PLUS_EXPR)
    return false;

  tree m_a = gimple_assign_rhs1 (mid_stmt);
  tree m_b = gimple_assign_rhs2 (mid_stmt);
  gimple *ma_def = def_of (m_a);
  gimple *mb_def = def_of (m_b);
  tree ma_x, ma_y, mb_x, mb_y;
  if (!ma_def || !is_mul (ma_def, &ma_x, &ma_y))
    return false;
  if (!mb_def || !is_mul (mb_def, &mb_x, &mb_y))
    return false;

  tree X_root = NULL_TREE, Y_root = NULL_TREE;
  gimple *LH_stmt = NULL, *HL_stmt = NULL;
  if (match_cross_pair (ma_x, ma_y, mb_x, mb_y, &X_root, &Y_root))
    { LH_stmt = ma_def; HL_stmt = mb_def; }
  else if (match_cross_pair (mb_x, mb_y, ma_x, ma_y, &X_root, &Y_root))
    { LH_stmt = mb_def; HL_stmt = ma_def; }
  else
    return false;

  basic_block bb = gimple_bb (mid_stmt);

  gimple *LL_stmt = find_quadrant_mul (bb, X_root, Y_root,
					true, true, LH_stmt, HL_stmt, NULL);
  if (!LL_stmt)
    return false;

  gimple *HH_stmt = find_quadrant_mul (bb, X_root, Y_root,
				       /*x_low=*/false, /*y_low=*/false,
				       LL_stmt, LH_stmt, HL_stmt);
  if (!HH_stmt)
    return false;

  gimple *temp_sum_stmt = find_temp_sum (bb, LL_stmt, mid_stmt);
  if (!temp_sum_stmt)
    return false;

  gimple *mid_hi_stmt = find_rshift32_user (bb, mid_stmt);
  if (!mid_hi_stmt)
    return false;

  gimple *ts_hi_stmt = find_rshift32_user (bb, temp_sum_stmt);
  if (!ts_hi_stmt)
    return false;

  gimple *carry32_stmt = find_carry32 (bb, LH_stmt, HL_stmt, mid_stmt);
  if (!carry32_stmt)
    return false;

  if (!same_plus_chain (HH_stmt, mid_hi_stmt, ts_hi_stmt, carry32_stmt))
    return false;

  m->X = X_root;
  m->Y = Y_root;
  m->HH_stmt = HH_stmt;
  m->lo_out_stmt = NULL;
  m->mid_hi_stmt = mid_hi_stmt;
  m->ts_hi_stmt = ts_hi_stmt;
  m->carry32_stmt = carry32_stmt;
  return true;
}

/* Find the carry out of the low-half addition `LO = LL + ADDC32`.  An
   unsigned sum of two values wraps exactly when it comes out below either
   addend, so both `ADDC32 > LO` and `LL > LO` are the carry, and
   hand-written code uses all four spellings of that.  They do not all
   reach here as written: tree_swap_operands_p turns `LO < ADDC32' into
   `ADDC32 > LO', so the two LT spellings arrive as the two GT ones and
   the LT arm below is defensive rather than tested - measured.  Return the
   conversion stmt: that value is the high-half contribution, the same role
   `temp_sum >> 32` plays in the bit-splice formulation.  */
gimple *
find_lo_carry (basic_block bb, gimple *addc32_stmt, gimple *ll_stmt,
	       gimple *lo_stmt)
{
  tree lo = gimple_assign_lhs (lo_stmt);
  tree addc32 = gimple_assign_lhs (addc32_stmt);
  tree ll = gimple_assign_lhs (ll_stmt);
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lo)
    {
      gimple *cmp = USE_STMT (use_p);
      if (gimple_bb (cmp) != bb || !is_gimple_assign (cmp))
	continue;
      enum tree_code cc = gimple_assign_rhs_code (cmp);
      if (cc != GT_EXPR && cc != LT_EXPR)
	continue;
      /* The compared-against value must be one of this addition's own
	 addends.  The other operand needs no check of its own: CMP came
	 off LO's use list, so when LO is not the sum it occupies the
	 addend slot, and LO is neither ADDC32 nor LL.  */
      tree addend = (cc == GT_EXPR) ? gimple_assign_rhs1 (cmp)
				    : gimple_assign_rhs2 (cmp);
      if (!ssa_eq (addend, addc32) && !ssa_eq (addend, ll))
	continue;
      /* The comparison must feed exactly one widening conversion, which is
	 what the high half adds in.  Only the comparison is required to be
	 in BB: pass_sink_code will move the conversion into a loop
	 preheader while leaving the comparison behind, which is what
	 happens whenever the high half is consumed inside a loop, and
	 declining there would give up a schoolbook that is otherwise a
	 perfect match - measured, and the relaxed match verified against
	 the target's own 128-bit multiply.

	 Requiring both was tried and reverted.  The stated reason - that
	 it keeps claim_stmts from seeing a statement outside BB - does not
	 hold: claim_stmts compares pointers, rewrite_match reaches each
	 statement through gsi_for_stmt, which resolves in that statement's
	 own block, and SSA guarantees BB dominates wherever the conversion
	 was sunk to.  */
      use_operand_p conv_use;
      gimple *conv;
      if (!single_imm_use (gimple_assign_lhs (cmp), &conv_use, &conv))
	continue;
      if (!is_gimple_assign (conv)
	  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (conv))
	  || !is_unsigned_64 (gimple_assign_lhs (conv)))
	continue;
      return conv;
    }
  return NULL;
}

struct add_lo_parts
{
  tree X, Y;
  gimple *LL_stmt, *LH_stmt, *HL_stmt, *mid_stmt, *addc32_stmt;
};

/* Verify that LO_STMT is `LO = LL + (mid << 32)` with mid = X_hi*Y_lo +
   X_lo*Y_hi and LL = X_lo*Y_lo, all over unsigned 64-bit X and Y in the
   same block.  That sum is the low half of X*Y whether or not the high
   half is computed anywhere.  */
bool
match_add_lo_parts (gimple *lo_stmt, add_lo_parts *p)
{
  if (assign_code (lo_stmt) != PLUS_EXPR
      || !is_unsigned_64 (gimple_assign_lhs (lo_stmt)))
    return false;

  tree a = gimple_assign_rhs1 (lo_stmt);
  tree b = gimple_assign_rhs2 (lo_stmt);
  gimple *a_def = def_of (a);
  gimple *b_def = def_of (b);
  if (!a_def || !b_def)
    return false;

  /* One addend is `mid << 32`, the other is the LL quadrant product.  */
  tree mid_pre = NULL_TREE;
  gimple *addc32_stmt, *LL_stmt;
  if (is_binop_const (a_def, LSHIFT_EXPR, 32, &mid_pre))
    { addc32_stmt = a_def; LL_stmt = b_def; }
  else if (is_binop_const (b_def, LSHIFT_EXPR, 32, &mid_pre))
    { addc32_stmt = b_def; LL_stmt = a_def; }
  else
    return false;

  gimple *mid_stmt = def_of (mid_pre);
  if (!mid_stmt || assign_code (mid_stmt) != PLUS_EXPR)
    return false;

  /* mid = LH + HL, giving the X and Y roots.  */
  tree m_a = gimple_assign_rhs1 (mid_stmt);
  tree m_b = gimple_assign_rhs2 (mid_stmt);
  gimple *ma_def = def_of (m_a);
  gimple *mb_def = def_of (m_b);
  tree ma_x, ma_y, mb_x, mb_y;
  if (!ma_def || !is_mul (ma_def, &ma_x, &ma_y))
    return false;
  if (!mb_def || !is_mul (mb_def, &mb_x, &mb_y))
    return false;

  tree X_root = NULL_TREE, Y_root = NULL_TREE;
  gimple *LH_stmt, *HL_stmt;
  if (match_cross_pair (ma_x, ma_y, mb_x, mb_y, &X_root, &Y_root))
    { LH_stmt = ma_def; HL_stmt = mb_def; }
  else if (match_cross_pair (mb_x, mb_y, ma_x, ma_y, &X_root, &Y_root))
    { LH_stmt = mb_def; HL_stmt = ma_def; }
  else
    return false;

  /* The other addend of LO must be X_lo * Y_lo.  */
  tree ll_x, ll_y;
  if (!is_mul (LL_stmt, &ll_x, &ll_y)
      || !verify_cross (ll_x, ll_y, X_root, Y_root, true, true))
    return false;

  basic_block bb = gimple_bb (lo_stmt);
  if (gimple_bb (mid_stmt) != bb || gimple_bb (LL_stmt) != bb)
    return false;

  p->X = X_root;
  p->Y = Y_root;
  p->LL_stmt = LL_stmt;
  p->LH_stmt = LH_stmt;
  p->HL_stmt = HL_stmt;
  p->mid_stmt = mid_stmt;
  p->addc32_stmt = addc32_stmt;
  return true;
}

/* Try to identify the schoolbook whose low half is assembled by a wrapping
   addition rather than a bit splice, anchored at `LO = LL + (mid << 32)`:

     LO = LL + (mid << 32)              -- lo_stmt, wraps mod 2^64
     HI = HH + (mid >> 32)
	     + (uint64_t) ((mid << 32) > LO)      -- carry out of LO
	     + ((uint64_t) (HL > mid) << 32)      -- carry out of mid

   This is the formulation used by hand-written bignum code (OpenSSL BN and
   friends).  The carries must be spelled as explicit comparisons; a carry
   written as an `if' statement reaches this pass as a PHI diamond and is
   not matched - the first gap in the TODO above find_quadrant_mul.  The
   quadrant products and the high-half contributions are identical to the
   bit-splice form; only the low half and the shape of its carry differ.  */
bool
try_match_add_lo (gimple *lo_stmt, schoolbook_match *m)
{
  add_lo_parts p;
  if (!match_add_lo_parts (lo_stmt, &p))
    return false;

  tree X_root = p.X, Y_root = p.Y;
  gimple *LL_stmt = p.LL_stmt, *LH_stmt = p.LH_stmt, *HL_stmt = p.HL_stmt;
  gimple *mid_stmt = p.mid_stmt, *addc32_stmt = p.addc32_stmt;
  basic_block bb = gimple_bb (lo_stmt);

  gimple *HH_stmt = find_quadrant_mul (bb, X_root, Y_root,
				       /*x_low=*/false, /*y_low=*/false,
				       LL_stmt, LH_stmt, HL_stmt);
  if (!HH_stmt)
    return false;

  gimple *mid_hi_stmt = find_rshift32_user (bb, mid_stmt);
  if (!mid_hi_stmt)
    return false;

  gimple *lo_carry_stmt = find_lo_carry (bb, addc32_stmt, LL_stmt, lo_stmt);
  if (!lo_carry_stmt)
    return false;

  gimple *carry32_stmt = find_carry32 (bb, LH_stmt, HL_stmt, mid_stmt);
  if (!carry32_stmt)
    return false;

  if (!same_plus_chain (HH_stmt, mid_hi_stmt, lo_carry_stmt, carry32_stmt))
    return false;

  m->X = X_root;
  m->Y = Y_root;
  m->HH_stmt = HH_stmt;
  m->lo_out_stmt = lo_stmt;
  m->mid_hi_stmt = mid_hi_stmt;
  m->ts_hi_stmt = lo_carry_stmt;
  m->carry32_stmt = carry32_stmt;
  return true;
}

/* Try to identify a schoolbook whose high half is never computed, anchored
   at `LO = LL + (mid << 32)`.  Callers that only want the low 64 bits of
   the product write exactly this and nothing else; the sum is X*Y modulo
   2^64 no matter what the discarded high half would have been, so no carry
   or high-half contributor has to be present for the rewrite to be valid.
   Signalled to the rewriter by a null HH_stmt.  */
bool
try_match_lo_only (gimple *lo_stmt, schoolbook_match *m)
{
  add_lo_parts p;
  if (!match_add_lo_parts (lo_stmt, &p))
    return false;

  m->X = p.X;
  m->Y = p.Y;
  m->HH_stmt = NULL;
  m->lo_out_stmt = lo_stmt;
  m->mid_hi_stmt = NULL;
  m->ts_hi_stmt = NULL;
  m->carry32_stmt = NULL;
  return true;
}

/* Rewrite a low-half-only match: the statement that assembled the low half
   becomes a plain 64-bit multiply.  Nothing else is touched - the value is
   equal, so other uses stay correct, and the quadrant products die with
   the next DCE if this was their only consumer.  */
void
rewrite_lo_only (const schoolbook_match &m)
{
  tree lo_lhs = gimple_assign_lhs (m.lo_out_stmt);
  gimple_stmt_iterator gsi = gsi_for_stmt (m.lo_out_stmt);
  gimple *ns = gimple_build_assign (lo_lhs, MULT_EXPR, m.X, m.Y);
  gsi_replace (&gsi, ns, true);

  if (dump_file)
    {
      fprintf (dump_file, "mul_widen128: rewrote schoolbook low half (X=");
      print_generic_expr (dump_file, m.X, TDF_SLIM);
      fprintf (dump_file, ", Y=");
      print_generic_expr (dump_file, m.Y, TDF_SLIM);
      fprintf (dump_file, ") -> MULT_EXPR\n");
    }
}

/* Replace the schoolbook stmts with WIDEN_MULT_EXPR-derived values.

   Strategy: insert `prod = X w* Y; shifted = prod >> 64` once, before
   the EARLIEST of the stmts we're going to rewrite.  Then:

     HH_stmt       -> (DI) shifted     ; the real hi-half
     mid_hi_stmt   -> 0                ; absorbed into HH
     ts_hi_stmt    -> 0                ; absorbed into HH
     carry32_stmt  -> 0                ; absorbed into HH
     lo_out_stmt   -> (DI) prod        ; the lo-half

   If lo_out_stmt is absent, this is a HI-only match and only the high-half
   contributors are rewritten.

   The four PLUS_EXPRs that consumed {HH, mid_hi, ts_hi, carry32} now see
   `acc + 0`, leaving the HI value carried by HH alone -- exactly the
   umulh result.  Most of those additions are folded away in GIMPLE, but
   not all: measured on mul-widen128-1.c, `_9 = 0; _10 = _9 + _32;`
   reaches the optimized dump.  It costs nothing, since expansion drops
   it and the emitted code is a bare mul/umulh pair, but do not read the
   dumps expecting the adds to be gone.  */
void
rewrite_match (const schoolbook_match &m)
{
  /* TI = unsigned 128-bit.  */
  tree ti_type = unsigned_intTI_type_node;
  gcc_assert (ti_type);

  /* Anchor: earliest in block order, within the low half's block, of the
     statements we rewrite.  The one target that may sit elsewhere is a
     carry conversion pass_sink_code moved (see find_lo_carry); it is
     rewritten to zero, so it never reads PROD and needs no dominance
     from the anchor.  The two that do read PROD - HH_stmt and
     lo_out_stmt - are required to be in this block, and X and Y
     dominate it since the quadrant products consume them.  */
  gimple *targets[5] = {
    m.HH_stmt, m.mid_hi_stmt, m.ts_hi_stmt, m.carry32_stmt, m.lo_out_stmt
  };
  basic_block bb = gimple_bb (m.lo_out_stmt ? m.lo_out_stmt : m.HH_stmt);
  gimple *anchor_stmt = NULL;
  for (gimple_stmt_iterator it = gsi_start_bb (bb); !gsi_end_p (it);
       gsi_next (&it))
    {
      gimple *s = gsi_stmt (it);
      for (int i = 0; i < 5; ++i)
	if (targets[i] && s == targets[i])
	  {
	    anchor_stmt = s;
	    break;
	  }
      if (anchor_stmt)
	break;
    }
  /* Every match has at least its low-half output or its HH statement in
     BB, and that is where BB came from, so one of the targets is always
     found.  */
  gcc_assert (anchor_stmt);

  gimple_stmt_iterator anchor_gsi = gsi_for_stmt (anchor_stmt);

  /* prod = X w* Y  */
  tree prod_ssa = make_ssa_name (ti_type);
  gimple *prod_stmt = gimple_build_assign (prod_ssa, WIDEN_MULT_EXPR,
					   m.X, m.Y);
  gsi_insert_before (&anchor_gsi, prod_stmt, GSI_SAME_STMT);

  /* shifted = prod >> 64  */
  tree shifted_ssa = make_ssa_name (ti_type);
  tree shift_count = build_int_cst (integer_type_node, 64);
  gimple *shift_stmt = gimple_build_assign (shifted_ssa, RSHIFT_EXPR,
					    prod_ssa, shift_count);
  gsi_insert_before (&anchor_gsi, shift_stmt, GSI_SAME_STMT);

  /* HH_stmt:  HH_lhs = (DI) shifted.  HH now holds the whole high half
     rather than the single quadrant product the source wrote, so binds
     describing it would report a value that program never computed.  */
  {
    tree hh_lhs = gimple_assign_lhs (m.HH_stmt);
    reset_debug_uses (m.HH_stmt);
    gimple_stmt_iterator gsi = gsi_for_stmt (m.HH_stmt);
    gimple *ns = gimple_build_assign (hh_lhs, NOP_EXPR, shifted_ssa);
    gsi_replace (&gsi, ns, true);
  }

  /* lo_out_stmt:  lo_lhs = (DI) prod  */
  if (m.lo_out_stmt)
    {
      tree lo_lhs = gimple_assign_lhs (m.lo_out_stmt);
      gimple_stmt_iterator gsi = gsi_for_stmt (m.lo_out_stmt);
      gimple *ns = gimple_build_assign (lo_lhs, NOP_EXPR, prod_ssa);
      gsi_replace (&gsi, ns, true);
    }

  /* Zero out the three absorbed contributions.  Their values are now
     carried by HH, so anything still describing them for the debugger
     would describe a partial sum that no longer exists; reset those binds
     rather than let them report the zero.  */
  gimple *zeros[3] = { m.mid_hi_stmt, m.ts_hi_stmt, m.carry32_stmt };
  for (int i = 0; i < 3; ++i)
    {
      tree z_lhs = gimple_assign_lhs (zeros[i]);
      reset_debug_uses (zeros[i]);
      gimple_stmt_iterator gsi = gsi_for_stmt (zeros[i]);
      gimple *ns = gimple_build_assign (z_lhs, build_zero_cst (TREE_TYPE (z_lhs)));
      gsi_replace (&gsi, ns, true);
    }

  if (dump_file)
    {
      fprintf (dump_file, "mul_widen128: rewrote schoolbook (X=");
      print_generic_expr (dump_file, m.X, TDF_SLIM);
      fprintf (dump_file, ", Y=");
      print_generic_expr (dump_file, m.Y, TDF_SLIM);
      fprintf (dump_file, ") -> WIDEN_MULT_EXPR\n");
      if (!m.lo_out_stmt)
	fprintf (dump_file, "mul_widen128: HI-only match\n");
    }
}

/* Every rewrite mutates statements in place, and gsi_replace unlinks the
   statement it replaces from its block; looking that statement up a second
   time would search a block it no longer belongs to.  Two matches must
   therefore never rewrite the same statement.  Matching on the middle sum
   alone is not enough to guarantee that: two matches with distinct middle
   sums can still share a quadrant product, and a low-half-only match can
   land on a statement a full match already owns.  So claim the statements
   themselves, and drop any match that would overlap one already claimed.  */
bool
claim_stmts (hash_set<gimple *> &claimed, const schoolbook_match &m)
{
  gimple *targets[5] = { m.HH_stmt, m.mid_hi_stmt, m.ts_hi_stmt,
			 m.carry32_stmt, m.lo_out_stmt };
  for (int i = 0; i < 5; ++i)
    if (targets[i] && claimed.contains (targets[i]))
      return false;
  for (int i = 0; i < 5; ++i)
    if (targets[i])
      claimed.add (targets[i]);
  return true;
}

/* Process FUN in four passes over the blocks, strongest shape first: the
   two full schoolbooks, then high-half-only, then low-half-only.  A later
   pass never takes a statement an earlier one claimed.  Matches are
   collected before anything is rewritten, so no scan iterates a block
   while it is being changed.  */
unsigned int
process_function (function *fun)
{
  if (!fun)
    return 0;
  vec<schoolbook_match> matches = vNULL;
  hash_set<gimple *> claimed;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (assign_code (stmt) != BIT_IOR_EXPR)
	    continue;
	  schoolbook_match m;
	  if (try_match (stmt, &m) && claim_stmts (claimed, m))
	    matches.safe_push (m);
	}
    }
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (assign_code (stmt) != PLUS_EXPR)
	    continue;
	  schoolbook_match m;
	  if (try_match_add_lo (stmt, &m) && claim_stmts (claimed, m))
	    matches.safe_push (m);
	}
    }
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (assign_code (stmt) != PLUS_EXPR)
	    continue;
	  schoolbook_match m;
	  if (try_match_hi_only (stmt, &m) && claim_stmts (claimed, m))
	    matches.safe_push (m);
	}
    }
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (assign_code (stmt) != PLUS_EXPR)
	    continue;
	  schoolbook_match m;
	  if (try_match_lo_only (stmt, &m) && claim_stmts (claimed, m))
	    matches.safe_push (m);
	}
    }
  bool changed = !matches.is_empty ();
  for (unsigned i = 0; i < matches.length (); ++i)
    if (matches[i].HH_stmt)
      rewrite_match (matches[i]);
    else
      rewrite_lo_only (matches[i]);
  if (dump_file && changed)
    fprintf (dump_file, "mul_widen128: rewrote %u schoolbook(s) in %s\n",
	     matches.length (), function_name (fun));
  matches.release ();
  return 0;
}

const pass_data pass_data_aarch64_mul_widen128 =
{
  GIMPLE_PASS,
  "mul_widen128",                /* name */
  OPTGROUP_NONE,                 /* optinfo_flags */
  TV_NONE,                       /* tv_id */
  PROP_ssa | PROP_cfg,           /* properties_required */
  0,                             /* properties_provided */
  0,                             /* properties_destroyed */
  0,                             /* todo_flags_start */
  0,                             /* todo_flags_finish */
};

class pass_aarch64_mul_widen128 : public gimple_opt_pass
{
public:
  pass_aarch64_mul_widen128 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_aarch64_mul_widen128, ctxt)
  {}

  opt_pass *clone () final override
  {
    return new pass_aarch64_mul_widen128 (m_ctxt);
  }

  bool gate (function *) final override
  {
    return optimize > 0 && flag_aarch64_mul_widen128;
  }

  unsigned int execute (function *fun) final override
  {
    return process_function (fun);
  }
};

} // anon namespace

gimple_opt_pass *
make_pass_aarch64_mul_widen128 (gcc::context *ctxt)
{
  return new pass_aarch64_mul_widen128 (ctxt);
}
