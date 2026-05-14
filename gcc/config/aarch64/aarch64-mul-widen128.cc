/* AArch64 gimple pass: rewrite the SEAL multiply_uint64_generic schoolbook
   into a single WIDEN_MULT_EXPR<TImode>(X, Y).

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

/* This pass recognizes the canonical "manual 64x64 -> 128 schoolbook"
   pattern emitted by SEAL's multiply_uint64_generic (used when both
   __int128 and the _umul128 intrinsic are unavailable, the path SEAL
   takes on aarch64 by default), and rewrites it to:

     prod = X w* Y                           ; WIDEN_MULT_EXPR<TI>(DI, DI)
     hi   = (uint64_t) (prod >> 64)
     lo   = (uint64_t) prod

   The TImode product is then lowered by the existing aarch64 backend
   to `mul + umulh`, producing the same code as if the source had used
   __uint128_t directly.

   The pass is inserted after FORWPROP5 (slot 153), where the schoolbook
   IR is at its most regular shape: post-DOM2 (carry branch is collapsed
   to a 1-bit comparison), post-REASSOC1 (additions in canonical order),
   pre-REASSOC2 / pre-UADDC-recognition (no .ADD_OVERFLOW intrinsics yet,
   no __complex__ types).

   See aarch64-mul-widen128.cc inline comments for the matching strategy.
   Disable with -mno-mul-widen128.  */

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
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "fold-const.h"
#include "stor-layout.h"

namespace {

/* Defining stmt of an SSA name, or NULL if T is not an SSA name with a real
   def.  */
static gimple *
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
static enum tree_code
assign_code (gimple *stmt)
{
  if (!stmt || !is_gimple_assign (stmt))
    return ERROR_MARK;
  return gimple_assign_rhs_code (stmt);
}

/* True if T is an unsigned 64-bit integer value.  The schoolbook pattern we
   rewrite is specifically uint64_t x uint64_t -> uint128_t; accepting signed
   or differently-sized values risks changing multiplication semantics.  */
static bool
is_unsigned_64 (tree t)
{
  if (!t)
    return false;
  tree type = TREE_TYPE (t);
  return INTEGRAL_TYPE_P (type)
         && TYPE_UNSIGNED (type)
         && TYPE_PRECISION (type) == 64;
}

/* True if STMT is `R = X & MASK` with MASK == VAL; sets *SRC.  */
static bool
is_and_const (gimple *stmt, unsigned HOST_WIDE_INT val, tree *src)
{
  if (assign_code (stmt) != BIT_AND_EXPR)
    return false;
  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (rhs2) != INTEGER_CST
      || !tree_fits_uhwi_p (rhs2)
      || tree_to_uhwi (rhs2) != val)
    return false;
  *src = gimple_assign_rhs1 (stmt);
  return true;
}

/* True if STMT is `R = X >> AMT`; sets *SRC.  */
static bool
is_rshift_const (gimple *stmt, unsigned HOST_WIDE_INT amt, tree *src)
{
  if (assign_code (stmt) != RSHIFT_EXPR)
    return false;
  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (rhs2) != INTEGER_CST
      || !tree_fits_uhwi_p (rhs2)
      || tree_to_uhwi (rhs2) != amt)
    return false;
  *src = gimple_assign_rhs1 (stmt);
  return true;
}

/* True if STMT is `R = X << AMT`; sets *SRC.  */
static bool
is_lshift_const (gimple *stmt, unsigned HOST_WIDE_INT amt, tree *src)
{
  if (assign_code (stmt) != LSHIFT_EXPR)
    return false;
  tree rhs2 = gimple_assign_rhs2 (stmt);
  if (TREE_CODE (rhs2) != INTEGER_CST
      || !tree_fits_uhwi_p (rhs2)
      || tree_to_uhwi (rhs2) != amt)
    return false;
  *src = gimple_assign_rhs1 (stmt);
  return true;
}

/* True if STMT is `R = X * Y`; sets *X_OUT, *Y_OUT.  */
static bool
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
static bool
get_half (tree op, bool *is_low_out, tree *root_out)
{
  gimple *d = def_of (op);
  if (!d)
    return false;
  tree src;
  if (is_and_const (d, 0xFFFFFFFFULL, &src))
    {
      *is_low_out = true;
      *root_out = src;
      return true;
    }
  if (is_rshift_const (d, 32, &src))
    {
      *is_low_out = false;
      *root_out = src;
      return true;
    }
  return false;
}

/* SSA equality (handles SSA name canonicalization).  */
static bool
ssa_eq (tree a, tree b)
{
  return a == b || operand_equal_p (a, b, 0);
}

/* Bundle of stmts identified by a successful schoolbook match.  */
struct schoolbook_match
{
  tree X, Y;                    /* DImode source operands.  */
  gimple *LL_stmt;              /* X_lo * Y_lo  (== `right`)        */
  gimple *LH_stmt;              /* X_lo * Y_hi                       */
  gimple *HL_stmt;              /* X_hi * Y_lo  (== `middle1`)       */
  gimple *HH_stmt;              /* X_hi * Y_hi                       */
  gimple *mid_stmt;             /* mid = LH + HL                     */
  gimple *temp_sum_stmt;        /* (LL>>32) + (mid&0xFFFFFFFF)       */
  gimple *lo_out_stmt;          /* (temp_sum<<32) | (LL&0xFFFFFFFF)  */
  gimple *mid_hi_stmt;          /* mid >> 32                         */
  gimple *ts_hi_stmt;           /* temp_sum >> 32                    */
  gimple *carry32_stmt;         /* ((DI) (HL>mid)) << 32             */
};

/* Look up the SSA name for the lhs of a stmt.  */
static inline tree
lhs_of (gimple *s)
{
  return gimple_assign_lhs (s);
}

/* Find the DImode multiply stmt in BB that computes ROOT_X_HALF * ROOT_Y_HALF
   (modulo commute), where each operand is the (is_low? AND : RSHIFT) form
   over its respective root.  */
static gimple *
find_quadrant_mul (basic_block bb, tree X, tree Y, bool x_is_low, bool y_is_low,
                   gimple *exclude1, gimple *exclude2 = NULL,
                   gimple *exclude3 = NULL)
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
static gimple *
plus_chain_root (gimple *s)
{
  tree lhs = lhs_of (s);
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
      lhs = lhs_of (root);
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
static bool
same_plus_chain (gimple *hh_stmt, gimple *mid_hi_stmt, gimple *ts_hi_stmt,
                 gimple *carry32_stmt)
{
  gimple *root = plus_chain_root (hh_stmt);
  return root
         && plus_chain_root (mid_hi_stmt) == root
         && plus_chain_root (ts_hi_stmt) == root
         && plus_chain_root (carry32_stmt) == root;
}

/* Find a `SOURCE >> 32` immediate user.  */
static gimple *
find_rshift32_user (gimple *source)
{
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs_of (source))
    {
      gimple *u = USE_STMT (use_p);
      tree dummy;
      if (is_rshift_const (u, 32, &dummy))
        return u;
    }
  return NULL;
}

/* Find `temp_sum = (LL >> 32) + (mid & 0xffffffff)` in BB.  */
static gimple *
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
          if (A_def && is_rshift_const (A_def, 32, &A_src)
              && B_def && is_and_const (B_def, 0xFFFFFFFFULL, &B_src)
              && ssa_eq (A_src, lhs_of (LL_stmt))
              && ssa_eq (B_src, lhs_of (mid_stmt)))
            return s;
        }
    }
  return NULL;
}

/* Find carry32 = ((uint64_t) (M > mid or mid < M)) << 32, where M is either
   of the two cross products.  */
static gimple *
find_carry32 (basic_block bb, gimple *LH_stmt, gimple *HL_stmt,
              gimple *mid_stmt)
{
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *s = gsi_stmt (gsi);
      tree pre;
      if (!is_lshift_const (s, 32, &pre))
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
      tree m_hl = lhs_of (HL_stmt);
      tree m_lh = lhs_of (LH_stmt);
      tree m_mid = lhs_of (mid_stmt);
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
static bool
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

/* Try to identify the schoolbook anchored at IOR_STMT.  */
static bool
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
  if (u_def && v_def && is_lshift_const (u_def, 32, &ts_pre_lshift)
      && is_and_const (v_def, 0xFFFFFFFFULL, &ll_pre_and))
    ;
  else if (u_def && v_def && is_and_const (u_def, 0xFFFFFFFFULL, &ll_pre_and)
           && is_lshift_const (v_def, 32, &ts_pre_lshift))
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
      if (A_def && is_rshift_const (A_def, 32, &A_src)
          && B_def && is_and_const (B_def, 0xFFFFFFFFULL, &B_src))
        {
          ll_via_rshift = A_src;
          mid_via_and = B_src;
          break;
        }
    }
  if (!ll_via_rshift || !mid_via_and)
    return false;
  if (!ssa_eq (ll_via_rshift, lhs_of (LL_stmt)))
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

  /* Find LH (X_lo * Y_hi) and HL (X_hi * Y_lo) among ma/mb.  */
  auto verify_cross = [&](gimple *cand_stmt ATTRIBUTE_UNUSED,
                          tree cx, tree cy,
                          bool need_x_low, bool need_y_low) -> bool {
    bool cx_lo, cy_lo;
    tree cx_root, cy_root;
    if (!get_half (cx, &cx_lo, &cx_root)) return false;
    if (!get_half (cy, &cy_lo, &cy_root)) return false;
    if (ssa_eq (cx_root, X_root) && cx_lo == need_x_low
        && ssa_eq (cy_root, Y_root) && cy_lo == need_y_low)
      return true;
    if (ssa_eq (cx_root, Y_root) && cx_lo == need_y_low
        && ssa_eq (cy_root, X_root) && cy_lo == need_x_low)
      return true;
    return false;
  };

  gimple *LH_stmt = NULL, *HL_stmt = NULL;
  /* (ma is LH, mb is HL)?  */
  if (verify_cross (ma_def, ma_x, ma_y, /*x_low=*/true, /*y_low=*/false)
      && verify_cross (mb_def, mb_x, mb_y, /*x_low=*/false, /*y_low=*/true))
    { LH_stmt = ma_def; HL_stmt = mb_def; }
  /* Or (ma is HL, mb is LH).  */
  else if (verify_cross (ma_def, ma_x, ma_y, /*x_low=*/false, /*y_low=*/true)
           && verify_cross (mb_def, mb_x, mb_y, /*x_low=*/true, /*y_low=*/false))
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
  gimple *mid_hi_stmt = NULL;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs_of (mid_stmt))
    {
      gimple *u = USE_STMT (use_p);
      tree dummy;
      if (is_rshift_const (u, 32, &dummy))
        { mid_hi_stmt = u; break; }
    }
  if (!mid_hi_stmt)
    return false;

  /* Find ts_hi = temp_sum >> 32.  */
  gimple *ts_hi_stmt = NULL;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs_of (temp_sum_stmt))
    {
      gimple *u = USE_STMT (use_p);
      tree dummy;
      if (is_rshift_const (u, 32, &dummy))
        { ts_hi_stmt = u; break; }
    }
  if (!ts_hi_stmt)
    return false;

  /* Find carry32 = ((uint64_t) (HL>mid or mid<HL)) << 32.
     Approach: scan BB for a LSHIFT_EXPR by 32 whose source is a 1-bit
     compare result widened to DImode.  */
  gimple *carry32_stmt = NULL;
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *s = gsi_stmt (gsi);
      tree pre;
      if (!is_lshift_const (s, 32, &pre))
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
      /* For GT: (M > mid).  For LT: (mid < M).  M is whichever of HL/LH
         the source code computed first textually as `middle1` -- this
         depends on call-site argument orientation, so accept either.  */
      tree m_hl = lhs_of (HL_stmt);
      tree m_lh = lhs_of (LH_stmt);
      tree m_mid = lhs_of (mid_stmt);
      bool ok = false;
      if (cc == GT_EXPR)
        ok = (ssa_eq (ca, m_hl) && ssa_eq (cb, m_mid))
             || (ssa_eq (ca, m_lh) && ssa_eq (cb, m_mid));
      else /* LT_EXPR */
        ok = (ssa_eq (ca, m_mid) && ssa_eq (cb, m_hl))
             || (ssa_eq (ca, m_mid) && ssa_eq (cb, m_lh));
      if (ok)
        {
          carry32_stmt = s;
          break;
        }
    }
  if (!carry32_stmt)
    return false;

  /* The four HI components must contribute to the same additive result.  */
  if (!same_plus_chain (HH_stmt, mid_hi_stmt, ts_hi_stmt, carry32_stmt))
    return false;

  /* All checks passed -- populate the match record.  */
  m->X = X_root;
  m->Y = Y_root;
  m->LL_stmt = LL_stmt;
  m->LH_stmt = LH_stmt;
  m->HL_stmt = HL_stmt;
  m->HH_stmt = HH_stmt;
  m->mid_stmt = mid_stmt;
  m->temp_sum_stmt = temp_sum_stmt;
  m->lo_out_stmt = ior_stmt;
  m->mid_hi_stmt = mid_hi_stmt;
  m->ts_hi_stmt = ts_hi_stmt;
  m->carry32_stmt = carry32_stmt;
  return true;
}

/* Try to identify a schoolbook whose low half has been DCE'd or was never
   materialized, anchored at `mid = LH + HL`.  This catches accumulator-style
   callers that only consume the high half.  */
static bool
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
                                       /*x_low=*/true, /*y_low=*/true,
                                       LH_stmt, HL_stmt);
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

  gimple *mid_hi_stmt = find_rshift32_user (mid_stmt);
  if (!mid_hi_stmt)
    return false;

  gimple *ts_hi_stmt = find_rshift32_user (temp_sum_stmt);
  if (!ts_hi_stmt)
    return false;

  gimple *carry32_stmt = find_carry32 (bb, LH_stmt, HL_stmt, mid_stmt);
  if (!carry32_stmt)
    return false;

  if (!same_plus_chain (HH_stmt, mid_hi_stmt, ts_hi_stmt, carry32_stmt))
    return false;

  m->X = X_root;
  m->Y = Y_root;
  m->LL_stmt = LL_stmt;
  m->LH_stmt = LH_stmt;
  m->HL_stmt = HL_stmt;
  m->HH_stmt = HH_stmt;
  m->mid_stmt = mid_stmt;
  m->temp_sum_stmt = temp_sum_stmt;
  m->lo_out_stmt = NULL;
  m->mid_hi_stmt = mid_hi_stmt;
  m->ts_hi_stmt = ts_hi_stmt;
  m->carry32_stmt = carry32_stmt;
  return true;
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
   `acc + 0`, which forwprop+DCE fold to `acc`, leaving the HI value
   carried by HH alone -- exactly the umulh result.  */
static void
rewrite_match (const schoolbook_match &m)
{
  /* TI = unsigned 128-bit.  */
  tree ti_type = build_nonstandard_integer_type (128, 1);

  /* Anchor: earliest in BB order of the stmts we'll rewrite.  All
     should be in the same BB (try_match's scans are BB-local for HH
     and carry32; mid_hi/ts_hi/HH/lo_out are all reachable from there).  */
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
  if (!anchor_stmt)
    return;     /* shouldn't happen if try_match succeeded */

  gimple_stmt_iterator anchor_gsi = gsi_for_stmt (anchor_stmt);

  /* prod = X w* Y  */
  tree prod_ssa = make_ssa_name (ti_type);
  gimple *prod_stmt = gimple_build_assign (prod_ssa, WIDEN_MULT_EXPR,
                                           m.X, m.Y);
  gsi_insert_before (&anchor_gsi, prod_stmt, GSI_SAME_STMT);

  /* shifted = prod >> 64  */
  tree shifted_ssa = make_ssa_name (ti_type);
  gimple *shift_stmt = gimple_build_assign (shifted_ssa, RSHIFT_EXPR,
                                            prod_ssa,
                                            build_int_cst (integer_type_node, 64));
  gsi_insert_before (&anchor_gsi, shift_stmt, GSI_SAME_STMT);

  /* HH_stmt:  HH_lhs = (DI) shifted  */
  {
    tree hh_lhs = lhs_of (m.HH_stmt);
    gimple_stmt_iterator gsi = gsi_for_stmt (m.HH_stmt);
    gimple *ns = gimple_build_assign (hh_lhs, NOP_EXPR, shifted_ssa);
    gsi_replace (&gsi, ns, true);
  }

  /* lo_out_stmt:  lo_lhs = (DI) prod  */
  if (m.lo_out_stmt)
  {
    tree lo_lhs = lhs_of (m.lo_out_stmt);
    gimple_stmt_iterator gsi = gsi_for_stmt (m.lo_out_stmt);
    gimple *ns = gimple_build_assign (lo_lhs, NOP_EXPR, prod_ssa);
    gsi_replace (&gsi, ns, true);
  }

  /* Zero out the three absorbed contributions.  */
  gimple *zeros[3] = { m.mid_hi_stmt, m.ts_hi_stmt, m.carry32_stmt };
  for (int i = 0; i < 3; ++i)
    {
      tree z_lhs = lhs_of (zeros[i]);
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

static bool
same_schoolbook_mid (const auto_vec<schoolbook_match> &matches, gimple *mid_stmt)
{
  for (unsigned i = 0; i < matches.length (); ++i)
    if (matches[i].mid_stmt == mid_stmt)
      return true;
  return false;
}

/* Process FUN: scan every BB for full low+high matches first, then for HI-only
   matches.  Collect matches first, rewrite second to avoid mutating while
   iterating BB stmts.  */
static unsigned int
process_function (function *fun)
{
  if (!fun)
    return 0;
  auto_vec<schoolbook_match> matches;
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
          if (try_match (stmt, &m))
            matches.safe_push (m);
        }
    }
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
           gsi_next (&gsi))
        {
          gimple *stmt = gsi_stmt (gsi);
          if (assign_code (stmt) != PLUS_EXPR
              || same_schoolbook_mid (matches, stmt))
            continue;
          schoolbook_match m;
          if (try_match_hi_only (stmt, &m)
              && !same_schoolbook_mid (matches, m.mid_stmt))
            matches.safe_push (m);
        }
    }
  for (unsigned i = 0; i < matches.length (); ++i)
    rewrite_match (matches[i]);
  if (dump_file && !matches.is_empty ())
    fprintf (dump_file, "mul_widen128: rewrote %u schoolbook(s) in %s\n",
             matches.length (), function_name (fun));
  return matches.is_empty () ? 0 : (TODO_update_ssa | TODO_cleanup_cfg);
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
