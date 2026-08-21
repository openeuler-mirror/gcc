/* Helpers for RTL pattern matchers.
   Copyright (C) 2003-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_RTL_MATCHER_H
#define GCC_RTL_MATCHER_H

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "df.h"

/* Get definitions chain for the reg being used in insn.  */
static df_link *
get_def_chain (rtx_insn *insn, rtx reg)
{
  df_ref use;
  FOR_EACH_INSN_USE (use, insn)
    {
      rtx use_reg = DF_REF_REG (use);
      if (GET_CODE (use_reg) == SUBREG)
	{
	  if (REGNO (SUBREG_REG (use_reg)) == REGNO (reg))
	    return NULL;
	}
      else
	{
	  gcc_assert (REG_P (use_reg));
	  if (REGNO (use_reg) == REGNO (reg))
	    return DF_REF_CHAIN (use);
	}
    }

  return NULL;
}

/* Check if the reg is not global and actually modified in the ref.  */
static bool
check_def_chain_ref (df_ref ref, rtx reg)
{
  if (!ref || !DF_REF_INSN_INFO (ref))
    return false;

  return !(REGNO (reg) < FIRST_PSEUDO_REGISTER
	   && global_regs[REGNO (reg)])
    || reg_set_p (reg, DF_REF_INSN (ref));
}

/* Get the single def instruction of the reg being used in the insn.  */
static rtx_insn *
get_single_def_insn (rtx_insn *insn, rtx reg)
{
  if (!REG_P (reg))
    return NULL;

  df_link *ref_chain = get_def_chain (insn, reg);
  gcc_assert (ref_chain);

  if (!ref_chain || ref_chain->next
      || !check_def_chain_ref (ref_chain->ref, reg))
    return NULL;

  return DF_REF_INSN (ref_chain->ref);
}

/* Get the single user instruction of the reg being set in the insn.  */
static rtx_insn *
get_single_use_insn (rtx_insn *insn, rtx reg)
{
  df_ref def;
  struct df_link *ref_chain;

  if (!REG_P (reg))
    return NULL;

  FOR_EACH_INSN_DEF (def, insn)
    if (REGNO (DF_REF_REG (def)) == REGNO (reg))
      break;

  gcc_assert (def && "Broken def-use analysis chain.");

  ref_chain = DF_REF_CHAIN (def);

  if (!ref_chain || ref_chain->next || !ref_chain->ref)
    return NULL;

  return DF_REF_INSN (ref_chain->ref);
}

/* Get the rtx pattern of suitable opcode from single set instruction.  */
template <rtx_code OP1, rtx_code OP2>
static rtx
get_single_set_op (rtx_insn *insn)
{
  rtx pat = single_set (insn);
  if (!pat)
    return NULL_RTX;

  rtx src = SET_SRC (pat);
  if (GET_CODE (src) != OP1 && GET_CODE (src) != OP2)
    return NULL_RTX;

  return src;
}

/* Get the rtx pattern of suitable opcode from single set instruction.  */
template <rtx_code OP>
static rtx
get_single_set_op (rtx_insn *insn)
{
  return get_single_set_op<OP, OP> (insn);
}

/* Get the rtx constant from single set instruction of suitable opcode.  */
template<rtx_code OP>
static rtx
get_op_const_cst (rtx_insn *insn)
{
  rtx src = get_single_set_op<OP> (insn);
  if (!src)
    return NULL_RTX;

  rtx cst = XEXP (src, 1);
  return CONST_INT_P (cst) ? cst : NULL_RTX;
}

/* Get the rtx destination from single set instruction of suitable opcode.  */
template <rtx_code OP>
static rtx
get_single_set_dst (rtx_insn *insn)
{
  rtx pat = single_set (insn);
  if (!pat)
    return NULL_RTX;

  rtx dst = SET_DEST (pat);
  if (GET_CODE (dst) != OP)
    return NULL_RTX;

  return dst;
}

/* Get the rtx destination from single set instruction.  */
static rtx
get_single_set_dst (rtx_insn *insn)
{
  rtx pat = single_set (insn);
  if (!pat)
    return NULL_RTX;

  return SET_DEST (pat);
}

/* Check if the instruction is single set of suitable opcode.
   Also gather its source and destination patterns.  */
template <rtx_code OP>
static bool
check_simple_op (rtx_insn *insn, rtx &src, rtx &dst)
{
  rtx pat = single_set (insn);
  if (!pat)
    return false;

  src = SET_SRC (pat);
  dst = SET_DEST (pat);

  if (GET_CODE (src) != OP)
    return false;

  return true;
}

/* Minimal term info of the RTL matcher.  All of the custom matchers should
   inherit from it.

   It stores information about matched pattern, instruction
   of its location and predicate if the matched term represents operator
   inside the matched tree.  */
struct minimal_term_info
{
  minimal_term_info ()
    {}
  minimal_term_info (rtx_insn *loc, rtx src, bool is_op = false)
    : loc (loc), src (src), is_op (is_op)
    {}

  rtx_insn *loc;
  rtx src;
  bool is_op;
};

/* Term info for memory matcher.  */
struct mem_term_info : minimal_term_info
{
  mem_term_info ()
    {}
  mem_term_info (rtx_insn *loc, rtx src, unsigned HOST_WIDE_INT offset = 0)
    : minimal_term_info (loc, src), offset (offset)
    {}

  unsigned HOST_WIDE_INT offset;
};

/* A wrapper being used to turn a term into a matcher-like entity.  */
template<typename T = minimal_term_info>
struct matcher_term
{
  /* Required storage size information of the matcher.  */
  static const int holder_size = 1;
  static const int op_num = 0;
  typedef T term_type;
  typedef term_type holder_type[holder_size];
};

/* Simple matcher of patterns of suitable opcode.  */
template<rtx_code ARGOP, typename TERM = minimal_term_info>
struct arg_op_matcher : matcher_term<TERM>
{
  typedef typename matcher_term<TERM>::holder_type holder_type;

  static bool match (rtx_insn *, holder_type &)
    {
      return false;
    }

  static bool match (rtx src, rtx_insn *insn, holder_type &m)
    {
      if (GET_CODE (src) != ARGOP)
	return false;

      static_cast<minimal_term_info &> (m[0]) = minimal_term_info (insn, src);
      return true;
    }
};

/* Simple matcher of integer constants.  */
template<typename T>
struct int_cst_matcher : arg_op_matcher <CONST_INT, T>
{};

/* Unary operator matcher.  */
template<rtx_code OP1, typename ARG, bool store_op = false, rtx_code OP2 = OP1>
struct unop_matcher
{
  /* Required storage size information of the matcher.  */
  static const int holder_size = ARG::holder_size + store_op;
  static const int op_num = ARG::op_num + store_op;
  typedef typename ARG::term_type term_type;
  typedef term_type holder_type[holder_size];

  static bool match (rtx_insn *insn, holder_type &m)
    {
      rtx src = get_single_set_op<OP1, OP2> (insn);
      return src && match (src, insn, m);
    }

  static bool match (rtx src, rtx_insn *insn, holder_type &m)
    {
      if (REG_P (src))
	{
	  insn = get_single_def_insn (insn, src);
	  if (insn && (src = single_set (insn)))
	    src = SET_SRC (src);
	}

      if (!src || !insn || (GET_CODE (src) != OP1 && GET_CODE (src) != OP2))
	return false;

      /* Store current operation if needed.  */
      if (store_op)
	static_cast<minimal_term_info &> (m[0]) = minimal_term_info (insn, src,
								     true);

      rtx op = XEXP (src, 0);
      rtx_insn *def = get_single_def_insn (insn, op);
      typename ARG::holder_type &m_arg
	= (typename ARG::holder_type &) *(m + store_op);
      return (def && ARG::match (def, m_arg)) || ARG::match (op, insn, m_arg);
    }
};

/* Binary operator matcher.  */
template<rtx_code OP1, typename LHS, typename RHS, bool COMMUTATIVE = false,
	 bool store_op = false, rtx_code OP2 = OP1>
struct binop_matcher
{
  /* Required storage size information of the matcher.  */
  static const int holder_size = LHS::holder_size + RHS::holder_size + store_op;
  static const int op_num = LHS::op_num + RHS::op_num + store_op;
  typedef typename LHS::term_type term_type;
  typedef term_type holder_type[holder_size];

  static bool match (rtx_insn *insn, holder_type &m)
    {
      rtx src = get_single_set_op<OP1, OP2> (insn);
      return src && match (src, insn, m);
    }

  static bool match (rtx src, rtx_insn *insn, holder_type &m)
    {
      if (GET_CODE (src) != OP1 && GET_CODE (src) != OP2)
	return false;

      /* Store current operation if needed.  */
      if (store_op)
	static_cast<minimal_term_info &> (m[0]) = minimal_term_info (insn, src,
								     true);

      rtx lhs_op = XEXP (src, 0);
      rtx rhs_op = XEXP (src, 1);
      rtx_insn *lhs_def = get_single_def_insn (insn, lhs_op);
      rtx_insn *rhs_def = get_single_def_insn (insn, rhs_op);

      return match (lhs_def, rhs_def, lhs_op, rhs_op, insn, m)
	|| (COMMUTATIVE && match (rhs_def, lhs_def, rhs_op, lhs_op, insn, m));
    }

private:
  static bool match (rtx_insn *lhs_def, rtx_insn *rhs_def,
		     rtx lhs_op, rtx rhs_op, rtx_insn *insn,
		     holder_type &m)
    {
      /* Force template instantiation error on non-matching types.  */
      gcc_assert ((typename LHS::term_type *) NULL
		  == (typename RHS::term_type *) NULL);

      /* Obtain locations in the storage.  */
      typename LHS::holder_type &m_lhs
	= (typename LHS::holder_type &) *(m + store_op);
      typename RHS::holder_type &m_rhs
	= (typename RHS::holder_type &) *(m + store_op
					  + LHS::holder_size);

      /* Try match both instructions.  */
      if (lhs_def && rhs_def && LHS::match (lhs_def, m_lhs)
	  && RHS::match (rhs_def, m_rhs))
	return true;
      /* Try match instruction and pattern.  */
      else if (lhs_def && LHS::match (lhs_def, m_lhs)
	       && RHS::match (rhs_op, insn, m_rhs))
	return true;
      /* Try match pattern and instruction.  */
      else if (rhs_def && LHS::match (lhs_op, insn, m_lhs)
	       && RHS::match (rhs_def, m_rhs))
	return true;
      /* Try match both patterns.  */
      else
	return LHS::match (lhs_op, insn, m_lhs)
	  && RHS::match (rhs_op, insn, m_rhs);
    }
};

#endif // GCC_RTL_MATCHER_H
