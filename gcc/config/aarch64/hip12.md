;; hip12 pipeline description
;; Copyright (C) 2023 Free Software Foundation, Inc.
;;
;;Contributed by liyunfei
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_automaton "hip12")
(define_automaton "hip12_ldst")
(define_automaton "hip12_v")

; The hip12 core is modelled as issues pipeline that has
; the following functional units.
; 1. 4 pipelines for single cycle integer micro operations: ALU0, ALU1, ALU3, ALU4

(define_cpu_unit "hip12_alu0" "hip12")
(define_cpu_unit "hip12_alu1" "hip12")
(define_cpu_unit "hip12_alu3" "hip12")
(define_cpu_unit "hip12_alu4" "hip12")

 (define_reservation "hip12_alu0134" "hip12_alu0|hip12_alu1|hip12_alu3|hip12_alu4")
(define_reservation "hip12_alu14" "hip12_alu1|hip12_alu4")

; 2. 2 pipelines for multi cycles integer micro operations: ALU2, ALU5

(define_cpu_unit "hip12_alu2" "hip12")
(define_cpu_unit "hip12_alu5" "hip12")

(define_reservation "hip12_alu25" "hip12_alu2|hip12_alu5")
(define_reservation "hip12_alu1425" "hip12_alu1|hip12_alu4|hip12_alu2|hip12_alu5")

; 3. All ALU pipelines

(define_reservation "hip12_alu" "hip12_alu0|hip12_alu1|hip12_alu2|hip12_alu3|hip12_alu4|hip12_alu5")

; 4. 3 pipelines for load micro opetations: Load0, Load1, Load2

(define_cpu_unit "hip12_load0" "hip12_ldst")
(define_cpu_unit "hip12_load1" "hip12_ldst")
(define_cpu_unit "hip12_load2" "hip12_ldst")

(define_reservation "hip12_ld" "hip12_load0|hip12_load1|hip12_load2")

; 5. 2 pipelines for store micro operations: Store1, Store2

(define_cpu_unit "hip12_store0" "hip12_ldst")
(define_cpu_unit "hip12_store1" "hip12_ldst")

(define_reservation "hip12_st" "hip12_store0|hip12_store1")

; 6. 2 pipelines for store data micro operations: STD0, STD1

(define_cpu_unit "hip12_store_data0" "hip12_ldst")
(define_cpu_unit "hip12_store_data1" "hip12_ldst")

(define_reservation "hip12_std" "hip12_store_data0|hip12_store_data1")

; 7. 4 asymmetric pipelines for Asimd/FP/SVE micro operations: V0, V1, V2, V3

(define_cpu_unit "hip12_v0" "hip12_v")
(define_cpu_unit "hip12_v1" "hip12_v")
(define_cpu_unit "hip12_v2" "hip12_v")
(define_cpu_unit "hip12_v3" "hip12_v")

(define_reservation "hip12_v0123" "hip12_v0|hip12_v1|hip12_v2|hip12_v3")
(define_reservation "hip12_v02" "hip12_v0|hip12_v2")

; 8. 2 pipelines for branch operations: Branch0, Branch1

(define_cpu_unit "hip12_b0" "hip12")
(define_cpu_unit "hip12_b1" "hip12")

(define_reservation "hip12_b" "hip12_b0|hip12_b1")

;; Block all issue queues.

(define_reservation "hip12_block" "
          hip12_alu0+hip12_alu1+hip12_alu2+hip12_alu3
          +hip12_alu4+hip12_alu5+hip12_load0+hip12_load1+hip12_load2+hip12_store0+hip12_store1+hip12_store_data0+hip12_store_data1+hip12_v0+hip12_v1+hip12_v2+hip12_v3")

;; Branch execution Unit

(define_insn_reservation "hip12_branch" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "branch"))
  "hip12_b")

(define_insn_reservation "hip12_branch_and_link" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "call"))
  "hip12_b+hip12_alu14")

;; Integer arithmetic/logic instructions.

(define_insn_reservation "hip12_alu_basic" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "alu_imm,alu_sreg,\
			adc_reg,adc_imm"))
  "hip12_alu")

(define_insn_reservation "hip12_alu_basic_flagset" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "alus_imm,alus_sreg,\
			adcs_reg,adcs_imm"))
  "hip12_alu1425")

(define_insn_reservation "hip12_alu_basic_extend" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "alu_ext,alus_ext,\
            alu_shift_imm_lsl_1to4,alu_shift_imm_other,\
            alu_shift_reg,alus_shift_imm,alus_shift_reg"))
  "hip12_alu25")

(define_insn_reservation "hip12_alu_logical" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "logic_reg,logic_imm"))
  "hip12_alu")

(define_insn_reservation "hip12_alu_logical_imm" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "logic_imm"))
  "hip12_alu14")

(define_insn_reservation "hip12_alu_logical_flagset" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "logics_reg"))
  "hip12_alu1425")

(define_insn_reservation "hip12_alu_logical_flagset_imm" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "logics_imm"))
  "hip12_alu25")

(define_insn_reservation "hip12_alu_conditional" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "csel"))
  "hip12_alu14")

;; Divide and Multiply instructions.

(define_insn_reservation "hip12_divide" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "sdiv,udiv"))
  "hip12_alu25")

(define_insn_reservation "hip12_multiply" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "mul,muls"))
  "hip12_alu25")

(define_insn_reservation "hip12_multiply_long" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "smull,umull,smulls,umulls"))
  "hip12_alu25")

(define_insn_reservation "hip12_multiply_accumulate" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "mla,mlas"))
  "hip12_alu25+hip12_alu0134")

(define_insn_reservation "hip12_multiply_accumulate_long" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "smlal,umlal"))
  "hip12_alu25+hip12_alu0134")

;; no Pointer Authentication instructions in backend types.

;; Miscellaneous Data-Processing instructions.

(define_insn_reservation "hip12_address" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "adr"))
  "hip12_alu14")

(define_insn_reservation "hip12_bitfield" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "bfm,bfx"))
  "hip12_alu14")

;; Todo: Does hip12 have reg move or mvn instructions?
(define_insn_reservation "hip12_move" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "mov_imm,mov_shift_reg"))
  "hip12_alu")

(define_insn_reservation "hip12_count_leading" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "clz"))
  "hip12_alu14")

(define_insn_reservation "hip12_reverse_bits_bytes" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "rbit,rev"))
  "hip12_alu14")

; Todo: Does hip12 have imm shift instructions?
(define_insn_reservation "hip12_variable_shift" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "shift_reg"))
  "hip12_alu14")

; Block all issue pipes for a cycle
(define_insn_reservation "hip12_block" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "block"))
  "hip12_block")

;; Load and Store instructions.

(define_insn_reservation "hip12_load_register" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "load_4,load_8"))
  "hip12_ld")

(define_insn_reservation "hip12_load_pair" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "load_16"))
  "hip12_ld")

(define_insn_reservation "hip12_store" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "store_4,store_8"))
  "hip12_st+hip12_std")

(define_insn_reservation "hip12_store_pair" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "store_16"))
  "hip12_st+hip12_std")

;; FP Data Processing instructions.
; abs/neg/cpy
(define_insn_reservation "hip12_fp_arith" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "ffariths,ffarithd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_compare" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fcmpd,fcmps"))
  "hip12_v02+hip12_alu0134")

(define_insn_reservation "hip12_fp_conditional_compare" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fccmpd,fccmps"))
  "hip12_alu14,hip12_v0123")

(define_insn_reservation "hip12_fp_conditional_select" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fcsel"))
  "hip12_alu14,hip12_v0123")

(define_insn_reservation "hip12_fp_divide_single" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fdivs"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_divide_double" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fdivd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_square_single" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fsqrts"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_square_double" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fsqrtd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_fused_multiply_add" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "ffmad,ffmas"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_max_min" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_minmaxd,f_minmaxs"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_add" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fadds,faddd"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_multiply" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fmuld,fmuls"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_round_int" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_rintd,f_rints"))
  "hip12_v0123")

;; FP Miscellaneous instructions.

(define_insn_reservation "hip12_fp_covert_i2f" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_cvti2f"))
  "hip12_alu14,hip12_v0123")

(define_insn_reservation "hip12_fp_covert_f2i" 5
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_cvtf2i"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_covert_f2f" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_cvt"))
  "hip12_v0123")
  
(define_insn_reservation "hip12_fp_move" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "fmov"))
  "hip12_v0123")

(define_insn_reservation "hip12_fp_transfer_arm2vfp" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_mcr"))
  "hip12_alu14")

; transfer low half + high half
(define_insn_reservation "hip12_fp_transfer_2arm2vfp" 10
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_mcrr"))
  "hip12_alu14,nothing*3,hip12_alu14,hip12_v0123")

(define_insn_reservation "hip12_fp_transfer_vfp2arm" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_mrc,f_mrrc"))
  "hip12_v0123")

;; FP Load instructions.
; only basic double/single load
(define_insn_reservation "hip12_fp_load" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_loadd,f_loads"))
  "hip12_ld")

(define_insn_reservation "hip12_fp_load_vector_pair" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_ldr,neon_ldp,neon_ldp_q"))
  "hip12_alu+hip12_ld")

;; FP Store instructions.
; only basic double/single store
(define_insn_reservation "hip12_fp_store" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "f_stored,f_stores"))
  "hip12_st+hip12_std")

(define_insn_reservation "hip12_fp_store_vector" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_ldr"))
  "hip12_alu+hip12_st+hip12_std")

(define_insn_reservation "hip12_fp_store_pair" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_ldp,neon_ldp_q"))
  "hip12_st+hip12_std+hip12_alu")

;; ASIMD Int instructions.

(define_insn_reservation "hip12_neon_absolute_diff" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_abd,neon_abd_q,neon_abd_long"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_basic" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_abs,neon_abs_q,\
			neon_add,neon_add_q,\
			neon_sub,neon_sub_q,\
			neon_neg,neon_neg_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_long" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_add_long,neon_sub_long"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_wide" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_add_widen,neon_sub_widen"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_complex" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_qadd,neon_qadd_q,\
       neon_qsub,neon_qsub_q,\
       neon_qneg,neon_qneg_q,\
       neon_qabs,neon_qabs_q"))
  "hip12_v0123")
; arith pair not specified

; neon_reduc_add is used for both addp and [su]adalp
(define_insn_reservation "hip12_neon_arith_reduce" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_reduc_add,neon_reduc_add_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_cmp" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_compare,neon_compare_q,neon_compare_zero,\
			 neon_tst,neon_tst_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_arith_dot" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_dot,neon_dot_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_logical" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_logic,neon_logic_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_multiply_accumulate" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_mla_b,neon_mla_b_q,\
       neon_mla_h,neon_mla_h_q,\
       neon_mla_s,neon_mla_s_q,\
       neon_mla_b_long,neon_mla_h_long,\
       neon_mla_s_long,neon_mla_h_scalar,\
       neon_mla_h_scalar_q,neon_mla_s_scalar,\
       neon_mla_s_scalar_q,neon_mla_h_scalar_long,\
       neon_mla_s_scalar_long,neon_sat_mla_b_long,\
       neon_sat_mla_h_long,neon_sat_mla_s_long,\
       neon_sat_mla_h_scalar_long,neon_sat_mla_s_scalar_long"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_minmax" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_minmax,neon_minmax_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_minmax_reduce" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_reduc_minmax,neon_reduc_minmax_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_multiply" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_mul_b,neon_mul_b_q,\
        neon_mul_h,neon_mul_h_q,\
        neon_mul_s,neon_mul_s_q,\
        neon_mul_b_long,neon_mul_h_long,\
        neon_mul_s_long,neon_mul_d_long,\
        neon_mul_h_scalar,neon_mul_h_scalar_q,\
        neon_mul_s_scalar,neon_mul_s_scalar_q,\
        neon_mul_h_scalar_long,neon_mul_s_scalar_long,\
        neon_sat_mul_b,neon_sat_mul_b_q,\
        neon_sat_mul_h,neon_sat_mul_h_q,\
        neon_sat_mul_s,neon_sat_mul_s_q,\
        neon_sat_mul_b_long,neon_sat_mul_h_long,\
        neon_sat_mul_s_long,neon_sat_mul_h_scalar,\
        neon_sat_mul_h_scalar_q,neon_sat_mul_s_scalar,\
        neon_sat_mul_s_scalar_q,neon_sat_mul_h_scalar_long,\
        neon_sat_mul_s_scalar_long"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_shift" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_shift_imm,neon_shift_imm_q,\
        neon_shift_imm_narrow_q,neon_shift_imm_long,\
        neon_shift_reg,neon_shift_reg_q,\
        neon_sat_shift_imm,neon_sat_shift_imm_q,\
        neon_sat_shift_imm_narrow_q,neon_sat_shift_reg,\
        neon_sat_shift_reg_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_shift_accumulate" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_shift_acc,neon_shift_acc_q"))
  "hip12_v0123")

;; ASIMD FP instructions.

(define_insn_reservation "hip12_neon_fp_abs" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_abs_s,neon_fp_abs_s_q,\
       neon_fp_abs_d,neon_fp_abs_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_neg" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_neg_s,neon_fp_neg_s_q,\
       neon_fp_neg_d,neon_fp_neg_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_abd" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_abd_s,neon_fp_abd_s_q,\
       neon_fp_abd_d,neon_fp_abd_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_arith" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_addsub_s,neon_fp_addsub_s_q,\
       neon_fp_addsub_d,neon_fp_addsub_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_compare" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_compare_s,neon_fp_compare_s_q,\
       neon_fp_compare_d,neon_fp_compare_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_convert_narrow" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_cvt_narrow_s_q,neon_fp_cvt_narrow_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_convert_2int" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_to_int_s,neon_fp_to_int_d"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_convert_2int_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_to_int_s_q,neon_fp_to_int_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_convert_from_int" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_int_to_fp_s,neon_int_to_fp_d"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_convert_from_int_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_int_to_fp_s_q,neon_int_to_fp_d_q"))
  "hip12_v0123")

; D/F32
(define_insn_reservation "hip12_neon_fp_divide_s" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_div_s"))
  "hip12_v0123")

; Q/F32
(define_insn_reservation "hip12_neon_fp_divide_s_q" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_div_s_q"))
  "hip12_v0123")

; Q/F64
(define_insn_reservation "hip12_neon_fp_divide_d" 9
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_div_d,neon_fp_div_d_q"))
  "hip12_v0123")

; D/F32
(define_insn_reservation "hip12_neon_fp_sqrt_s" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_sqrt_s"))
  "hip12_v0123")

; Q/F32
(define_insn_reservation "hip12_neon_fp_sqrt_s_q" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_sqrt_s_q"))
  "hip12_v0123")

; Q/F64
(define_insn_reservation "hip12_neon_fp_sqrt_d" 9
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_sqrt_d,neon_fp_sqrt_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_minmax" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_minmax_s,neon_fp_minmax_s_q,\
        neon_fp_minmax_d,neon_fp_minmax_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_minmax_reduce" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_s_q,\
        neon_fp_reduc_minmax_d,neon_fp_reduc_minmax_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_multiply" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_mul_s,neon_fp_mul_s_q,\
        neon_fp_mul_s_scalar,neon_fp_mul_s_scalar_q,\
        neon_fp_mul_d,neon_fp_mul_d_q,\
        neon_fp_mul_d_scalar_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_multiply_add" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_mla_s,neon_fp_mla_s_q,\
        neon_fp_mla_s_scalar,neon_fp_mla_s_scalar_q,\
        neon_fp_mla_d,neon_fp_mla_d_q,\
        neon_fp_mla_d_scalar_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_round" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_round_s,neon_fp_round_d"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_round_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_round_s_q,neon_fp_round_d_q"))
  "hip12_v0123")

;; ASIMD Miscellaneous instructions

(define_insn_reservation "hip12_neon_bit_reverse" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_rbit,neon_rbit_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_bitwise_insert" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_bsl,neon_bsl_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_count" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_cls,neon_cls_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_count_ds" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_cnt_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_count_bh" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_cnt"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_duplicate" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_dup,neon_dup_q"))
  "(hip12_v0123)+hip12_alu0134")

(define_insn_reservation "hip12_neon_extract" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_dup,neon_dup_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_insert" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_ins,neon_ins_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_move" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_move,neon_move_q,neon_move_narrow_q"))
  "hip12_v0123")

; gcc only gen neon fp recp
(define_insn_reservation "hip12_neon_fp_recp" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_recpe_s,neon_fp_recpe_d,\
       neon_fp_rsqrte_s,neon_fp_rsqrte_d"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_recp_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_recpe_s_q,neon_fp_recpe_d_q,\
       neon_fp_rsqrte_s_q,neon_fp_rsqrte_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_recpx" 3
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_recpx_s,neon_fp_recpx_s_q,\
        neon_fp_recpx_d,neon_fp_recpx_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_fp_recps" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_fp_recps_s,neon_fp_recps_s_q,\
        neon_fp_recps_d,neon_fp_recps_d_q,\
        neon_fp_rsqrts_s,neon_fp_rsqrts_s_q,\
        neon_fp_rsqrts_d,neon_fp_rsqrts_d_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_rev" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_rev,neon_rev_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_tbl_12" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_tbl1,neon_tbl1_q,\
        neon_tbl2,neon_tbl2_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_tbl_3" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_tbl3,neon_tbl3_q,\
        neon_tbl2,neon_tbl2_q"))
  "hip12_v0123")

(define_insn_reservation "hip12_neon_tbl_4" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_tbl4,neon_tbl4_q,\
        neon_tbl2,neon_tbl2_q"))
  "hip12_v0123")
; gcc only gen neon tbl, no tbx

; no neon transfer specified

(define_insn_reservation "hip12_neon_zip" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_zip,neon_zip_q"))
  "hip12_v0123")

;; ASIMD Load instructions.

(define_insn_reservation "hip12_neon_ld1_12reg" 6
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,\
       neon_load1_2reg,neon_load1_2reg_q"))
  "hip12_ld")

(define_insn_reservation "hip12_neon_ld1_34reg" 7
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load1_3reg,neon_load1_3reg_q,\
       neon_load1_4reg,neon_load1_4reg_q"))
  "hip12_ld")

(define_insn_reservation "hip12_neon_ld1_lane" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load1_all_lanes,neon_load1_all_lanes_q,\
       neon_load1_one_lane,neon_load1_one_lane_q"))
  "hip12_ld+(hip12_v0123)")

(define_insn_reservation "hip12_neon_ld2" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load2_2reg,neon_load2_2reg_q,\
       neon_load2_4reg,neon_load2_4reg_q,\
       neon_load2_all_lanes,neon_load2_all_lanes_q,\
       neon_load2_one_lane,neon_load2_one_lane_q"))
  "(hip12_ld)+(hip12_v0123)")

(define_insn_reservation "hip12_neon_ld3" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load3_3reg,neon_load3_3reg_q,\
       neon_load3_all_lanes,neon_load3_all_lanes_q,\
       neon_load3_one_lane,neon_load3_one_lane_q"))
  "hip12_ld+hip12_v0123")

(define_insn_reservation "hip12_neon_ld4_reg" 10
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load4_4reg,neon_load4_4reg_q"))
  "hip12_ld+hip12_v0123")

(define_insn_reservation "hip12_neon_ld4_lane" 8
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_load4_all_lanes,neon_load4_all_lanes_q,\
       neon_load4_one_lane,neon_load4_one_lane_q"))
  "hip12_ld+hip12_v0123")

;; ASIMD Load instructions.

(define_insn_reservation "hip12_neon_st1_12reg_4reg" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_store1_1reg,neon_store1_1reg_q,\
       neon_store1_2reg,neon_store1_2reg_q,\
       neon_store1_4reg"))
  "hip12_st+hip12_std+hip12_v0123")

(define_insn_reservation "hip12_neon_st1_3reg" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_store1_3reg,neon_store1_3reg_q"))
  "hip12_st+hip12_std+hip12_v0123")

(define_insn_reservation "hip12_neon_st1_4reg_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_store1_4reg_q"))
  "hip12_st+hip12_std")

(define_insn_reservation "hip12_neon_st1_lane_st2" 1
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_store1_one_lane,neon_store1_one_lane_q,\
       neon_store2_2reg,neon_store2_2reg_q,\
       neon_store2_4reg,neon_store2_4reg_q,\
       neon_store2_one_lane,neon_store2_one_lane_q"))
  "hip12_st+hip12_std")

(define_insn_reservation "hip12_neon_st3_st4_q" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_store3_3reg,neon_store3_3reg_q,\
       neon_store3_one_lane,neon_store3_one_lane_q,\
       neon_store4_4reg_q"))
  "hip12_v0123+hip12_st+hip12_std")

(define_insn_reservation "hip12_neon_st4" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "neon_store4_4reg,\
       neon_store4_one_lane,neon_store4_one_lane_q"))
  "hip12_v0123+hip12_st+hip12_std")

;; Cryptography Extensions

(define_insn_reservation "hip12_crypto_aes" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_aese,crypto_aesmc"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_pmull" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_pmull"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_sha1_fast" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha1_fast,crypto_sha1_xor"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_sha256_fast" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha256_fast"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_complex_1" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha1_slow"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_complex_256" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha256_slow"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sha512" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha512"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sha3" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sha3"))
  "hip12_v0123")

(define_insn_reservation "hip12_crypto_sm3" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sm3"))
  "hip12_v02")

(define_insn_reservation "hip12_crypto_sm4" 4
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crypto_sm4"))
  "hip12_v0123")

;; CRC instructions

(define_insn_reservation "hip12_crc" 2
  (and (eq_attr "tune" "hip12")
       (eq_attr "type" "crc"))
  "hip12_alu25")

;; Simple execution unit bypasses
(define_bypass 2 "hip12_fp_fused_multiply_add"
	         "hip12_fp_fused_multiply_add")
          
(define_bypass 2 "hip12_neon_arith_dot"
	         "hip12_neon_arith_dot")

(define_bypass 2 "hip12_neon_multiply_accumulate"
	         "hip12_neon_multiply_accumulate")

(define_bypass 1 "hip12_neon_shift_accumulate"
	         "hip12_neon_shift_accumulate")

(define_bypass 2 "hip12_neon_fp_multiply_add"
	         "hip12_neon_fp_multiply_add")

(define_bypass 2 "hip12_neon_fp_recps"
	         "hip12_neon_fp_recps")