;; hip09 pipeline description
;; Copyright (C) 2023 Free Software Foundation, Inc.
;;
;;Contributed by Yushuai Xing
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

(define_automaton "hip09")
(define_automaton "hip09_ldst")
(define_automaton "hip09_fsu")

(define_attr "hip09_type"
  "hip09_neon_abs, hip09_neon_fp_arith, hip09_neon_mul, hip09_neon_mla,
   hip09_neon_dot, hip09_neon_fp_div, hip09_neon_fp_sqrt,
   hip09_neon_ins, hip09_neon_load1, hip09_neon_load1_lanes,
   hip09_neon_load2and4, hip09_neon_load3_3reg,
   hip09_neon_load4_4reg, hip09_neon_store1and2,
   hip09_neon_store1_1reg, hip09_neon_store1_2reg,
   hip09_neon_store1_3reg, hip09_neon_store1_4reg,
   hip09_neon_store3and4_lane, hip09_neon_store3_3reg,
   hip09_neon_store4_4reg, unknown"
  (cond [
         (eq_attr "type" "neon_abs,neon_abs_q,neon_add,neon_add_q,\
                  neon_neg,neon_neg_q,neon_sub,neon_sub_q,neon_add_widen,\
                  neon_sub_widen,neon_qadd,neon_qadd_q,\
                  neon_add_long,neon_sub_long,\
                  neon_qabs,neon_qabs_q,neon_qneg,\
                  neon_qneg_q,neon_qsub,neon_qsub_q,neon_compare,\
                  neon_compare_q,neon_compare_zero,\
                  neon_compare_zero_q,neon_logic,neon_logic_q,\
                  neon_minmax,neon_minmax_q,neon_tst,\
                  neon_tst_q,neon_bsl,neon_bsl_q,\
                  neon_cls,neon_cls_q,neon_ext,\
                  neon_ext_q,neon_rev,neon_rev_q,\
                  neon_tbl1,neon_tbl1_q,neon_fp_abs_s,\
                  neon_fp_abs_s_q,neon_fp_abs_d,\
                  neon_fp_neg_s,neon_fp_neg_s_q,\
                  neon_fp_neg_d,neon_fp_neg_d_q,\
                  neon_shift_imm_narrow_q,neon_move,neon_move_q")
           (const_string "hip09_neon_abs")
         (eq_attr "type" "neon_abd,neon_abd_q,\
                  neon_arith_acc,neon_arith_acc_q,\
                  neon_add_halve,neon_add_halve_q,\
                  neon_sub_halve,neon_sub_halve_q,\
                  neon_add_halve_narrow_q,\
                  neon_sub_halve_narrow_q,neon_reduc_add,\
                  neon_reduc_add_q,\
                  neon_sat_mul_b,neon_sat_mul_b_q,\
                  neon_sat_mul_b_long,neon_mul_b,neon_mul_b_q,\
                  neon_mul_b_long,neon_mla_b,neon_mla_b_q,\
                  neon_mla_b_long,neon_sat_mla_b_long,\
                  neon_sat_shift_imm,\
                  neon_sat_shift_imm_q,neon_shift_imm_long,\
                  neon_shift_imm,neon_shift_imm_q,neon_cnt,\
                  neon_cnt_q,neon_fp_recpe_s,neon_fp_recpe_s_q,\
                  neon_fp_recpe_d,neon_fp_recpe_d_q,\
                  neon_fp_rsqrte_s,neon_fp_rsqrte_s_q,\
                  neon_fp_rsqrte_d,neon_fp_rsqrte_d_q,\
                  neon_fp_recpx_s,neon_fp_recpx_s_q,\
                  neon_fp_recpx_d,neon_fp_recpx_d_q,\
                  neon_tbl2,neon_tbl2_q,neon_to_gp,\
                  neon_to_gp_q,neon_fp_abd_s,neon_fp_abd_s_q,\
                  neon_fp_abd_d,neon_fp_abd_d_q,\
                  neon_fp_addsub_s,neon_fp_addsub_s_q,\
                  neon_fp_addsub_d,neon_fp_addsub_d_q,\
                  neon_fp_compare_s,neon_fp_compare_s_q,\
                  neon_fp_compare_d,neon_fp_compare_d_q,\
                  neon_fp_cvt_widen_s,neon_fp_to_int_s,\
                  neon_fp_to_int_s_q,neon_fp_to_int_d,\
                  neon_fp_to_int_d_q,neon_fp_minmax_s,\
                  neon_fp_minmax_s_q,neon_fp_minmax_d,\
                  neon_fp_minmax_d_q,neon_fp_round_s,\
                  neon_fp_round_s_q,neon_fp_cvt_narrow_d_q,\
                  neon_fp_round_d,neon_fp_round_d_q,\
                  neon_fp_cvt_narrow_s_q")
           (const_string "hip09_neon_fp_arith")
         (eq_attr "type" "neon_sat_mul_h,neon_sat_mul_h_q,\
                  neon_sat_mul_s,neon_sat_mul_s_q,\
                  neon_sat_mul_h_scalar,neon_sat_mul_s_scalar,\
                  neon_sat_mul_h_scalar_q,neon_sat_mul_h_long,\
                  neon_sat_mul_s_long,neon_sat_mul_h_scalar_long,\
                  neon_sat_mul_s_scalar_long,neon_mul_h,neon_mul_h_q,\
                  neon_mul_s,neon_mul_s_q,neon_mul_h_long,\
                  neon_mul_s_long,neon_mul_h_scalar_long,\
                  neon_mul_s_scalar_long,neon_mla_h,neon_mla_h_q,\
                  neon_mla_s,neon_mla_h_scalar,\
                  neon_mla_h_scalar_q,neon_mla_s_scalar,\
                  neon_mla_h_long,\
                  neon_mla_s_long,neon_sat_mla_h_long,\
                  neon_sat_mla_s_long,neon_sat_mla_h_scalar_long,\
                  neon_sat_mla_s_scalar_long,neon_mla_s_scalar_long,\
                  neon_mla_h_scalar_long,neon_mla_s_scalar_q,\
                  neon_shift_acc,neon_shift_acc_q,neon_shift_reg,\
                  neon_shift_reg_q,neon_sat_shift_reg,\
                  neon_sat_shift_reg_q,neon_sat_shift_imm_narrow_q,\
                  neon_tbl3,neon_tbl3_q,neon_fp_reduc_add_s,\
                  neon_fp_reduc_add_s_q,neon_fp_reduc_add_d,\
                  neon_fp_reduc_add_d_q,neon_fp_reduc_minmax_s,\
                  neon_fp_reduc_minmax_d,neon_fp_reduc_minmax_s_q,\
                  neon_fp_reduc_minmax_d_q,\
                  neon_fp_mul_s_q,\
                  neon_fp_mul_d,neon_fp_mul_d_q,\
                  neon_fp_mul_d_scalar_q,neon_fp_mul_s_scalar,\
                  neon_fp_mul_s_scalar_q")
           (const_string "hip09_neon_mul")
         (eq_attr "type" "neon_mla_s_q,neon_reduc_minmax,\
                  neon_reduc_minmax_q,neon_fp_recps_s,\
                  neon_fp_recps_s_q,neon_fp_recps_d,\
                  neon_fp_recps_d_q,neon_tbl4,neon_tbl4_q,\
                  neon_fp_mla_s,\
                  neon_fp_mla_d,neon_fp_mla_d_q,\
                  neon_fp_mla_s_scalar,neon_fp_mla_s_scalar_q,\
                  neon_fp_mla_d_scalar_q")
           (const_string "hip09_neon_mla")
         (eq_attr "type" "neon_dot,neon_dot_q")
           (const_string "hip09_neon_dot")
         (eq_attr "type" "neon_fp_div_s,neon_fp_div_s_q,\
                   neon_fp_div_d,neon_fp_div_d_q")
           (const_string "hip09_neon_fp_div")
         (eq_attr "type" "neon_fp_sqrt_s,neon_fp_sqrt_s_q,\
                   neon_fp_sqrt_d,neon_fp_sqrt_d_q")
           (const_string "hip09_neon_fp_sqrt")
         (eq_attr "type" "neon_dup,neon_dup_q,\
                   neon_ins,neon_ins_q")
           (const_string "hip09_neon_ins")
         (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,\
                   neon_load1_2reg,neon_load1_2reg_q,\
                   neon_load1_3reg,neon_load1_3reg_q,\
                   neon_load1_4reg,neon_load1_4reg_q")
           (const_string "hip09_neon_load1")
         (eq_attr "type" "neon_load1_one_lane,\
                   neon_load1_one_lane_q,\
                   neon_load1_all_lanes,neon_load1_all_lanes_q")
           (const_string "hip09_neon_load1_lanes")
         (eq_attr "type" "neon_load2_all_lanes,\
                   neon_load2_all_lanes_q,\
                   neon_load2_one_lane,neon_load2_2reg,\
                   neon_load2_2reg_q,neon_load3_one_lane,\
                   neon_load3_all_lanes,neon_load3_all_lanes_q,\
                   neon_load4_one_lane,neon_load4_all_lanes,\
                   neon_load4_all_lanes_q")
           (const_string "hip09_neon_load2and4")
         (eq_attr "type" "neon_load3_3reg,neon_load3_3reg_q")
           (const_string "hip09_neon_load3_3reg")
         (eq_attr "type" "neon_load4_4reg,neon_load4_4reg_q")
           (const_string "hip09_neon_load4_4reg")
         (eq_attr "type" "neon_store1_one_lane,\
                   neon_store1_one_lane_q,neon_store2_one_lane,\
                   neon_store2_one_lane_q,neon_store2_2reg,\
                   neon_store2_2reg_q")
           (const_string "hip09_neon_store1and2")
         (eq_attr "type" "neon_store1_1reg,neon_store1_1reg_q")
           (const_string "hip09_neon_store1_1reg")
         (eq_attr "type" "neon_store1_2reg,neon_store1_2reg_q")
           (const_string "hip09_neon_store1_2reg")
         (eq_attr "type" "neon_store1_3reg,neon_store1_3reg_q")
           (const_string "hip09_neon_store1_3reg")
         (eq_attr "type" "neon_store1_4reg,neon_store1_4reg_q")
           (const_string "hip09_neon_store1_4reg")
         (eq_attr "type" "neon_store3_one_lane,\
                   neon_store3_one_lane_q,neon_store4_one_lane,\
                   neon_store4_one_lane_q")
           (const_string "hip09_neon_store3and4_lane")
         (eq_attr "type" "neon_store3_3reg,\
                  neon_store3_3reg_q")
           (const_string "hip09_neon_store3_3reg")
         (eq_attr "type" "neon_store4_4reg,\
                   neon_store4_4reg_q")
           (const_string "hip09_neon_store4_4reg")]
  (const_string "unknown")))

; The hip09 core is modelled as issues pipeline that has
; the following functional units.
; 1.  Two pipelines for branch micro operations: BRU1, BRU2

(define_cpu_unit "hip09_bru0" "hip09")
(define_cpu_unit "hip09_bru1" "hip09")

(define_reservation "hip09_bru01" "hip09_bru0|hip09_bru1")

; 2.  Four pipelines for single cycle integer micro operations: ALUs1, ALUs2, ALUs3, ALUs4

(define_cpu_unit "hip09_alus0" "hip09")
(define_cpu_unit "hip09_alus1" "hip09")
(define_cpu_unit "hip09_alus2" "hip09")
(define_cpu_unit "hip09_alus3" "hip09")

(define_reservation "hip09_alus0123" "hip09_alus0|hip09_alus1|hip09_alus2|hip09_alus3")
(define_reservation "hip09_alus01" "hip09_alus0|hip09_alus1")
(define_reservation "hip09_alus23" "hip09_alus2|hip09_alus3")

; 3. Two pipelines for multi cycles integer micro operations: ALUm1, ALUm2

(define_cpu_unit "hip09_alum0" "hip09")
(define_cpu_unit "hip09_alum1" "hip09")

(define_reservation "hip09_alum01" "hip09_alum0|hip09_alum1")

; 4. Two pipelines for load micro opetations: Load1, Load2

(define_cpu_unit "hip09_load0" "hip09_ldst")
(define_cpu_unit "hip09_load1" "hip09_ldst")

(define_reservation "hip09_ld01" "hip09_load0|hip09_load1")

; 5. Two pipelines for store micro operations: Store1, Store2

(define_cpu_unit "hip09_store0" "hip09_ldst")
(define_cpu_unit "hip09_store1" "hip09_ldst")

(define_reservation "hip09_st01" "hip09_store0|hip09_store1")

; 6. Two pipelines for store data micro operations: STD0,STD1

(define_cpu_unit "hip09_store_data0" "hip09_ldst")
(define_cpu_unit "hip09_store_data1" "hip09_ldst")

(define_reservation "hip09_std01" "hip09_store_data0|hip09_store_data1")

; 7.  Four asymmetric pipelines for Asimd and FP micro operations: FSU1, FSU2, FSU3, FSU4

(define_cpu_unit "hip09_fsu0" "hip09_fsu")
(define_cpu_unit "hip09_fsu1" "hip09_fsu")
(define_cpu_unit "hip09_fsu2" "hip09_fsu")
(define_cpu_unit "hip09_fsu3" "hip09_fsu")

(define_reservation "hip09_fsu0123" "hip09_fsu0|hip09_fsu1|hip09_fsu2|hip09_fsu3")
(define_reservation "hip09_fsu02" "hip09_fsu0|hip09_fsu2")


; 8. Two pipelines for sve operations but same with fsu1 and fsu3: SVE1, SVE2

;; Simple Execution Unit:
;
;; Simple ALU without shift
(define_insn_reservation "hip09_alu" 1
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "alu_imm,logic_imm,\
            adc_imm,adc_reg,\
            alu_sreg,logic_reg,\
            mov_imm,mov_reg,\
            csel,rotate_imm,bfm,mov_imm,\
            clz,rbit,rev"))
  "hip09_alus0123")

(define_insn_reservation "hip09_alus" 1
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "alus_sreg,alus_imm,\
            adcs_reg,adcs_imm,\
            logics_imm,logics_reg,adr"))
  "hip09_alus23")

;; ALU ops with shift and extend
(define_insn_reservation "hip09_alu_ext_shift" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "alu_ext,alus_ext,\
        logics_shift_imm,logics_shift_reg,\
        logic_shift_reg,logic_shift_imm,\
        "))
  "hip09_alum01")

;; Multiplies instructions
(define_insn_reservation "hip09_mult" 3
  (and (eq_attr "tune" "hip09")
       (ior (eq_attr "mul32" "yes")
       (eq_attr "widen_mul64" "yes")))
  "hip09_alum01")

;; Integer divide
(define_insn_reservation "hip09_div" 10
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "udiv,sdiv"))
  "hip09_alum0")

;; Branch execution Unit
;
; Branches take two issue slot.
; No latency as there is no result
(define_insn_reservation "hip09_branch" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "branch,call"))
  "hip09_bru01 + hip09_alus23")

;; Load execution Unit
;
; Loads of up to two words.
(define_insn_reservation "hip09_load1" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "load_4,load_8"))
  "hip09_ld01")

; Stores of up to two words.
(define_insn_reservation "hip09_store1" 1
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "store_4,store_8"))
  "hip09_st01")

;; FP data processing instructions.

(define_insn_reservation "hip09_fp_arith" 1
   (and (eq_attr "tune" "hip09")
        (eq_attr "type" "ffariths,ffarithd,fmov,fconsts,fconstd,\
         f_mrc"))
   "hip09_fsu0123")

(define_insn_reservation "hip09_fp_cmp" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fcmps,fcmpd"))
  "hip09_fsu0123+hip09_alus23")

(define_insn_reservation "hip09_fp_ccmp" 7
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fccmps,fccmpd"))
  "hip09_alus01+hip09_fsu0123+hip09_alus23")

(define_insn_reservation "hip09_fp_csel" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fcsel,f_mcr"))
  "hip09_alus01+hip09_fsu0123")

(define_insn_reservation "hip09_fp_divs" 7
  (and (eq_attr "tune" "hip09")
  (eq_attr "type" "fdivs"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_fp_divd" 10
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fdivd"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_fp_sqrts" 9
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fsqrts"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_fp_sqrtd" 15
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fsqrtd"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_fp_mul" 3
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fmuls,fmuld"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_fp_add" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fadds,faddd,f_minmaxs,f_minmaxd,f_cvt,\
       f_rints,f_rintd"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_fp_mac" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "fmacs,fmacd"))
  "hip09_fsu0123")

;; FP miscellaneous instructions.

(define_insn_reservation "hip09_fp_cvt" 5
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "f_cvtf2i"))
  "hip09_fsu0123+hip09_alus23")

(define_insn_reservation "hip09_fp_cvt2" 5
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "f_cvti2f"))
  "hip09_alus01+hip09_fsu0123")

;; FP Load Instructions 

(define_insn_reservation "hip09_fp_load" 7
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "f_loads,f_loadd"))
  "hip09_ld01")

(define_insn_reservation "hip09_fp_load2" 6
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "neon_ldp_q,neon_ldp"))
  "hip09_ld01+hip09_alus01")

;; FP store instructions

(define_insn_reservation "hip09_fp_store" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "f_stores,f_stored"))
  "hip09_st01+hip09_std01")

;; ASIMD integer instructions

(define_insn_reservation "hip09_asimd_base1" 1
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_abs"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_base2" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_fp_arith"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_base3" 3
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_mul"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_base4" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_mla"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_base5" 5
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "neon_fp_mul_s"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_dot" 6
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_dot"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_bfmmla" 9
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "neon_fp_mla_s_q"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_fdiv" 15
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_fp_div"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_fsqrt" 25
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_fp_sqrt"))
  "hip09_fsu0123")

(define_insn_reservation "hip09_asimd_pmull" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "crypto_pmull"))
  "hip09_fsu2")

(define_insn_reservation "hip09_asimd_dup" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_ins"))
  "hip09_alus01+hip09_fsu0123")

;; ASIMD load instructions

(define_insn_reservation "hip09_asimd_ld1_reg" 6
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_load1"))
  "hip09_ld01")

(define_insn_reservation "hip09_asimd_ld1_lane" 7
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_load1_lanes"))
  "hip09_ld01+hip09_fsu0123")

(define_insn_reservation "hip09_asimd_ld23" 8
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_load2and4"))
"hip09_ld01+hip09_fsu0123")

(define_insn_reservation "hip09_asimd_ld3_mtp" 9
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_load3_3reg"))
  "hip09_ld01+hip09_fsu0123")

(define_insn_reservation "hip09_asimd_ld4_mtp" 13
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_load4_4reg"))
  "hip09_ld01+hip09_fsu0123")

;; ASIMD store instructions

(define_insn_reservation "hip09_asimd_st12" 1
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store1and2"))
  "hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st1_1reg" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store1_1reg"))
  "hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st1_2reg" 3
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store1_2reg"))
  "hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st1_3reg" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store1_3reg"))
  "hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st1_4reg" 5
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store1_4reg"))
  "hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st34_lane" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store3and4_lane"))
  "hip09_fsu0123+hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st3_mtp" 7
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store3_3reg"))
  "hip09_fsu0123+hip09_st01+hip09_std01")

(define_insn_reservation "hip09_asimd_st4_mtp" 10
  (and (eq_attr "tune" "hip09")
       (eq_attr "hip09_type" "hip09_neon_store4_4reg"))
  "hip09_fsu0123+hip09_st01+hip09_std01")

;; Cryptography extensions

(define_insn_reservation "hip09_asimd_aes" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "crypto_aese,crypto_aesmc"))
  "hip09_fsu02")

(define_insn_reservation "hip09_asimd_sha3" 1
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "crypto_sha3"))
  "hip09_fsu2")

(define_insn_reservation "hip09_asimd_sha1" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "crypto_sha1_fast,crypto_sha1_xor,\
       crypto_sha256_fast,crypto_sha512,\
       crypto_sm3"))
  "hip09_fsu2")

(define_insn_reservation "hip09_asimd_sha1_and256" 4
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "crypto_sha1_slow,crypto_sha256_slow,\
       crypto_sm4"))
  "hip09_fsu2")

;; CRC extension.

(define_insn_reservation "hip09_crc" 2
  (and (eq_attr "tune" "hip09")
       (eq_attr "type" "crc"))
  "hip09_alum01")
