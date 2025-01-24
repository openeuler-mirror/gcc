;; hip10a pipeline description
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

(define_automaton "hip10a")
(define_automaton "hip10a_ldst")
(define_automaton "hip10a_fsu")

(define_attr "hip10a_type"
  "hip10a_neon_base1, hip10a_neon_base2, hip10a_neon_base3, hip10a_neon_base4,
   hip10a_neon_load1_12, hip10a_neon_load1_34, hip10a_neon_load1_lanes, hip10a_neon_load2,
   hip10a_neon_load34_all_lane, hip10a_neon_load34_one_lane, hip10a_neon_load34, hip10a_neon_load34_q,
   hip10a_neon_store1, hip10a_neon_store2, hip10a_neon_store1_34reg_d, hip10a_neon_store1_12reg_d,
   hip10a_neon_store34,
   unknown"
  (cond [
        (eq_attr "type" "neon_abs,neon_abs_q,\
                neon_neg,neon_neg_q,\
                neon_add,neon_add_q,neon_add_widen,neon_add_long,\
                neon_sub,neon_sub_q,neon_sub_widen,neon_sub_long,\
                neon_qadd,neon_qadd_q,\
                neon_qsub,neon_qsub_q,\
                neon_qabs,neon_qabs_q,\
                neon_qneg,neon_qneg_q,\
                neon_compare,neon_compare_q,neon_compare_zero,neon_compare_zero_q,\
                neon_logic,neon_logic_q,\
                neon_minmax,neon_minmax_q,\
                neon_tst,neon_tst_q,\
                neon_bsl,neon_bsl_q,\
                neon_cls,neon_cls_q,\
                neon_ext,neon_ext_q,\
                neon_rev,neon_rev_q,\
                neon_fp_abs_s,neon_fp_abs_s_q,neon_fp_abs_d,\
                neon_fp_neg_s,neon_fp_neg_s_q,neon_fp_neg_d,neon_fp_neg_d_q,\
                neon_move,neon_move_q,\
                neon_ins,neon_ins_q")
          (const_string "hip10a_neon_base1")
        (eq_attr "type" "neon_abd,neon_abd_q,\
                neon_tbl1,neon_tbl1_q,\
                neon_arith_acc,neon_arith_acc_q,\
                neon_add_halve,neon_add_halve_q,neon_add_halve_narrow_q,\
                neon_sub_halve,neon_sub_halve_q,neon_sub_halve_narrow_q,\
                neon_sat_shift_imm,neon_sat_shift_imm_q,\
                neon_shift_imm,neon_shift_imm_q,neon_shift_imm_long,\
                neon_shift_imm_narrow_q,\
                neon_cnt,neon_cnt_q,\
                neon_tbl1,neon_tbl1_q,neon_tbl2,neon_tbl2_q,\
                neon_to_gp,neon_to_gp_q,\
                neon_fp_recpe_s,neon_fp_recpe_s_q,\
                neon_fp_recpe_d,neon_fp_recpe_d_q,\
                neon_fp_rsqrte_s,neon_fp_rsqrte_s_q,\
                neon_fp_rsqrte_d,neon_fp_rsqrte_d_q,\
                neon_fp_recpx_s,neon_fp_recpx_s_q,\
                neon_fp_recpx_d,neon_fp_recpx_d_q,\
                neon_fp_abd_s,neon_fp_abd_s_q,\
                neon_fp_abd_d,neon_fp_abd_d_q,\
                neon_fp_addsub_s,neon_fp_addsub_s_q,\
                neon_fp_addsub_d,neon_fp_addsub_d_q,\
                neon_fp_compare_s,neon_fp_compare_s_q,\
                neon_fp_compare_d,neon_fp_compare_d_q,\
                neon_fp_minmax_s,\
                neon_fp_minmax_s_q,neon_fp_minmax_d,\
                neon_fp_minmax_d_q,neon_fp_round_s,\
                neon_fp_round_s_q,\
                neon_fp_round_d,neon_fp_round_d_q")
          (const_string "hip10a_neon_base2")
        (eq_attr "type" "neon_dot,neon_dot_q,\
                neon_reduc_add,neon_reduc_add_q,\
                neon_sat_mul_b,neon_sat_mul_b_q,neon_sat_mul_b_long,\
                neon_sat_mul_h,neon_sat_mul_h_q,\
                neon_sat_mul_s,neon_sat_mul_s_q,\
                neon_sat_mul_h_scalar,neon_sat_mul_s_scalar,\
                neon_sat_mul_h_scalar_q,neon_sat_mul_h_long,\
                neon_sat_mul_s_long,neon_sat_mul_h_scalar_long,\
                neon_sat_mul_s_scalar_long,neon_mul_h,neon_mul_h_q,\
                neon_mul_b,neon_mul_b_q,neon_mul_b_long,\
                neon_mul_s,neon_mul_s_q,neon_mul_h_long,\
                neon_mul_s_long,neon_mul_h_scalar_long,\
                neon_mul_s_scalar_long,\
                neon_mla_b,neon_mla_b_q,neon_mla_b_long,\
                neon_mla_h,neon_mla_h_q,neon_mla_h_long,\
                neon_mla_h_scalar,neon_mla_h_scalar_q,neon_mla_h_scalar_long,\
                neon_mla_s,neon_mla_s_q,neon_mla_s_long,\
                neon_mla_s_scalar,neon_mla_s_scalar_q,neon_mla_s_scalar_long,\
                neon_sat_mla_b_long,\
                neon_sat_mla_h_long,\
                neon_sat_mla_h_scalar_long,\
                neon_sat_mla_s_long,\
                neon_sat_mla_s_scalar_long,\
                neon_shift_acc,neon_shift_acc_q,neon_shift_reg,neon_shift_reg_q,\
                neon_sat_shift_reg,neon_sat_shift_reg_q,neon_sat_shift_imm_narrow_q,\
                neon_reduc_minmax,neon_reduc_minmax_q,\
                neon_fp_reduc_add_s,neon_fp_reduc_add_s_q,\
                neon_fp_reduc_add_d,neon_fp_reduc_add_d_q,\
                neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_s_q,\
                neon_fp_reduc_minmax_d,neon_fp_reduc_minmax_d_q,\
                neon_fp_mul_s,neon_fp_mul_s_q,neon_fp_mul_s_scalar,\
                neon_fp_mul_d,neon_fp_mul_d_q,neon_fp_mul_d_scalar_q,\
                neon_fp_mul_s_scalar_q,\
                neon_fp_recpe_s,neon_fp_recpe_d,\
                neon_fp_recpx_s,neon_fp_recpx_s_q,neon_fp_recpx_d,neon_fp_recpx_d_q,\
                neon_fp_to_int_s,neon_fp_to_int_d")
          (const_string "hip10a_neon_base3")
        (eq_attr "type" "neon_tbl3,neon_tbl3_q,\
                neon_fp_recpe_s_q,neon_fp_recpe_d_q,\
                neon_fp_recps_s_q,neon_fp_recps_d,neon_fp_recps_s,neon_fp_recps_d_q,\
                neon_fp_to_int_s_q,neon_fp_to_int_d_q,\
                neon_fp_cvt_narrow_d_q,neon_fp_cvt_narrow_s_q,\
                neon_fp_mla_s,neon_fp_mla_s_q,\
                neon_fp_mla_d,neon_fp_mla_d_q,\
                neon_fp_mla_s_scalar,neon_fp_mla_s_scalar_q,\
                neon_fp_mla_d_scalar_q")
          (const_string "hip10a_neon_base4")
        (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q,\
                neon_load1_2reg,neon_load1_2reg_q")
          (const_string "hip10a_neon_load1_12")
        (eq_attr "type" "neon_load1_3reg,neon_load1_3reg_q,\
                neon_load1_4reg,neon_load1_4reg_q")
          (const_string "hip10a_neon_load1_34")
        (eq_attr "type" "neon_load1_one_lane,\
                neon_load1_one_lane_q,\
                neon_load1_all_lanes,neon_load1_all_lanes_q")
          (const_string "hip10a_neon_load1_lanes")
        (eq_attr "type" "neon_load2_all_lanes,\
                neon_load2_all_lanes_q,\
                neon_load2_one_lane,neon_load2_2reg,\
                neon_load2_2reg_q,neon_load3_one_lane")
          (const_string "hip10a_neon_load2")
        (eq_attr "type" "neon_load4_one_lane,neon_load4_one_lane")
          (const_string "hip10a_neon_load34_one_lane")
        (eq_attr "type" "neon_load3_all_lanes,neon_load3_all_lanes_q,\
                neon_load4_all_lanes,neon_load4_all_lanes_q")
          (const_string "hip10a_neon_load34_all_lane")
        (eq_attr "type" "neon_load3_3reg,neon_load4_4reg")
          (const_string "hip10a_neon_load34")
        (eq_attr "type" "neon_load3_3reg_q,neon_load4_4reg_q")
          (const_string "hip10a_neon_load34_q")
        (eq_attr "type" "neon_store1_1reg_q,neon_store1_2reg_q,\
                neon_store1_3reg_q,neon_store1_4reg_q,\
                neon_store1_one_lane,neon_store1_one_lane_q")
          (const_string "hip10a_neon_store1")
        (eq_attr "type" "neon_store2_one_lane,neon_store2_one_lane_q,\
                neon_store2_2reg,neon_store2_2reg_q")
          (const_string "hip10a_neon_store2")
        (eq_attr "type" "neon_store1_1reg,neon_store1_2reg")
          (const_string "hip10a_neon_store1_12reg_d")
        (eq_attr "type" "neon_store1_3reg,neon_store1_4reg")
          (const_string "hip10a_neon_store1_34reg_d")
        (eq_attr "type" "neon_store3_one_lane,neon_store3_one_lane_q,\
                neon_store4_one_lane,neon_store4_one_lane_q,\
                neon_store3_3reg_q,neon_store3_3reg,\
                neon_store4_4reg_q,neon_store4_4reg")
          (const_string "hip10a_neon_store34")]
  (const_string "unknown")))

; The hip10a core is modelled as issues pipeline that has
; the following functional units.
; 1.  Three pipelines for single cycle integer micro operations: ALUs0, ALUs1, ALUs2

(define_cpu_unit "hip10a_alus0" "hip10a")
(define_cpu_unit "hip10a_alus1" "hip10a")
(define_cpu_unit "hip10a_alus2" "hip10a")

(define_reservation "hip10a_alus012" "hip10a_alus0|hip10a_alus1|hip10a_alus2")
;(define_reservation "hip10a_alus01" "hip10a_alus0|hip10a_alus1")
;(define_reservation "hip10a_alus23" "hip10a_alus2|hip10a_alus3")

; 2. Three pipelines for multi cycles integer micro operations: ALUm0, ALUm1, ALUm2

(define_cpu_unit "hip10a_alum0" "hip10a")
(define_cpu_unit "hip10a_alum1" "hip10a")
(define_cpu_unit "hip10a_alum2" "hip10a")

(define_reservation "hip10a_alum012" "hip10a_alum0|hip10a_alum1|hip10a_alum2")

; 3. All ALU pipelines

(define_reservation "hip10a_alu" "hip10a_alus0|hip10a_alus1|hip10a_alus2|hip10a_alum0|hip10a_alum1|hip10a_alum2")

; 4. Three pipelines for load micro opetations: Load0, Load1, Load2

(define_cpu_unit "hip10a_load0" "hip10a_ldst")
(define_cpu_unit "hip10a_load1" "hip10a_ldst")
(define_cpu_unit "hip10a_load2" "hip10a_ldst")

(define_reservation "hip10a_ld012" "hip10a_load0|hip10a_load1|hip10a_load2")

; 5. Two pipelines for store micro operations: Store1, Store2

(define_cpu_unit "hip10a_store0" "hip10a_ldst")
(define_cpu_unit "hip10a_store1" "hip10a_ldst")

(define_reservation "hip10a_st01" "hip10a_store0|hip10a_store1")

; 6. Two pipelines for store data micro operations: STD0,STD1

(define_cpu_unit "hip10a_store_data0" "hip10a_ldst")
(define_cpu_unit "hip10a_store_data1" "hip10a_ldst")

(define_reservation "hip10a_std01" "hip10a_store_data0|hip10a_store_data1")

; 7.  Four asymmetric pipelines for Asimd and FP micro operations: FSU0, FSU1, FSU2, FSU3

(define_cpu_unit "hip10a_fsu0" "hip10a_fsu")
(define_cpu_unit "hip10a_fsu1" "hip10a_fsu")
(define_cpu_unit "hip10a_fsu2" "hip10a_fsu")
(define_cpu_unit "hip10a_fsu3" "hip10a_fsu")

(define_reservation "hip10a_fsu0123" "hip10a_fsu0|hip10a_fsu1|hip10a_fsu2|hip10a_fsu3")
(define_reservation "hip10a_fsu02" "hip10a_fsu0|hip10a_fsu2")


; 8. Two pipelines for sve operations but same with fsu1 and fsu3: SVE1, SVE2

;; Branch execution Unit
;
; Branches take two issue slot.
; No latency as there is no result
(define_insn_reservation "hip10a_branch" 0
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "branch,call"))
  "hip10a_alus012")

;; Simple Execution Unit:
;
;; Simple ALU without shift
(define_insn_reservation "hip10a_alu_all" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "alu_imm,\
            adc_imm,adc_reg,\
            alu_sreg,\
            mov_imm,mov_reg"))
  "hip10a_alu")

(define_insn_reservation "hip10a_alum" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "logic_imm,logic_reg,\
            csel,rotate_imm,bfm,\
            clz,rbit,rev"))
  "hip10a_alum012")

(define_insn_reservation "hip10a_alus" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "alus_sreg,alus_imm,\
            adcs_reg,adcs_imm,\
            logics_imm,logics_reg,adr"))
  "hip10a_alus012")

;; ALU ops with shift and extend
(define_insn_reservation "hip10a_alu_ext_shift" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "alu_ext,alus_ext,\
        logics_shift_imm,logics_shift_reg,\
        logic_shift_reg,logic_shift_imm,\
        "))
  "hip10a_alum012")

;; Multiply and mulitply accumulate and count leading zeros
(define_insn_reservation "hip10a_mul" 3
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "mul,muls,clz,smull,umull"))
  "hip10a_alum012")

(define_insn_reservation "hip10a_mla" 4
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "mla,mlas,smlal,umlal"))
  "hip10a_alum012|hip10a_alu")

;; Integer divide
(define_insn_reservation "hip10a_div" 11
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "udiv,sdiv"))
  "hip10a_alum0")

;; Load execution Unit
;
; Loads of up to two words.
(define_insn_reservation "hip10a_load1" 4
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "load_4,load_8,load_16"))
  "hip10a_ld012")

; Stores of up to two words.
(define_insn_reservation "hip10a_store1" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "store_4,store_8,load_16"))
  "hip10a_st01")

;; FP data processing instructions.

(define_insn_reservation "hip10a_fp_arith" 1
   (and (eq_attr "tune" "hip10a")
        (eq_attr "type" "ffariths,ffarithd,fmov,fconsts,fconstd,\
         f_mrc"))
   "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_cmp" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fcmps,fcmpd"))
  "hip10a_fsu02+hip10a_alus012")

(define_insn_reservation "hip10a_fp_ccmp" 6
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fccmps,fccmpd"))
  "hip10a_fsu0123+hip10a_alus012")

(define_insn_reservation "hip10a_fp_csel" 6
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fcsel,f_mcr"))
  "hip10a_fsu0123+hip10a_alus012")

(define_insn_reservation "hip10a_fp_divs" 7
  (and (eq_attr "tune" "hip10a")
  (eq_attr "type" "fdivs"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_divd" 10
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fdivd"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_sqrts" 9
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fsqrts"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_sqrtd" 15
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fsqrtd"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_mul" 3
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fmuls,fmuld"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_add" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fadds,faddd,f_minmaxs,f_minmaxd,f_cvt,\
       f_rints,f_rintd"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_fp_mac" 4
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "fmacs,fmacd"))
  "hip10a_fsu0123")

;; FP miscellaneous instructions.

(define_insn_reservation "hip10a_fp_cvt" 5
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "f_cvtf2i"))
  "hip10a_fsu0123+hip10a_alus012")

(define_insn_reservation "hip10a_fp_cvt2" 6
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "f_cvti2f"))
  "hip10a_alus012+hip10a_fsu0123")

;; FP Load Instructions

(define_insn_reservation "hip10a_fp_load" 8
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "f_loads,f_loadd"))
  "hip10a_ld012")

(define_insn_reservation "hip10a_fp_load2" 6
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_ldp_q,neon_ldp"))
  "hip10a_ld012+hip10a_alu")

;; FP store instructions

(define_insn_reservation "hip10a_fp_store" 3
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "f_stores,f_stored"))
  "hip10a_st01+hip10a_std01")

(define_insn_reservation "hip10a_fp_store2" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_stp_q,neon_stp"))
  "hip10a_st01+hip10a_std01+hip10a_alu")

;; ASIMD integer instructions

(define_insn_reservation "hip10a_asimd_base1" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_base1"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base2" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_base2"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base3" 3
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_base3"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base4" 4
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_base4"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base5" 5
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" ""))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base6" 6
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_tbl4,neon_tbl4_q"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base7" 7
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_fp_div_s,neon_fp_div_d"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_base9" 9
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_fp_div_s_q,neon_fp_sqrt_s,neon_fp_sqrt_d"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_fsqrt_q" 13
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_fp_sqrt_s_q"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_fdiv_f64_q" 15
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_fp_div_d_q"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_fsqrt_f64_q" 25
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_fp_sqrt_d_q"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_dup" 5
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "neon_dup,neon_dup_q"))
  "hip10a_alus012+hip10a_fsu0123")

;; ASIMD load instructions

(define_insn_reservation "hip10a_asimd_ld1_12" 6
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_load1_12"))
  "hip10a_ld012")

(define_insn_reservation "hip10a_asimd_ld1_34" 7
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_load1_34"))
  "hip10a_ld012")

(define_insn_reservation "hip10a_asimd_ld7" 7
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_load1_lanes,hip10a_neon_load2,hip10a_neon_load34_all_lane,hip10a_neon_load34"))
  "hip10a_ld012+hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_ld8" 8
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_load34_one_lane,hip10a_neon_load34_q"))
"hip10a_ld012+hip10a_fsu0123")

;; ASIMD store instructions

(define_insn_reservation "hip10a_asimd_st1" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_store1,hip10a_neon_store2"))
  "hip10a_st01+hip10a_std01")

(define_insn_reservation "hip10a_asimd_st1_12" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_store1_12reg_d"))
  "hip10a_st01+hip10a_std01+hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_st4" 4
  (and (eq_attr "tune" "hip10a")
       (eq_attr "hip10a_type" "hip10a_neon_store1_34reg_d,hip10a_neon_store34"))
  "hip10a_fsu0123+hip10a_st01+hip10a_std01")

;; Cryptography extensions


(define_insn_reservation "hip10a_asimd_pmull" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "crypto_pmull"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_aes" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "crypto_aese,crypto_aesmc"))
  "hip10a_fsu0+hip10a_fsu2")

(define_insn_reservation "hip10a_asimd_sha3" 1
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "crypto_sha3"))
  "hip10a_fsu0123")

(define_insn_reservation "hip10a_asimd_sha1" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "crypto_sha1_fast,crypto_sha1_xor,\
       crypto_sha256_fast,crypto_sha512,\
       crypto_sm3"))
  "hip10a_fsu0+hip10a_fsu2")

(define_insn_reservation "hip10a_asimd_sha1_and256" 4
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "crypto_sha1_slow,crypto_sha256_slow,\
       crypto_sm4"))
  "hip10a_fsu0+hip10a_fsu2")

;; CRC extension.

(define_insn_reservation "hip10a_crc" 2
  (and (eq_attr "tune" "hip10a")
       (eq_attr "type" "crc"))
  "hip10a_alum012")
