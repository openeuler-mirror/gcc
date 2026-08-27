;; hip11 pipeline description
;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

(define_automaton "hip11")

;; The hip11 core is modelled as issues pipeline that has
;; the following functional units.
;; 1.  Three pipelines for integer operations: ALU1, ALU2, ALU3

(define_cpu_unit "hip11_alu1_issue" "hip11")
(define_reservation "hip11_alu1" "hip11_alu1_issue")

(define_cpu_unit "hip11_alu2_issue" "hip11")
(define_reservation "hip11_alu2" "hip11_alu2_issue")

(define_cpu_unit "hip11_alu3_issue" "hip11")
(define_reservation "hip11_alu3" "hip11_alu3_issue")

(define_reservation "hip11alu" "hip11_alu1|hip11_alu2|hip11_alu3")

;; 2.  One pipeline for complex integer operations: MDU

(define_cpu_unit "hip11_mdu_issue" "hip11")
(define_reservation "hip11_mdu" "hip11_mdu_issue")

;; 3.  Two asymmetric pipelines for Asimd and FP operations: FSU1, FSU2
(define_automaton "hip11_fsu")

(define_cpu_unit "hip11_fsu1_issue"
		 "hip11_fsu")
(define_cpu_unit "hip11_fsu2_issue"
		 "hip11_fsu")

(define_reservation "hip11_fsu1" "hip11_fsu1_issue")
(define_reservation "hip11_fsu2" "hip11_fsu2_issue")
(define_reservation "hip11_fsu_pipe" "hip11_fsu1|hip11_fsu2")

;; 4.  Two pipeline for branch operations but same with alu2 and alu3: BRU1, BRU2

;; 5.  Two pipelines for load and store operations: LS1, LS2.

(define_cpu_unit "hip11_ls1_issue" "hip11")
(define_cpu_unit "hip11_ls2_issue" "hip11")
(define_reservation "hip11_ls1" "hip11_ls1_issue")
(define_reservation "hip11_ls2" "hip11_ls2_issue")

;; Block all issue queues.

(define_reservation "hip11_block" "hip11_fsu1_issue + hip11_fsu2_issue
				  + hip11_mdu_issue + hip11_alu1_issue
				  + hip11_alu2_issue + hip11_alu3_issue + hip11_ls1_issue + hip11_ls2_issue")

;; Branch execution Unit
;;
(define_insn_reservation "hip11_branch" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "branch"))
  "hip11_alu2|hip11_alu3")

(define_insn_reservation "hip11_return_from_subroutine" 6
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "branch")
       (eq_attr "sls_length" "retbr"))
  "hip11_mdu,(hip11_alu2|hip11_alu3)")

  ;; Simple Execution Unit:
;;
;; Simple ALU without shift
(define_insn_reservation "hip11_alu" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "alu_imm,logic_imm,\
			alu_sreg,logic_reg,\
			adc_imm,adc_reg,\
			adr,bfm,clz,rbit,rev,\
			shift_imm,shift_reg,\
			mov_imm,mov_reg,\
			mvn_imm,mvn_reg,\
			mrs,multiple,csel,\
            rotate_imm"))
  "hip11_alu1|hip11_alu2|hip11_alu3")
  
(define_insn_reservation "hip11_alus" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "alus_imm,logics_imm,\
			alus_sreg,logics_reg,\
			adcs_imm,adcs_reg"))
  "hip11_alu2|hip11_alu3")

;; ALU ops with shift
(define_insn_reservation "hip11_alu_shift" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "extend,\
			alu_shift_imm_lsl_1to4,alu_shift_imm_other,alu_shift_reg,\
			crc,logic_shift_imm,logic_shift_reg,\
			mov_shift,mvn_shift,\
			mov_shift_reg,mvn_shift_reg"))
  "hip11_mdu")
  
(define_insn_reservation "hip11_alus_shift" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "alus_shift_imm,alus_shift_reg,\
			logics_shift_imm,logics_shift_reg"))
  "hip11_alu2|hip11_alu3")

;; Multiplies instructions
(define_insn_reservation "hip11_mult" 3
  (and (eq_attr "tune" "hip11")
       (ior (eq_attr "mul32" "yes")
	    (eq_attr "widen_mul64" "yes")))
  "hip11_mdu")

;; Integer divide
(define_insn_reservation "hip11_div" 10
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "udiv,sdiv"))
  "hip11_mdu")

(define_insn_reservation "hip11_mla" 4
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "mla,smlal,umlal,smull,umull"))
  "hip11_mdu")

;; Block all issue pipes for a cycle
(define_insn_reservation "hip11_block" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "block"))
  "hip11_block")

;; Load-store execution Unit
;;
(define_insn_reservation "hip11_load1" 4
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "load_4,load_8,load_16"))
  "hip11_ls1|hip11_ls2")

(define_insn_reservation "hip11_fp_load" 5
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "f_loads,f_loadd"))
  "hip11_ls1|hip11_ls2")

(define_insn_reservation "hip11_neon_ld1_single" 7
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load1_one_lane,neon_load1_one_lane_q,\
       neon_load1_all_lanes,neon_load1_all_lanes_q"))
  "(hip11_ls1|hip11_ls2)+hip11_fsu1")

(define_insn_reservation "hip11_neon_ld1_1reg" 5
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load1_1reg,neon_load1_1reg_q"))
  "hip11_ls1|hip11_ls2")

(define_insn_reservation "hip11_neon_ld1_2reg" 6
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load1_2reg,neon_load1_2reg_q"))
  "hip11_ls1|hip11_ls2")

(define_insn_reservation "hip11_neon_ld1_3reg" 7
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load1_3reg,neon_load1_3reg_q"))
  "hip11_ls1|hip11_ls2")

(define_insn_reservation "hip11_neon_ld1_4reg" 8
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load1_4reg,neon_load1_4reg_q"))
  "hip11_ls1|hip11_ls2")

(define_insn_reservation "hip11_neon_ld2" 8
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load2_one_lane,neon_load2_one_lane_q,\
       neon_load2_all_lanes,neon_load2_all_lanes_q,\
       neon_load2_2reg,neon_load2_2reg_q,\
       neon_load2_4reg,neon_load2_4reg_q"))
  "(hip11_ls1|hip11_ls2)+hip11_fsu1")

(define_insn_reservation "hip11_neon_ld3_single" 9
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load3_one_lane,neon_load3_one_lane_q,\
       neon_load3_all_lanes,neon_load3_all_lanes_q"))
  "(hip11_ls1|hip11_ls2)+hip11_fsu1")

(define_insn_reservation "hip11_neon_ld3_multiple" 13
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load3_3reg,neon_load3_3reg_q"))
  "(hip11_ls1|hip11_ls2)+hip11_fsu1")

(define_insn_reservation "hip11_neon_ld4_single" 10
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load4_one_lane,neon_load4_one_lane_q,\
       neon_load4_all_lanes,neon_load4_all_lanes_q"))
  "(hip11_ls1|hip11_ls2)+hip11_fsu1")

(define_insn_reservation "hip11_neon_ld4_multiple" 11
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_load4_4reg,neon_load4_4reg_q"))
  "(hip11_ls1|hip11_ls2)+hip11_fsu1")

;; Stores of up to two words.
(define_insn_reservation "hip11_store1" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "store_4,store_8,store_16,\
       f_stored,f_stores"))
  "hip11_ls1|hip11_ls2")

;; Floating-Point Operations.
(define_insn_reservation "hip11_fp_arith" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "ffariths,ffarithd,f_minmaxs,\
       f_minmaxd,fadds,faddd,neon_fcadd"))
  "hip11_fsu_pipe")

(define_insn_reservation "hip11_fp_mul" 3
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_fp_mul_d,neon_fp_mul_d_q,\
       neon_fp_mul_s_scalar,neon_fp_mul_s_scalar_q,\
       neon_fp_mul_d_scalar_q,fmuld,fmuls"))
  "hip11_fsu_pipe")

(define_insn_reservation "hip11_fp_cmp" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fccmpd,fccmps"))
  "hip11alu,hip11_fsu_pipe")

(define_insn_reservation "hip11_fp_csel" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fcsel"))
  "hip11alu,hip11_fsu1")

(define_insn_reservation "hip11_fp_fcmp" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fcmpd,fcmps"))
  "hip11_fsu_pipe")

(define_insn_reservation "hip11_fp_divs" 7
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fdivs"))
  "hip11_fsu1")

(define_insn_reservation "hip11_fp_divd" 10
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fdivd"))
  "hip11_fsu1")

(define_insn_reservation "hip11_fp_sqrts" 9
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fsqrts"))
  "hip11_fsu1")

(define_insn_reservation "hip11_fp_sqrtd" 15
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fsqrtd"))
  "hip11_fsu1")

(define_insn_reservation "hip11_fp_mac" 4
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fmacs,ffmas,fmacd,ffmad"))
  "hip11_fsu_pipe")

(define_insn_reservation "hip11_fp_mov" 1
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "fmov,neon_dup,neon_dup_q,\
       neon_from_gp,neon_from_gp_q,\
       neon_ins,neon_ins_q,\
       neon_to_gp,neon_to_gp_q,\
       neon_move,neon_move_q,\
       neon_rev,neon_rev_q,\
       neon_permute,neon_permute_q,\
       neon_shift_imm_narrow_q,\
       neon_ext,neon_ext_q,\
       neon_rbit,\
       crypto_sha3,neon_tbl1,neon_tbl1_q,\
       neon_tbl2_q,f_mcr,neon_tst,neon_tst_q,\
       neon_move_narrow_q"))
  "hip11_fsu1")

;; ASIMD instructions
(define_insn_reservation "hip11_asimd_simple_arithmetic" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_abs,neon_abs_q,neon_neg,neon_neg_q,\
       neon_abd,neon_abd_q,\
       neon_add_long,neon_sub_long,neon_sub_widen,neon_add_widen,\
       neon_add_halve_narrow_q,neon_sub_halve_narrow_q,\
       neon_arith_acc,neon_arith_acc_q,\
       neon_compare,neon_compare_q,\
       neon_compare_zero,neon_compare_zero_q,\
       neon_minmax,neon_minmax_q,\
       neon_logic,neon_logic_q,\
       neon_reduc_add,neon_reduc_add_q,\
       neon_reduc_minmax,neon_reduc_minmax_q,\
       neon_fp_to_int_s,neon_fp_to_int_s_q,\
       neon_fp_to_int_d,neon_fp_to_int_d_q,\
       neon_fp_cvt_widen_s,\
       neon_fp_cvt_narrow_d_q,\
       neon_cls,neon_cls_q,\
       neon_cnt,neon_cnt_q,\
       f_rints,f_rintd,f_cvtf2i,f_cvt,\
       neon_tbl3,neon_fp_round_s,neon_fp_round_s_q,\
       neon_fp_round_d,neon_fp_round_d_q,\
       neon_int_to_fp_s,neon_fp_recpe_s,neon_fp_recpe_s_q,\
       neon_fp_recpe_d,neon_fp_recpe_d_q,\
       neon_fp_cvt_narrow_s_q,\
       crypto_aese,crypto_aesmc,\
       crypto_sha1_fast,crypto_sha1_xor,\
       crypto_sha1_slow,\
       crypto_sha256_fast,\
       crypto_sha512,crypto_sm3,\
       neon_qabs,neon_qabs_q,\
       neon_qneg,neon_qneg_q,\
       neon_qadd,neon_qadd_q,\
       neon_qsub,neon_qsub_q,\
       neon_add_halve,neon_add_halve_q,\
       neon_sub_halve,neon_sub_halve_q,\
       neon_fp_reduc_minmax_s,neon_fp_reduc_minmax_s_q,\
       neon_fp_reduc_minmax_d,neon_fp_reduc_minmax_d_q,\
       neon_fp_rsqrte_s,neon_fp_rsqrte_s_q,\
       neon_fp_rsqrte_d,neon_fp_rsqrte_d_q"))
  "hip11_fsu1")

(define_insn_reservation "hip11_asimd_complex_arithmetic" 4
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_mul_b,neon_mul_b_q,\
       neon_mul_h,neon_mul_h_q,\
       neon_mul_s,neon_mul_s_q,\
       neon_mla_b,neon_mla_b_q,\
       neon_mla_h,neon_mla_h_q,\
       neon_mla_s,\
       neon_mla_h_scalar,neon_mla_h_scalar_q,\
       neon_mla_s_scalar,neon_mla_s_scalar_q,\
       neon_sat_mul_h_scalar,neon_sat_mul_h_scalar_q,\
       neon_sat_mul_s_scalar,neon_sat_mul_s_scalar_q,\
       neon_sat_mul_b,neon_sat_mul_b_q,\
       neon_sat_mul_h,neon_sat_mul_h_q,\
       neon_sat_mul_s,neon_sat_mul_s_q,\
       neon_mla_b_long,neon_mla_h_long,neon_mla_s_long,\
       neon_mul_b_long,neon_mul_h_long,neon_mul_s_long,\
       neon_sat_mla_b_long,neon_sat_mla_h_long,neon_sat_mla_s_long,\
       neon_sat_mla_h_scalar_long,neon_sat_mla_s_scalar_long,\
       neon_sat_mul_b_long,neon_sat_mul_h_long,neon_sat_mul_s_long,\
       neon_sat_mul_h_scalar_long,neon_sat_mul_s_scalar_long,\
       crypto_pmull,\
       neon_sat_shift_reg,neon_sat_shift_reg_q,\
       neon_shift_reg,neon_shift_reg_q,\
       neon_shift_imm,neon_shift_imm_q,\
       neon_shift_imm_long,\
       neon_sat_shift_imm,neon_sat_shift_imm_q,\
       neon_sat_shift_imm_narrow_q,\
       neon_shift_acc,neon_shift_acc_q,\
       crypto_sha256_slow"))
  "hip11_fsu1")

(define_insn_reservation "hip11_asimd_fp_compare" 2
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_fp_abs_s,neon_fp_abs_s_q,\
       neon_fp_abs_d,neon_fp_abs_d_q,\
       neon_fp_neg_s,neon_fp_neg_s_q,\
       neon_fp_neg_d,neon_fp_neg_d_q,\
       neon_fp_compare_s,neon_fp_compare_s_q,\
       neon_fp_compare_d,neon_fp_compare_d_q,\
       neon_fp_minmax_s,neon_fp_minmax_s_q,\
       neon_fp_minmax_d,neon_fp_minmax_d_q,\
       neon_fp_addsub_s,neon_fp_addsub_s_q,\
       neon_fp_addsub_d,neon_fp_addsub_d_q,\
       neon_fp_reduc_add_s,neon_fp_reduc_add_s_q,\
       neon_fp_reduc_add_d,neon_fp_reduc_add_d_q,\
       neon_fp_abd_s,neon_fp_abd_s_q,\
       neon_fp_abd_d,neon_fp_abd_d_q"))
  "hip11_fsu_pipe")

(define_insn_reservation "hip11_asimd_fdiv" 10
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_fp_div_s,neon_fp_div_s_q,\
       neon_fp_div_d,neon_fp_div_d_q"))
  "hip11_fsu1")

(define_insn_reservation "hip11_asimd_fsqrt" 15
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_fp_sqrt_s,neon_fp_sqrt_s_q,\
       neon_fp_sqrt_d,neon_fp_sqrt_d_q"))
  "hip11_fsu1")

(define_insn_reservation "hip11_asimd_fp_multiply_add" 4
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_fp_mla_s,neon_fp_mla_s_q,\
       neon_fp_mla_d,neon_fp_mla_d_q,\
       neon_fp_mla_s_scalar,neon_fp_mla_s_scalar_q,\
       neon_fp_mul_s,neon_fp_mul_s_q,neon_fcmla,\
       neon_fp_recps_s,neon_fp_recps_s_q,\
       neon_fp_recps_d,neon_fp_recps_d_q,\
       neon_fp_rsqrts_s,neon_fp_rsqrts_s_q,\
       neon_fp_rsqrts_d,neon_fp_rsqrts_d_q"))
  "hip11_fsu_pipe")

(define_insn_reservation "hip11_asimd_frecpx" 3
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_fp_recpx_s,neon_fp_recpx_s_q,\
       neon_fp_recpx_d,neon_fp_recpx_d_q,neon_tbl4,\
       neon_dot,neon_dot_q"))
  "hip11_fsu1")

(define_insn_reservation "hip11_asimd_mmla" 6
  (and (eq_attr "tune" "hip11")
       (eq_attr "type" "neon_mla_s_q"))
  "hip11_fsu1")
