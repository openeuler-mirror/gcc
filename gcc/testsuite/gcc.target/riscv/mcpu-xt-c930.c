/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xt-c930 -mabi=lp64d" } */
/* xt-c930 = rv64imafdcbvh_zic64b_zicbom_zicbop_zicboz_ziccamoa_ziccif_zicclsm_ziccrse_zicfilp_zicfiss_zicntr_zicond_zicsr_zifencei_zihintntl_zihintpause_zihpm_zimop_zmmul_za64rs_zaamo_zabha_zacas_zalrsc_zawrs_zfa_zfbfmin_zfh_zfhmin_zca_zcb_zcd_zba_zbb_zbc_zbs_zkr_zkt_zvbb_zvbc_zve32f_zve32x_zve64d_zve64f_zve64x_zvfbfmin_zvfbfwma_zvfh_zvfhmin_zvkb_zvkg_zvkn_zvknc_zvkned_zvkng_zvknha_zvknhb_zvks_zvksc_zvksed_zvksg_zvksh_zvkt_zvl128b_zvl256b_zvl32b_zvl64b_smaia_smepmp_ssaia_sscofpmf_ssstateen_sstc_svinval_svnapot_svpbmt -mabi=lp64d */

#if !((__riscv_xlen == 64)			\
      && defined(__riscv_float_abi_double)	\
      && defined(__riscv_mul)			\
      && defined(__riscv_atomic)		\
      && defined(__riscv_compressed)		\
      && defined(__riscv_flen)			\
      && defined(__riscv_vector)		\
      && __riscv_v_min_vlen == 256 		\
      && defined(__riscv_zic64b)		\
      && defined(__riscv_zicbom)		\
      && defined(__riscv_zicbop)		\
      && defined(__riscv_zicboz)		\
      && defined(__riscv_ziccamoa)		\
      && defined(__riscv_ziccif)		\
      && defined(__riscv_zicclsm)		\
      && defined(__riscv_ziccrse)		\
      && defined(__riscv_zicfilp)		\
      && defined(__riscv_zicfiss)		\
      && defined(__riscv_zicntr)		\
      && defined(__riscv_zicond)		\
      && defined(__riscv_zicsr)			\
      && defined(__riscv_zifencei)		\
      && defined(__riscv_zihintntl)		\
      && defined(__riscv_zihintpause)		\
      && defined(__riscv_zihpm)			\
      && defined(__riscv_zimop)			\
      && defined(__riscv_zmmul)			\
      && defined(__riscv_za64rs)		\
      && defined(__riscv_zaamo)			\
      && defined(__riscv_zabha)			\
      && defined(__riscv_zacas)			\
      && defined(__riscv_zalrsc)		\
      && defined(__riscv_zawrs)			\
      && defined(__riscv_zfa)			\
      && defined(__riscv_zfbfmin)		\
      && defined(__riscv_zfh)			\
      && defined(__riscv_zfhmin)		\
      && defined(__riscv_zca)			\
      && defined(__riscv_zcb)			\
      && defined(__riscv_zcd)			\
      && defined(__riscv_zba)			\
      && defined(__riscv_zbb)			\
      && defined(__riscv_zbc)			\
      && defined(__riscv_zbs)			\
      && defined(__riscv_zkr)			\
      && defined(__riscv_zkt)			\
      && defined(__riscv_zvbb)			\
      && defined(__riscv_zvbc)			\
      && defined(__riscv_zve32f)		\
      && defined(__riscv_zve32x)		\
      && defined(__riscv_zve64d)		\
      && defined(__riscv_zve64f)		\
      && defined(__riscv_zve64x)		\
      && defined(__riscv_zvfbfmin)		\
      && defined(__riscv_zvfbfwma)		\
      && defined(__riscv_zvfh)			\
      && defined(__riscv_zvfhmin)		\
      && defined(__riscv_zvkb)			\
      && defined(__riscv_zvkg)			\
      && defined(__riscv_zvkn)			\
      && defined(__riscv_zvknc)			\
      && defined(__riscv_zvkned)		\
      && defined(__riscv_zvkng)			\
      && defined(__riscv_zvknha)		\
      && defined(__riscv_zvknhb)		\
      && defined(__riscv_zvks)			\
      && defined(__riscv_zvksc)			\
      && defined(__riscv_zvksed)		\
      && defined(__riscv_zvksg)			\
      && defined(__riscv_zvksh)			\
      && defined(__riscv_zvkt)			\
      && defined(__riscv_zvl128b)		\
      && defined(__riscv_zvl256b)		\
      && defined(__riscv_zvl32b)		\
      && defined(__riscv_zvl64b)		\
      && defined(__riscv_smaia)			\
      && defined(__riscv_smepmp)		\
      && defined(__riscv_ssaia)			\
      && defined(__riscv_sscofpmf)		\
      && defined(__riscv_ssstateen)		\
      && defined(__riscv_sstc)			\
      && defined(__riscv_svinval)		\
      && defined(__riscv_svnapot)		\
      && defined(__riscv_svpbmt))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
