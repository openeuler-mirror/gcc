/* { dg-lto-do link }  */
/* { dg-require-effective-target shared } */
/* { dg-extra-ld-options {-shared -finline-force=c_lto_binary-inline-2_1.o} } */
/* { dg-lto-options {{-O3 -flto -fPIC -fdump-ipa-inline-details}} }  */ 

extern double multi_op(float x);

double func_a (float x)
{
  double res = 0;
  res = multi_op (x);
  return res;
}

/* { dg-final { scan-wpa-ipa-dump "Inlined 1 calls"  "inline"  } } */
