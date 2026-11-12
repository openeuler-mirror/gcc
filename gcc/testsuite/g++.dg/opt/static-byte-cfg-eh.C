/* { dg-do compile } */
/* { dg-require-effective-target exceptions } */
/* { dg-options "-O2 -fexceptions -ftree-static-byte-cfg -fchecking=2 -fdump-tree-static-byte-cfg-details" } */

extern unsigned may_throw (unsigned) __attribute__ ((pure));

__attribute__ ((noipa)) unsigned
eh_scan (unsigned key)
{
  const unsigned char *p = (const unsigned char *) "bounded bytes";
  unsigned sum = key;
  try
    {
      while (*p)
	sum += may_throw (*p++ ^ key);
    }
  catch (...)
    {
      return 99;
    }
  return sum;
}

/* { dg-final { scan-tree-dump "reason=(effects-eh-or-unsupported|abnormal-eh-edge|nonduplicable-block)" "static-byte-cfg" } } */
/* { dg-final { scan-tree-dump-not "RESIDUAL-EMIT" "static-byte-cfg" } } */
