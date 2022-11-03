/* check_ptr_layers bugfix.*/
/* { dg-do compile } */
struct {
  char a;
} **b = 0, *e = 0;
long c;
char d = 9;
int f;

void g()
{
  for (; f;)
    if (c)
      (*e).a++;
  if (!d)
    for (;;)
      b &&c;
}
int
main()
{ 
  g();
}
/* { dg-final { scan-ipa-dump "No structures to transform." "struct_reorg" } } */