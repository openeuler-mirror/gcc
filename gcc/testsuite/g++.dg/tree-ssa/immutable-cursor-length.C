/* { dg-do run } */
/* { dg-options "-O3 -fchecking=2 -ftree-immutable-cursor-specialization" } */
#include <string>
__attribute__((noinline,noipa)) bool equal(const std::string &s)
{ return s=="a"; }
__attribute__((noinline,noipa)) int scan(const std::string &s, unsigned bit)
{
  const char *p="a:b";
  int result=0;
  while (*p) {
    if (*p==':') { ++p; continue; }
    if (s.size()==1 && s[0]==*p) result+=bit ? 7 : 3;
    ++p;
  }
  return result;
}
int main()
{
  std::string embedded("a\0b",3), one("a"), empty, longstr(200,'a');
  if(equal(embedded)||equal(empty)||equal(longstr)||!equal(one)) __builtin_abort();
  if(scan(embedded,1)||scan(empty,0)||scan(longstr,1)||scan(one,1)!=7) __builtin_abort();
}
