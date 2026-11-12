/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -g -flto -fgnu89-inline -fipa-value-specialization -fipa-value-specialization-cold" } */

/* Keep the checked value live only up to the call, as in the original
   regression.  Cold specialization must preserve its debug availability.  */
#include "pr41616-1.c"
