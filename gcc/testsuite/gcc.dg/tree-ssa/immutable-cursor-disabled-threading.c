/* The generic path registry requires ordinary threading to be enabled.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-thread-jumps -ftree-immutable-loop-specialization-cursor -fchecking=2" } */
#include "immutable-cursor-1.c"
