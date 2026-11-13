/* { dg-do run } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O3 -flto -fipa-cp-clone -fpeel-loops -fipa-value-specialization --param ipa-cp-eval-threshold=0 --param ipa-value-specialization-peel-growth-budget=4096 --param ipa-value-specialization-peel-growth-percent=0" } */

#include "readonly-peel-growth-run.c"
