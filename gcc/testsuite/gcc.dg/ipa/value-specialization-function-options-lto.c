/* { dg-do run } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fno-peel-loops --param ipa-cp-eval-threshold=0 -flto -flto-partition=max -fdump-ipa-cp-details -fno-ipa-value-specialization" } */
/* The signed reference sum needs at least 32-bit int.  */
/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target lto } */
#define INTERPRET_ATTRIBUTES __attribute__((noinline, optimize ("ipa-value-specialization")))
#include "value-specialization-function-options-run.h"
/* { dg-final { scan-wpa-ipa-dump-times "Creating a specialized node of interpret" 17 "cp" } } */
