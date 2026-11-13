/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O3 -fchecking=2 -fipa-cp-clone -fipa-value-specialization -fno-peel-loops --param ipa-cp-eval-threshold=0 -fdump-ipa-cp-details -fno-ipa-value-specialization -flto -flto-partition=one" } */
#include "readonly-constructor.h"
/* { dg-final { scan-wpa-ipa-dump-not "immutable object size:" "cp" } } */
/* { dg-final { scan-wpa-ipa-dump-not "Adjusting immutable value benefit" "cp" } } */
