/* { dg-module-do link } */
/* { dg-options "-std=c++11 -fpatchable-function-entry=1 -O2" } */
/* { dg-additional-options "-std=c++11 -fpatchable-function-entry=1 -O2" } */

/* This test is in the "modules" package because it supports multiple files
   linkage.  */

#include "pr105169.h"

WinsockInterfaceClass::WinsockInterfaceClass(void)
{
}
