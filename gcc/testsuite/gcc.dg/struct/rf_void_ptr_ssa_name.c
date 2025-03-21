// Add a void* ssa_name check and escape.
/* { dg-do compile } */

// includes
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "string.h"
#include "limits.h"
#include "float.h"

#define JOTAI_NUM_RANDS_ 25

const unsigned rand_primes[JOTAI_NUM_RANDS_] = {179, 103, 479, 647, 229, 37,
271, 557, 263, 607, 18743, 50359, 21929, 48757, 98179, 12907, 52937, 64579,
49957, 52567, 507163, 149939, 412157, 680861, 757751};

int next_i ()
{
  int counter = 0;
  return rand_primes[(++counter)%JOTAI_NUM_RANDS_];
}

float next_f ()
{
  int counter = 0;
  return rand_primes[(++counter)%JOTAI_NUM_RANDS_] / 757751.0F;
}

// Usage menu
void usage()
{
    printf("%s", "Usage:\n\
    prog [ARGS]\n\
\nARGS:\n\
       0	    big-arr\n\
       1	    big-arr-10x\n\
       2	    empty\n\
\n\
");
}

// ------------------------------------------------------------------------- //

typedef unsigned long size_t;  // Customize by platform.
typedef long intptr_t;
typedef unsigned long uintptr_t;
typedef long scalar_t__;  // Either arithmetic or pointer type.
/* By default, we understand bool (as a convenience). */
typedef int bool;
#define false 0
#define true 1

/* Forward declarations */

/* Type definitions */
typedef  size_t u32 ;
struct octeon_device {int octeon_id; } ;

/* Variables and functions */
 size_t MAX_OCTEON_DEVICES ; 
 struct octeon_device** octeon_device ; 

int lio_get_device_id(void *dev)
{
  struct octeon_device *octeon_dev = (struct octeon_device *)dev;
  u32 i;

  for (i = 0; i < MAX_OCTEON_DEVICES; i++)
    {
      if (octeon_device[i] == octeon_dev)
	return octeon_dev->octeon_id;
    }
  return -1;
}

// ------------------------------------------------------------------------- //

int main(int argc, char *argv[])
{
  if (argc != 2)
    {
      usage();
      return 1;
    }

  int opt = atoi(argv[1]);
  switch(opt)
    {
      // big-arr
      case 0:
	{
	  void * dev;
	  int benchRet = lio_get_device_id(dev);
	  printf("%d\n", benchRet); 
	  break;
	}

      // big-arr-10x
      case 1:
	{
	  void * dev;
	  int benchRet = lio_get_device_id(dev);
	  printf("%d\n", benchRet); 
	  break;
	}

      // empty
      case 2:
	{
	  void * dev;
	  int benchRet = lio_get_device_id(dev);
	  printf("%d\n", benchRet); 
	  break;
	}

      default:
	usage();
	break;
    }

  return 0;
}

/* { dg-final { scan-ipa-dump "No structures to transform" "struct_reorg" } } */
