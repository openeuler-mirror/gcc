/* { dg-do run } */
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
int next_i() {
  int counter = 0;
  return rand_primes[(++counter)%JOTAI_NUM_RANDS_];
}
typedef unsigned long size_t;  // Customize by platform.
typedef long intptr_t; typedef unsigned long uintptr_t;
typedef long scalar_t__;  // Either arithmetic or pointer type.
typedef int bool;
#define false 0
#define true 1
typedef  struct TYPE_2__   TYPE_1__;
struct pci_dev {int devfn; TYPE_1__* sriov; int /*<<< orphan*/  is_physfn; };
struct TYPE_2__ {int offset; int stride; } ;
int EINVAL ;
int pci_iov_virtfn_devfn(struct pci_dev *dev, int vf_id)
{
  if (!dev->is_physfn)
    return -EINVAL;
  return (dev->devfn + dev->sriov->offset +
    dev->sriov->stride * vf_id) & 0xff;
}
int main(int argc, char *argv[]) {
  int opt = 1;
  switch(opt) {
  case 0:
  {
    int vf_id = 100;
    int _len_dev0 = 1;
    struct pci_dev * dev =
      (struct pci_dev *) malloc(_len_dev0*sizeof(struct pci_dev));
    for(int _i0 = 0; _i0 < _len_dev0; _i0++) {
      dev[_i0].devfn = ((-2 * (next_i()%2)) + 1) * next_i();
      int _len_dev__i0__sriov0 = 1;
      dev[_i0].sriov =
      (struct TYPE_2__ *) malloc(_len_dev__i0__sriov0*sizeof(struct TYPE_2__));
      for(int _j0 = 0; _j0 < _len_dev__i0__sriov0; _j0++) {
	dev[_i0].sriov->offset = ((-2 * (next_i()%2)) + 1) * next_i();
	dev[_i0].sriov->stride = ((-2 * (next_i()%2)) + 1) * next_i();
      }
      dev[_i0].is_physfn = ((-2 * (next_i()%2)) + 1) * next_i();
    }
    int benchRet = pci_iov_virtfn_devfn(dev,vf_id);
    printf("%d\n", benchRet); 
    for(int _aux = 0; _aux < _len_dev0; _aux++) {
      free(dev[_aux].sriov);
    }
    free(dev);
    break;
  }
  case 1:
  {
    int vf_id = 255;
    int _len_dev0 = 65025;
    struct pci_dev * dev = (struct pci_dev *) malloc(_len_dev0*sizeof(struct pci_dev));
    for(int _i0 = 0; _i0 < _len_dev0; _i0++) {
      dev[_i0].devfn = ((-2 * (next_i()%2)) + 1) * next_i();
      int _len_dev__i0__sriov0 = 1;
      dev[_i0].sriov = (struct TYPE_2__ *) malloc(_len_dev__i0__sriov0*sizeof(struct TYPE_2__));
      for(int _j0 = 0; _j0 < _len_dev__i0__sriov0; _j0++) {
	dev[_i0].sriov->offset = ((-2 * (next_i()%2)) + 1) * next_i();
      dev[_i0].sriov->stride = ((-2 * (next_i()%2)) + 1) * next_i();
      }
      dev[_i0].is_physfn = ((-2 * (next_i()%2)) + 1) * next_i();
    }
    int benchRet = pci_iov_virtfn_devfn(dev,vf_id);
    printf("%d\n", benchRet); 
    for(int _aux = 0; _aux < _len_dev0; _aux++) {
      free(dev[_aux].sriov);
    }
    free(dev);
    break;
  }
  case 2:
  {
    int vf_id = 10;
    int _len_dev0 = 100;
    struct pci_dev * dev = (struct pci_dev *) malloc(_len_dev0*sizeof(struct pci_dev));
    for(int _i0 = 0; _i0 < _len_dev0; _i0++) {
      dev[_i0].devfn = ((-2 * (next_i()%2)) + 1) * next_i();
      int _len_dev__i0__sriov0 = 1;
      dev[_i0].sriov = (struct TYPE_2__ *) malloc(_len_dev__i0__sriov0*sizeof(struct TYPE_2__));
      for(int _j0 = 0; _j0 < _len_dev__i0__sriov0; _j0++) {
	dev[_i0].sriov->offset = ((-2 * (next_i()%2)) + 1) * next_i();
	dev[_i0].sriov->stride = ((-2 * (next_i()%2)) + 1) * next_i();
      }
      dev[_i0].is_physfn = ((-2 * (next_i()%2)) + 1) * next_i();
    }
    int benchRet = pci_iov_virtfn_devfn(dev,vf_id);
    printf("%d\n", benchRet); 
    for(int _aux = 0; _aux < _len_dev0; _aux++) {
      free(dev[_aux].sriov);
    }
    free(dev);
    break;
  }
  case 3:
  {
    int vf_id = ((-2 * (next_i()%2)) + 1) * next_i();
    int _len_dev0 = 1;
    struct pci_dev * dev = (struct pci_dev *) malloc(_len_dev0*sizeof(struct pci_dev));
    for(int _i0 = 0; _i0 < _len_dev0; _i0++) {
      dev[_i0].devfn = ((-2 * (next_i()%2)) + 1) * next_i();
      int _len_dev__i0__sriov0 = 1;
      dev[_i0].sriov = (struct TYPE_2__ *) malloc(_len_dev__i0__sriov0*sizeof(struct TYPE_2__));
      for(int _j0 = 0; _j0 < _len_dev__i0__sriov0; _j0++) {
	dev[_i0].sriov->offset = ((-2 * (next_i()%2)) + 1) * next_i();
	dev[_i0].sriov->stride = ((-2 * (next_i()%2)) + 1) * next_i();
      }
      dev[_i0].is_physfn = ((-2 * (next_i()%2)) + 1) * next_i();
    }
    int benchRet = pci_iov_virtfn_devfn(dev,vf_id);
    printf("%d\n", benchRet); 
    for(int _aux = 0; _aux < _len_dev0; _aux++) {
      free(dev[_aux].sriov);
    }
    free(dev);
    break;
  }
  default:
    break;
  }
  return 0;
}