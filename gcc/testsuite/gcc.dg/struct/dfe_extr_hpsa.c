/* { dg-do compile} */

#define NULL ((void*)0)
typedef unsigned long size_t;
typedef long intptr_t;
typedef unsigned long uintptr_t;
typedef long scalar_t__;
typedef int bool;
#define false 0
#define true 1

typedef struct TYPE_6__ TYPE_3__;
typedef struct TYPE_5__ TYPE_2__;
typedef struct TYPE_4__ TYPE_1__;

struct io_accel2_cmd
{
  int dummy;
};

struct hpsa_tmf_struct
{
  int it_nexus;
};

struct hpsa_scsi_dev_t
{
  int nphysical_disks;
  int ioaccel_handle;
  struct hpsa_scsi_dev_t **phys_disk;
};

struct ctlr_info
{
  TYPE_3__ *pdev;
  struct io_accel2_cmd *ioaccel2_cmd_pool;
};
struct TYPE_4__
{
  int LunAddrBytes;
};

struct TYPE_5__
{
  TYPE_1__ LUN;
};

struct CommandList
{
  size_t cmdindex;
  int cmd_type;
  struct hpsa_scsi_dev_t *phys_disk;
  TYPE_2__ Header;
};

struct TYPE_6__
{
  int dev;
};

int BUG ();
#define CMD_IOACCEL1 132
#define CMD_IOACCEL2 131
#define CMD_IOCTL_PEND 130
#define CMD_SCSI 129
#define IOACCEL2_TMF 128
int dev_err (int *, char *, int);
scalar_t__ hpsa_is_cmd_idle (struct CommandList *);
int le32_to_cpu (int);
int test_memcmp (unsigned char *, int *, int);

__attribute__((used)) static bool
hpsa_cmd_dev_match (struct ctlr_info *h, struct CommandList *c,
		    struct hpsa_scsi_dev_t *dev, unsigned char *scsi3addr)
{
  int i;
  bool match = false;
  struct io_accel2_cmd * c2 = &h->ioaccel2_cmd_pool[c->cmdindex];
  struct hpsa_tmf_struct *ac = (struct hpsa_tmf_struct *)c2;

  if (hpsa_is_cmd_idle (c))
    return false;

  switch (c->cmd_type)
    {
      case CMD_SCSI:
      case CMD_IOCTL_PEND:
	match = !test_memcmp (scsi3addr, &c->Header.LUN.LunAddrBytes,
			      sizeof (c->Header.LUN.LunAddrBytes));
	break;

      case CMD_IOACCEL1:
      case CMD_IOACCEL2:
	if (c->phys_disk == dev)
	  {
	    match = true;
	  }
	else
	  {
	    for (i = 0; i < dev->nphysical_disks && !match; i++)
	      {
		match = dev->phys_disk[i] == c->phys_disk;
	      }
	  }
	break;

      case IOACCEL2_TMF:
	for (i = 0; i < dev->nphysical_disks && !match; i++)
	  {
	    match = dev->phys_disk[i]->ioaccel_handle == 
		    le32_to_cpu (ac->it_nexus);
	  }
	break;

      case 0:
	match = false;
	break;
      default:
	dev_err (&h->pdev->dev, "unexpected cmd_type: %d\n", c->cmd_type);
	BUG ();
    }

  return match;
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 0 "reorder_fields" } } */
