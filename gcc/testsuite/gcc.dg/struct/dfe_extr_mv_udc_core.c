/* { dg-do compile} */

#define NULL ((void*)0)
typedef unsigned long size_t;
typedef long intptr_t;
typedef unsigned long uintptr_t;
typedef long scalar_t__;
typedef int bool;
#define false 0
#define true 1

typedef struct TYPE_4__ TYPE_2__;
typedef struct TYPE_3__ TYPE_1__;
typedef int u32;

struct mv_udc
{
  TYPE_2__ *op_regs;
  TYPE_1__ *ep_dqh;
  struct mv_ep *eps;
};

struct mv_ep
{
  TYPE_1__ *dqh;
  struct mv_udc *udc;
};

struct TYPE_4__
{
  int *epctrlx;
};

struct TYPE_3__
{
  int max_packet_length;
  int next_dtd_ptr;
};

int EP0_MAX_PKT_SIZE;
int EPCTRL_RX_ENABLE;
int EPCTRL_RX_EP_TYPE_SHIFT;
int EPCTRL_TX_ENABLE;
int EPCTRL_TX_EP_TYPE_SHIFT;
int EP_QUEUE_HEAD_IOS;
int EP_QUEUE_HEAD_MAX_PKT_LEN_POS;
int EP_QUEUE_HEAD_NEXT_TERMINATE;
int USB_ENDPOINT_XFER_CONTROL;
int readl (int *);
int writel (int, int *);

__attribute__((used)) static void
ep0_reset (struct mv_udc *udc)
{
  struct mv_ep *ep;
  u32 epctrlx;
  int i = 0;
  for (i = 0; i < 2; i++)
    {
      ep = &udc->eps[i];
      ep->udc = udc;
      ep->dqh = &udc->ep_dqh[i];
      ep->dqh->max_packet_length =
	(EP0_MAX_PKT_SIZE << EP_QUEUE_HEAD_MAX_PKT_LEN_POS)
	| EP_QUEUE_HEAD_IOS;
      ep->dqh->next_dtd_ptr = EP_QUEUE_HEAD_NEXT_TERMINATE;
      epctrlx = readl (&udc->op_regs->epctrlx[0]);
      if (i)
	{
	  epctrlx |= EPCTRL_TX_ENABLE
		  | (USB_ENDPOINT_XFER_CONTROL << EPCTRL_TX_EP_TYPE_SHIFT);
	}
      else
	{
	  epctrlx |= EPCTRL_RX_ENABLE
		  | (USB_ENDPOINT_XFER_CONTROL << EPCTRL_RX_EP_TYPE_SHIFT);
	}
      writel (epctrlx, &udc->op_regs->epctrlx[0]);
    }
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 2 "struct_reorg" } } */
