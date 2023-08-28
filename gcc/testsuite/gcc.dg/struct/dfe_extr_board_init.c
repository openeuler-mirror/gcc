/* { dg-do compile} */

#define NULL ((void*)0)
typedef unsigned long size_t;
typedef long intptr_t;
typedef unsigned long uintptr_t;
typedef long scalar_t__;
typedef int bool;
#define false 0
#define true 1

typedef struct TYPE_5__ TYPE_2__;
typedef struct TYPE_4__ TYPE_1__;

struct TYPE_4__
{
  int Pin;
  int Pull;
  int Mode;
  int Speed;
};

struct TYPE_5__
{
  int MEMRMP;
};
typedef TYPE_1__ GPIO_InitTypeDef;

int BT_RST_PIN;
int BT_RST_PORT;
int CONN_POS10_PIN;
int CONN_POS10_PORT;
int GPIO_HIGH (int, int);
int GPIO_MODE_INPUT;
int GPIO_MODE_OUTPUT_PP;
int GPIO_NOPULL;
int GPIO_PULLUP;
int GPIO_SPEED_FREQ_LOW;
int HAL_GPIO_Init (int, TYPE_1__ *);
scalar_t__ IS_GPIO_RESET (int, int);
TYPE_2__ *SYSCFG;
int __HAL_RCC_GPIOB_CLK_ENABLE ();
int __HAL_RCC_GPIOC_CLK_ENABLE ();

__attribute__((used)) static void 
LBF_DFU_If_Needed (void)
{
  GPIO_InitTypeDef GPIO_InitStruct;
  __HAL_RCC_GPIOC_CLK_ENABLE ();
  GPIO_InitStruct.Mode = GPIO_MODE_OUTPUT_PP;
  GPIO_InitStruct.Pull = GPIO_NOPULL;
  GPIO_InitStruct.Speed = GPIO_SPEED_FREQ_LOW;
  GPIO_InitStruct.Pin = BT_RST_PIN;
  HAL_GPIO_Init (BT_RST_PORT, &GPIO_InitStruct);

  GPIO_HIGH (BT_RST_PORT, BT_RST_PIN);
  __HAL_RCC_GPIOB_CLK_ENABLE ();
  GPIO_InitStruct.Mode = GPIO_MODE_INPUT;
  GPIO_InitStruct.Pull = GPIO_PULLUP;
  GPIO_InitStruct.Pin = CONN_POS10_PIN;
  HAL_GPIO_Init (CONN_POS10_PORT, &GPIO_InitStruct);

  if (IS_GPIO_RESET (CONN_POS10_PORT, CONN_POS10_PIN))
    {
      SYSCFG->MEMRMP = 0x00000001;
      asm (
	"LDR     R0, =0x000000\n\t"
	"LDR     SP, [R0, #0]\n\t"
	  );
      asm (
	"LDR     R0, [R0, #0]\n\t"
	"BX       R0\n\t"
	  );
    }
}

/* { dg-final { scan-ipa-dump-times "Dead field elimination" 0 "reorder_fields" } } */
