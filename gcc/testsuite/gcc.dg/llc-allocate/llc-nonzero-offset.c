/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -c -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param filter-kernels=0" } */

#include <stdio.h>

typedef struct stack_def
{
  int top;                      /* index to top stack element */
  unsigned long reg_set;        /* set of live registers */
  unsigned char reg[128];       /* register - stack mapping */
} *stack;

typedef struct block_info_def
{
  struct stack_def stack_in;    /* Input stack configuration.  */
  struct stack_def stack_out;   /* Output stack configuration.  */
  unsigned long out_reg_set;    /* Stack regs live on output.  */
  int done;                     /* True if block already converted.  */
  int predecessors;             /* Number of predecessors that need
                                   to be visited.  */
} *block_info;

typedef struct basic_block_def
{
  void *aux;
} *basic_block;

unsigned char
convert_regs_exit (basic_block bb, int value_reg_low, int value_reg_high)
{
  stack output_stack;

  output_stack = &(((block_info) bb->aux)->stack_in);
  if (value_reg_low == -1)
    output_stack->top = -1;
  else
    {
      int reg;
      output_stack->top = value_reg_high - value_reg_low;
      for (reg = value_reg_low; reg <= value_reg_high; ++reg)
        {
          (output_stack->reg + 16)[value_reg_high - reg] = reg;
          output_stack->reg_set |= (unsigned long) 1 << reg;
        }
    }
  return output_stack->reg[0];
}

/* { dg-final { scan-tree-dump-times "runtime issue" 1 "llc_allocate" } } */
/* { dg-final { scan-tree-dump-times "static issue" 1 "llc_allocate" } } */
