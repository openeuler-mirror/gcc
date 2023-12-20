/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O1 -fsplit-ldp-stp" } */
/*
 *    Test is:
 *        Pattern where LDP insns should be split with rearrangement in order
 *        to deal with data dependecy betwen subinstruction.  
 *                                                                          */

int __RTL (startwith ("split_complex_instructions"))
simple_ldp_after_store ()
{
(function "ldp_equal_registers"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 228 (set (reg/i:DI x1) 
                   (reg/i:DI x0)))
      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI x1)
                          (const_int 8))[1 S4 A32])(reg:DI x0)))
      (cinsn 10 (parallel [
        (set (reg:DI x1)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int 8)) [1 S4 A32]))
        (set (reg:DI x2)
          (mem:DI (plus:DI (reg/f:DI x1)
            (const_int 16)) [1 S4 A32]))]))
      (cinsn 11 (use (reg/i:DI sp)))
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 13 (use (reg/i:DI x0)))
      (cinsn 14 (use (reg/i:DI x1)))
      (cinsn 15 (use (reg/i:DI x2)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "ldp_equal_registers"
}

/* Verify that the output code rearrange ldrs.  */
/* { dg-final { scan-assembler-times ".*ldr.*x2.*x1,.*16.*ldr.*x1.*x1.*8" 1 } }  */