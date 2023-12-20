/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O1 -fsplit-ldp-stp" } */
/*
 *    Tests are:
 *          Patterns where LDP insns should NOT be split
 *                       */

int __RTL (startwith ("split_complex_instructions"))
simple_ldp_after_store ()
{
(function "simple_ldp_after_store"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 228 (set (reg/i:DI sp) 
                   (reg/i:DI x0)))
      (cinsn 238 (set (reg/i:DI x1)
                   (reg/i:DI x0)))

      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI sp)
                          (const_int 32))[1 S4 A32])(reg:DI x0)))
      (cinsn 10 (parallel [
        (set (reg:DI x29)
          (mem:DI (plus:DI (reg/f:DI sp) (const_int 8)) [1 S4 A32]))
        (set (reg:DI x30)
          (mem:DI (plus:DI (reg/f:DI sp)
            (const_int 16)) [1 S4 A32]))]))
      (cinsn 11 (use (reg/i:DI x29)))
      (cinsn 12 (use (reg/i:DI x30)))

      /* stp x0, x2, [x1].  */
      (cinsn 102 (parallel [
        (set (mem:DI (reg/f:DI x1) [1 S4 A32])
             (reg:DI x0))
        (set (mem:DI (plus:DI (reg/f:DI x1) (const_int 8)) [1 S4 A32])
             (reg:DI x2))]))
      /* ldp x5, x6, [x1].  */
      (cinsn 13 (parallel [
        (set (reg:DI x5) (mem:DI (reg/f:DI x1) [1 S4 A32]))
        (set (reg:DI x6) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 8)) [1 S4 A32]))
      ]))
      (cinsn 14 (use (reg/i:DI x5)))
      (cinsn 15 (use (reg/i:DI x6)))

      (cinsn 100 (use (reg/i:DI sp)))
      (cinsn 200 (use (reg/i:DI cc)))
      (cinsn 300 (use (reg/i:DI x0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "simple_ldp_after_store"
}

int __RTL (startwith ("split_complex_instructions"))
ldp_after_store_in_different_bb ()
{
(function "ldp_after_store_in_different_bb"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 228 (set (reg/i:DI sp) 
                   (reg/i:DI x0)))
      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI sp)
                          (const_int 32))[1 S4 A32])(reg:DI x0)))
      (edge-to 3 (flags "FALLTHRU"))
    ) ;; block 2
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 4 [bb 3] NOTE_INSN_BASIC_BLOCK)
      (cinsn 10 (parallel [
        (set (reg:DI x29)
          (mem:DI (plus:DI (reg/f:DI sp) (const_int 8)) [1 S4 A32]))
        (set (reg:DI x30)
          (mem:DI (plus:DI (reg/f:DI sp)
            (const_int 16)) [1 S4 A32]))]))
      (cinsn 11 (use (reg/i:DI sp)))
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 13 (use (reg/i:DI x29)))
      (cinsn 14 (use (reg/i:DI x30)))
      (cinsn 15 (use (reg/i:DI x0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 3
  ) ;; insn-chain
) ;; function "ldp_after_store_in_different_bb"
}

/* Verify that the output code contains exactly 3 ldp.  */
/* { dg-final { scan-assembler-times {ldp\t} 3 } }  */