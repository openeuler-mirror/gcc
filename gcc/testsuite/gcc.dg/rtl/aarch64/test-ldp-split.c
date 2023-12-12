/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O1 -fsplit-ldp-stp" } */
/*
 *    Tests are:
 *          Patterns where LDP insns should be split
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
                          (const_int 8))[1 S4 A32])(reg:DI x0)))
      (cinsn 10 (parallel [
        (set (reg:DI x29)
          (mem:DI (plus:DI (reg/f:DI sp) (const_int 8)) [1 S4 A32]))
        (set (reg:DI x30)
          (mem:DI (plus:DI (reg/f:DI sp)
            (const_int 16)) [1 S4 A32]))]))

      (cinsn 102 (set (mem/c:DI (plus:DI (reg/f:DI x1)
                                          (const_int -16)) [1 S4 A32])
                      (reg:DI x0)))
      (cinsn 11 (parallel [
        (set (reg:DI x3)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int -16)) [1 S4 A32]))
        (set (reg:DI x4)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int -8)) [1 S4 A32]))
      ]))

      (cinsn 103 (set (mem/c:DI (reg/f:DI x1) [1 S4 A32])
                      (reg:DI x0)))
      (cinsn 12 (parallel [
        (set (reg:DI x5) (mem:DI (reg/f:DI x1) [1 S4 A32]))
        (set (reg:DI x6) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 8)) [1 S4 A32]))
      ]))

      (cinsn 13 (use (reg/i:DI sp)))
      (cinsn 14 (use (reg/i:DI cc)))
      (cinsn 15 (use (reg/i:DI x29)))
      (cinsn 16 (use (reg/i:DI x30)))
      (cinsn 17 (use (reg/i:DI x0)))
      (cinsn 18 (use (reg/i:DI x3)))
      (cinsn 19 (use (reg/i:DI x4)))
      (cinsn 20 (use (reg/i:DI x5)))
      (cinsn 21 (use (reg/i:DI x6)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "simple_ldp_after_store"
}

int __RTL (startwith ("split_complex_instructions"))
ldp_ti_after_store ()
{
  (function "ldp_ti_after_store"
    (insn-chain
      (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 228 (set (reg/i:DI sp)
                   (reg/i:DI x0)))
      (cinsn 238 (set (reg/i:DI x2)
                   (reg/i:DI x0)))

      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI sp)
                          (const_int 136))[1 S4 A32])(reg:DI x0)))
      (insn 81 (set (reg:TI x0 [1 S4 A32])
              (mem/c:TI (plus:DI (reg/f:DI sp)
                      (const_int 136 )) [1 S4 A32]))
           (expr_list:REG_EQUIV (mem/c:TI (plus:DI (reg/f:DI sfp)
                      (const_int -24 )) [1 S4 A32])
              (nil)))

      (cinsn 102 (set (mem/c:DI (plus:DI (reg/f:DI x2)
                                          (const_int -16)) [1 S4 A32])
                      (reg:DI x0)))
      (insn 82 (set (reg:TI x3 [1 S4 A32])
                    (mem/c:TI (plus:DI (reg/f:DI x2)
                                        (const_int -16)) [1 S4 A32])))

      (cinsn 103 (set (mem/c:DI (reg/f:DI x2) [1 S4 A32])
                      (reg:DI x0)))
      (insn 83 (set (reg:TI x5 [1 S4 A32])
                    (mem/c:TI (reg/f:DI x2) [1 S4 A32])))

      (cinsn 11 (use (reg/i:DI sp)))
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 13 (use (reg/i:DI x29)))
      (cinsn 14 (use (reg/i:DI x30)))
      (cinsn 15 (use (reg/i:DI x0)))
      (cinsn 16 (use (reg/i:DI x3)))
      (cinsn 17 (use (reg/i:DI x5)))
      (cinsn 18 (use (reg/i:DI x1)))
      (cinsn 19 (use (reg/i:DI x4)))
      (cinsn 20 (use (reg/i:DI x6)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "ldp_ti_after_store"
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
      (cinsn 238 (set (reg/i:DI x1)
                   (reg/i:DI x0)))

      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI sp)
                          (const_int 8))[1 S4 A32])(reg:DI x0)))
      (cinsn 102 (set (mem/c:DI (plus:DI (reg/f:DI x1)
                                          (const_int -16)) [1 S4 A32])
                      (reg:DI x0)))
      (cinsn 103 (set (mem/c:DI (reg/f:DI x1) [1 S4 A32])
                      (reg:DI x0)))
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
      (cinsn 11 (parallel [
        (set (reg:DI x3)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int -16)) [1 S4 A32]))
        (set (reg:DI x4)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int -8)) [1 S4 A32]))
      ]))
      (cinsn 12 (parallel [
        (set (reg:DI x5) (mem:DI (reg/f:DI x1) [1 S4 A32]))
        (set (reg:DI x6) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 8)) [1 S4 A32]))
      ]))
      (cinsn 13 (use (reg/i:DI sp)))
      (cinsn 14 (use (reg/i:DI cc)))
      (cinsn 15 (use (reg/i:DI x29)))
      (cinsn 16 (use (reg/i:DI x30)))
      (cinsn 17 (use (reg/i:DI x0)))
      (cinsn 18 (use (reg/i:DI x3)))
      (cinsn 19 (use (reg/i:DI x4)))
      (cinsn 20 (use (reg/i:DI x5)))
      (cinsn 21 (use (reg/i:DI x6)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 3
  ) ;; insn-chain
) ;; function "ldp_after_store_in_different_bb"
}

/* Verify that the output code doesn't contain ldp.  */
/* { dg-final { scan-assembler-not {ldp\t} } }  */
