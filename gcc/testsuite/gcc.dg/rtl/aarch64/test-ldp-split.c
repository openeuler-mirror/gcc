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
      /* mov sp, x0.  */
      (cinsn 228 (set (reg/i:DI sp)
                      (reg/i:DI x0)))
      /* mov x1, x0.  */
      (cinsn 238 (set (reg/i:DI x1)
                      (reg/i:DI x0)))

      /* str x0, [sp, 8].  */
      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI sp)
                          (const_int 8))[1 S4 A32])(reg:DI x0)))
      /* ldp x29, x30, [sp, 8].  */
      (cinsn 10 (parallel [
        (set (reg:DI x29)
          (mem:DI (plus:DI (reg/f:DI sp) (const_int 8)) [1 S4 A32]))
        (set (reg:DI x30)
          (mem:DI (plus:DI (reg/f:DI sp)
            (const_int 16)) [1 S4 A32]))]))
      (cinsn 11 (use (reg/i:DI x29)))
      (cinsn 12 (use (reg/i:DI x30)))

      /* str x0, [x1, -16].  */
      (cinsn 102 (set (mem/c:DI (plus:DI (reg/f:DI x1)
                                          (const_int -16)) [1 S4 A32])
                      (reg:DI x0)))
      /* ldp x3, x4, [x1, -16].  */
      (cinsn 13 (parallel [
        (set (reg:DI x3)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int -16)) [1 S4 A32]))
        (set (reg:DI x4)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int -8)) [1 S4 A32]))
      ]))
      (cinsn 14 (use (reg/i:DI x3)))
      (cinsn 15 (use (reg/i:DI x4)))

      /* str x0, [x1].  */
      (cinsn 103 (set (mem/c:DI (reg/f:DI x1) [1 S4 A32])
                      (reg:DI x0)))
      /* ldp x5, x6, [x1].  */
      (cinsn 16 (parallel [
        (set (reg:DI x5) (mem:DI (reg/f:DI x1) [1 S4 A32]))
        (set (reg:DI x6) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 8)) [1 S4 A32]))
      ]))
      (cinsn 17 (use (reg/i:DI x5)))
      (cinsn 18 (use (reg/i:DI x6)))

      /* ldp x29, x30, [sp], 96.  */
      (cinsn 19 (parallel [
        (set (reg/f:DI sp)
          (plus:DI (reg/f:DI sp) (const_int 96)))
        (set (reg:DI x29)
          (mem:DI (reg/f:DI sp) [1 S4 A32]))
        (set (reg:DI x30)
          (mem:DI (plus:DI (reg/f:DI sp)
            (const_int 8)) [1 S4 A32]))]))
      (cinsn 20 (use (reg/i:DI x29)))
      (cinsn 21 (use (reg/i:DI x30)))

      /* stp x0, x2, [x1, 128].  */
      (cinsn 104 (parallel [
        (set (mem:DI (plus:DI (reg/f:DI x1) (const_int 128)) [1 S4 A32])
             (reg:DI x0))
        (set (mem:DI (plus:DI (reg/f:DI x1) (const_int 136)) [1 S4 A32])
             (reg:DI x2))]))
      /* ldp x29, x30, [x1, 120].  */
      (cinsn 22 (parallel [
        (set (reg:DI x29)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int 120)) [1 S4 A32]))
        (set (reg:DI x30)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int 128)) [1 S4 A32]))]))
      (cinsn 23 (use (reg/i:DI x29)))
      (cinsn 24 (use (reg/i:DI x30)))

      /* stp x0, x2, [x1, 128].  */
      (cinsn 105 (parallel [
        (set (mem:DI (plus:DI (reg/f:DI x1) (const_int 128)) [1 S4 A32])
             (reg:DI x0))
        (set (mem:DI (plus:DI (reg/f:DI x1) (const_int 136)) [1 S4 A32])
             (reg:DI x2))]))
      /* ldp x3, x4, [x1, 136].  */
      (cinsn 25 (parallel [
        (set (reg:DI x3)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int 136)) [1 S4 A32]))
        (set (reg:DI x4)
          (mem:DI (plus:DI (reg/f:DI x1) (const_int 144)) [1 S4 A32]))
      ]))
      (cinsn 26 (use (reg/i:DI x3)))
      (cinsn 27 (use (reg/i:DI x4)))

      /* stp w0, w2, [x1, 32].  */
      (cinsn 106 (parallel [
        (set (mem:SI (plus:DI (reg/f:DI x1) (const_int 32)) [1 S4 A32])
             (reg:SI x0))
        (set (mem:SI (plus:DI (reg/f:DI x1) (const_int 36)) [1 S4 A32])
             (reg:SI x2))]))
      /* ldp x5, x6, [x1, 32].  */
      (cinsn 28 (parallel [
        (set (reg:DI x5) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 32)) [1 S4 A32]))
        (set (reg:DI x6) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 40)) [1 S4 A32]))
      ]))
      (cinsn 29 (use (reg/i:DI x5)))
      (cinsn 30 (use (reg/i:DI x6)))

      /* stp w0, w2, [x1, 40].  */
      (cinsn 107 (parallel [
        (set (mem:SI (plus:DI (reg/f:DI x1) (const_int 40)) [1 S4 A32])
             (reg:SI x0))
        (set (mem:SI (plus:DI (reg/f:DI x1) (const_int 44)) [1 S4 A32])
             (reg:SI x2))]))
      /* ldp x5, x6, [x1, 32].  */
      (cinsn 31 (parallel [
        (set (reg:DI x5) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 32)) [1 S4 A32]))
        (set (reg:DI x6) (mem:DI (plus:DI (reg/f:DI x1)
                                          (const_int 40)) [1 S4 A32]))
      ]))
      (cinsn 32 (use (reg/i:DI x5)))
      (cinsn 33 (use (reg/i:DI x6)))

      (cinsn 100 (use (reg/i:DI sp)))
      (cinsn 200 (use (reg/i:DI cc)))
      (cinsn 400 (use (reg/i:DI x0)))
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
      /* mov sp, x0.  */
      (cinsn 228 (set (reg/i:DI sp)
                      (reg/i:DI x0)))
      /* mov x2, x0.  */
      (cinsn 238 (set (reg/i:DI x2)
                      (reg/i:DI x0)))
      /* str x0, [sp, 136].  */
      (cinsn 101 (set (mem/c:DI
                        (plus:DI (reg/f:DI sp)
                          (const_int 136))[1 S4 A32])(reg:DI x0)))
      /* ldp x0, x1, [sp, 136].  */
      (cinsn 81 (set (reg:TI x0 [1 S4 A32])
              (mem/c:TI (plus:DI (reg/f:DI sp)
                      (const_int 136)) [1 S4 A32])))
      /* str x0, [x2, -16].  */
      (cinsn 102 (set (mem/c:DI (plus:DI (reg/f:DI x2)
                                         (const_int -16)) [1 S4 A32])
                      (reg:DI x0)))
      /* ldp x3, x4, [x2, -16].  */
      (cinsn 82 (set (reg:TI x3 [1 S4 A32])
                    (mem/c:TI (plus:DI (reg/f:DI x2)
                                       (const_int -16)) [1 S4 A32])))
      /* str x0, [x2].  */
      (cinsn 103 (set (mem/c:DI (reg/f:DI x2) [1 S4 A32])
                      (reg:DI x0)))
      /* ldp x5, x6, [x2].  */
      (cinsn 83 (set (reg:TI x5 [1 S4 A32])
                    (mem/c:TI (reg/f:DI x2) [1 S4 A32])))

      /* stp x0, x1, [sp, -8].  */
      (cinsn 104 (set (mem:TI (plus:DI (reg/v/f:DI sp)
                                       (const_int -8)) [1 S4 A32])
                      (reg:TI x0)))
      /* ldp x5, x6, [sp], -16.  */
      (cinsn 84 (set (reg/v:TI x5 [1 S4 A32])
                    (mem:TI (post_dec:DI (reg/v/f:DI sp)) [1 S4 A32])))
      (cinsn 85 (use (reg/i:DI x5)))
      (cinsn 86 (use (reg/i:DI x6)))

      /* stp x0, x1, [sp, 8].  */
      (cinsn 105 (set (mem:TI (plus:DI (reg/v/f:DI sp)
                                       (const_int 8)) [1 S4 A32])
                      (reg:TI x0)))
      /* ldp x5, x6, [sp], -16.  */
      (cinsn 87 (set (reg/v:TI x5 [1 S4 A32])
                    (mem:TI (post_dec:DI (reg/v/f:DI sp)) [1 S4 A32])))
      (cinsn 88 (use (reg/i:DI x5)))
      (cinsn 89 (use (reg/i:DI x6)))

      /* Intersects with insn 102.  */
      /* ldp x2, x3, [x2, -16]!.  */
      (cinsn 90 (set (reg/v:TI x2 [1 S4 A32])
                    (mem:TI (pre_dec:DI (reg/v/f:DI x2)) [1 S4 A32])))
      (cinsn 91 (use (reg/i:DI x2)))
      (cinsn 92 (use (reg/i:DI x3)))

      /* mov x2, x0.  */
      (cinsn 248 (set (reg/i:DI x2)
                      (reg/i:DI x0)))
      /* str x0, [x2, 16].  */
      (cinsn 106 (set (mem:DI (plus:DI (reg/v/f:DI x2)
                                       (const_int 16)) [1 S4 A32])
                      (reg:DI x0)))
      /* ldp x3, x4, [x2, 16]!.  */
      (cinsn 93 (set (reg/v:TI x3 [1 S4 A32])
                    (mem:TI (pre_inc:DI (reg/v/f:DI x2)) [1 S4 A32])))
      (cinsn 94 (use (reg/i:DI x3)))
      (cinsn 95 (use (reg/i:DI x4)))

      (cinsn 11 (use (reg/i:DI sp)))
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 13 (use (reg/i:DI x29)))
      (cinsn 14 (use (reg/i:DI x30)))
      (cinsn 15 (use (reg/i:DI x0)))
      (cinsn 16 (use (reg/i:DI x3)))
      (cinsn 18 (use (reg/i:DI x1)))
      (cinsn 19 (use (reg/i:DI x4)))
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
