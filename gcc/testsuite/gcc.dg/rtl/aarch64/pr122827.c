/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O2 -fno-caller-saves -march=armv8.2-a+sve -msve-vector-bits=128" } */

/* A VNx2SF value occupies a full Z register even though its data size is
   only 64 bits for -msve-vector-bits=128.  It therefore cannot be kept in
   V8-V15 across an ordinary call, since AAPCS64 only preserves D8-D15.  */

void __RTL (startwith ("ira")) test (void)
{
(function "test"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (insn 2 (set (reg:VNx2SF <0>) (reg:VNx2SF v0)))
      (call_insn 3 (parallel [
                    (call (mem:DI (symbol_ref:DI ("clobber") [flags 0x41])
                                  [0 clobber S8 A8])
                          (const_int 0))
                    (unspec:DI [(const_int 2)] UNSPEC_CALLEE_ABI)
                    (clobber (reg:DI x30))
                  ]))
      (insn 4 (set (reg:VNx2SF v0) (reg:VNx2SF <0>)))
      (insn 5 (use (reg:VNx2SF v0)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
)
}

/* Before PR target/122827, this sequence used a Z8-Z15 register across the
   call while saving and restoring only its low 64 bits.  */
/* { dg-final { scan-assembler-not {mov\tz(8|9|1[0-5])\.d,} } } */
