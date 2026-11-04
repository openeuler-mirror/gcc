# Bounded static byte CFG specialization

`-ftree-static-byte-cfg` enables generic GIMPLE residual specialization at
optimization levels of at least O2. It is disabled by default, independently
controlled, and runs after fix_loops and before tree_loop. Disable it with
`-fno-tree-static-byte-cfg` or the corresponding function optimization option.

The GC05 implementation tracks live immutable addresses and byte equality
facts from actual GIMPLE. It contracts transparent CFG bridges and blocks
whose PHIs, definitions and successor are completely constant, and performs
bounded constant execution across backedges. Dynamic operations and branches
remain in the residual CFG. Parallel PHI inputs, individual edge arguments,
SSA form and pointer-address canonicalization are maintained. Parameter
hypotheses require real runtime guards, with the original CFG as fallback.

Each local lookahead is bounded at 512 steps. Exhaustion materializes the
real destination before its PHIs; it does not assert loop completion. An
unsupported statement, side effect, exception or unsuccessful overall plan
retains the original function. Admissible pure/const calls remain at their
original dynamic execution points. Functions above 256 basic blocks are not
handled. Residual states are bounded at 512, estimated emission cost at
4096 per function and 32768 per compiler process, and analysis at 1048576
charges per plan. These are cost limits, not semantic proofs or machine
instruction limits. Copied debug bindings are conservatively dropped.

Eligibility is derived from GIMPLE without application names or source fingerprints.
Diagnostic planning/emission traces are available only in the static-byte-cfg dump.
This standalone migration has source-level and fixture checks, but its GCC14
build and full compiler regression suite have not been executed locally.
