# Bounded static byte CFG specialization

`-ftree-static-byte-cfg` enables generic GIMPLE residual specialization at
optimization levels of at least O2. It is disabled by default, independently
controlled, and runs after fix_loops and before tree_loop. Functions that
may throw non-call exceptions, including `-fnon-call-exceptions`, are skipped. Disable it with
`-fno-tree-static-byte-cfg` or the corresponding function optimization option.

The implementation tracks live immutable addresses and byte equality
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
charges per plan. The process allowance is shared in function visitation
order; separate compiler or LTO worker processes do not share this counter.
The limits are fixed implementation constants, not user-tunable parameters.
These are cost limits, not semantic proofs or machine
instruction limits. Copied debug bindings are conservatively dropped.

Eligibility is derived from GIMPLE without application names or source fingerprints.
Diagnostic planning/emission traces are available only in the static-byte-cfg dump.
The directed fixtures cover actual guarded and unguarded emission, both
guard outcomes, wide dynamic PHIs with nested loops and joins, function
options, virtual uses, rejected effects/exceptions, and bounded growth.
Positive tests check emission; negative tests check the rejection reason
where the pass is entered. Validation must include a native checking build
and runtime comparison against the disabled implementation. Directed tests
do not substitute for a full compiler regression suite or bootstrap.
