# Disjoint-store PHI propagation

This is an independently developed, default-off extension for GCC 14.
It is not a backport of GCC PR123120 or PR116823.

Upstream commits 83a4c4c122ac8552f90a524906ba73f0602a2e8e,
143bb738d4f17b10ab369436ea1daaa2da3cc1dc and
1d63e50467f140b4749065afb1c1b3f94c819de8 refactor load handling,
allow one intervening non-clobbering store, and skip lifetime clobbers.
Their scalar path queries the load reference with TBAA disabled.

The GCC 14 extension instead checks every incoming automatic scalar object
against each intervening memory definition in the same block. It can cross
more than one such definition, retains the appropriate incoming virtual
memory state, and shares a bounded analysis budget across candidates.
The existing multi-store regression cases exercise that extra coverage.
Aggregate copies, potentially modified objects, incompatible object types
and exhausted budgets retain the original transformation rules.

The alias queries use the normal GCC alias oracle, including TBAA when
enabled. The incoming declarations must have types compatible with the
original load. This is not permission to assume disjointness for a legal
may_alias access or when strict aliasing is disabled. Tests include upstream
scalar positive and aggregate negative cases in addition to the extension's
alias, volatile, work-budget and differential runtime cases.

Replacing this extension with the upstream implementation would change its
default policy and multi-store coverage. Such a baseline migration should
be evaluated separately, including compile cost and retained transformations.

The separate PR116823 lifetime-clobber transformation is not implemented
here.  Its compile-only example is retained as a negative coverage control;
its upstream positive dump expectations do not describe this extension.
