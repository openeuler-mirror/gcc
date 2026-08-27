// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules-ts -fsimdmath" }

// A module interface unit must still compile under -fsimdmath.  What the
// option pre-includes is eleven declarations, not the handful of
// #defines that lets the target's own pre-include past the rule that a
// module-declaration be the first declaration in the file - so every
// module interface unit failed outright:
//
//   error: module-declaration only permitted as first declaration, or
//   ending a global module fragment
//
// with no way to opt out short of dropping -fsimdmath, which is meant to
// live in a project's CXXFLAGS.  The global module fragment spelling did
// not help either; the same error landed on `module;'.  The pre-include
// is skipped under -fmodules-ts instead, and invoke.texi says so.

export module openeuler_simdmath_test;

export int
f (int x)
{
  return x + 1;
}
