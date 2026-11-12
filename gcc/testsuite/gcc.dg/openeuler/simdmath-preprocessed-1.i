/* { dg-do compile } */
/* { dg-options "-fsimdmath -fpreprocessed -Wno-builtin-declaration-mismatch" } */

/* Already-preprocessed input must compile cleanly under -fsimdmath.
   openeuler.exp's glob has always accepted *.i and the directory
   contained none, so this path was never exercised at all.

   It does not pin the !preprocessed term of the guard: removing that
   term changes nothing here, because libcpp declines to open a default
   include for preprocessed input on its own - measured.  The term stays
   for symmetry with the target's own pre-include, which carries it, and
   is documented here as belt-and-braces rather than as covered.

   `int pow' rather than an ordinary declaration so that the file would
   notice if the declarations did arrive: a compatible redeclaration
   would go unseen, a conflicting one is an error.  The -Wno- above only
   silences the note that `pow' is a built-in name.  */

int pow;

int
f (void)
{
  return pow;
}
