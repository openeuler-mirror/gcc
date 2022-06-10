/* { dg-do compile } */

#include <stdio.h>
#include <stdlib.h>

struct node
{
    struct node *left, *right;
    double a, b, c, d, e, f;
}
*a;
int b, c;
void
CreateNode (struct node **p1)
{
    *p1 = calloc (10, sizeof (struct node));
}

int
main ()
{
    a->left = 0;
    struct node *t = a;
    CreateNode (&t->right);

    struct node p = *a;
    b = 1;
    if (p.left)
        b = 0;
    if (b)
        printf ("   Tree.\n");
}

/* { dg-final { scan-ipa-dump "No structures to transform." "struct_layout" } } */