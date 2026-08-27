/* { dg-do compile } */

struct A {
    int a;
};

extern unsigned long long offset();

int main() {
    unsigned char num[16];
    ((struct A*)(num + offset() * 4))->a = 10;

    return num[0];
}

/* { dg-final { scan-ipa-dump "struct A(\\\(\[0-9\]*\\\))? has escaped: \"Type escapes a cast to a different pointer\"" "struct_reorg" } } */
