
/* { dg-do compile { target { aarch64*-*-linux* } } } */
/* { dg-options "-O3 -march=armv8.2-a+sve -static -fllc-allocate -fdump-tree-llc_allocate-details-lineno --param=outer-loop-nums=10 --param=issue-topn=4 --param=force-issue=1 --param=filter-kernels=0" } */


int val[100000];
int main(){
	for(int i=0;i<100000;i++){
		__builtin_prefetch_full(&val[i],1,6);
		val[i]=i+1;		
	}
}

/* { dg-final { scan-assembler "PSTL4KEEP"  } } */

