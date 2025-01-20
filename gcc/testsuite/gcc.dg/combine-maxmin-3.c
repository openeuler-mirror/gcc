/* { dg-do run { target aarch64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -flto -funroll-all-loops -ftrapv" } */

#include <stdlib.h>
signed char var_0 = (signed char)-65;
_Bool var_1 = (_Bool)0;
long long int var_2 = 4537210610282933998LL;
signed char var_3 = (signed char)126;
int var_4 = -613839209;
_Bool var_5 = (_Bool)1;
signed char var_6 = (signed char)-77;
unsigned short var_7 = (unsigned short)39911;
short var_8 = (short)-25330;
signed char var_9 = (signed char)124;
int var_10 = -2094906633;
long long int var_11 = 7607510567686389644LL;
int var_12 = 1942694297;
_Bool var_13 = (_Bool)0;
unsigned short var_14 = (unsigned short)12282;
long long int var_15 = 8390000680465515555LL;
int var_16 = -2035744757;
short var_17 = (short)14848;
signed char var_18 = (signed char)34;
unsigned char var_19 = (unsigned char)207;
int var_20 = -1740420486;
short var_21 = (short)28148;
unsigned char var_22 = (unsigned char)219;
signed char var_23 = (signed char)-48;
int var_24 = 1425916332;
long long int var_25 = 2357432417774053057LL;
long long int var_26 = 3783649483858092121LL;
unsigned long long int var_27 = 13696518060880031861ULL;
signed char var_28 = (signed char)39;
unsigned int var_29 = 4191102921U;
int var_30 = -205775739;
unsigned int var_31 = 2837591405U;
unsigned short var_32 = (unsigned short)30004;
unsigned char var_33 = (unsigned char)81;
short var_34 = (short)31524;
unsigned short var_35 = (unsigned short)9467;
unsigned long long int var_36 = 4985314089458744927ULL;
int var_37 = -531898798;
_Bool var_38 = (_Bool)0;
unsigned long long int var_39 = 18217768829329251554ULL;
short var_40 = (short)-22483;
short var_41 = (short)-7861;
unsigned long long int var_42 = 10662454878540201729ULL;
unsigned short var_43 = (unsigned short)15689;
unsigned char var_44 = (unsigned char)34;
short var_45 = (short)28212;
unsigned long long int var_46 = 16343757274715883049ULL;
unsigned short var_47 = (unsigned short)15300;
signed char var_48 = (signed char)-114;
unsigned long long int var_49 = 3639316430873843181ULL;
unsigned long long int var_50 = 560747942848858208ULL;
unsigned short var_51 = (unsigned short)13196;
unsigned long long int var_52 = 2040528781998952716ULL;
signed char var_53 = (signed char)-94;
signed char var_54 = (signed char)-12;
unsigned short var_55 = (unsigned short)29602;
_Bool var_56 = (_Bool)1;
long long int var_57 = 3625521270311471171LL;
unsigned short var_58 = (unsigned short)45244;
long long int var_59 = 3634905474514226279LL;
short var_60 = (short)30809;
unsigned char arr_0 [22] ;
short arr_1 [22] ;
int arr_2 [22] ;
_Bool arr_3 [22] ;
unsigned int arr_4 [22] [20] ;
long long int arr_5 [22] [20] ;
short arr_6 [22] [20] [23] ;
_Bool arr_7 [22] [20] [23] ;
unsigned short arr_8 [17] ;
long long int arr_9 [17] ;
int arr_10 [17] [20] ;
unsigned long long int arr_11 [17] [20] ;
unsigned int arr_12 [17] [20] [22] ;
short arr_17 [17] [20] [22] [16] [15] ;
unsigned long long int arr_34 [10] [18] [19] ;
unsigned int arr_38 [21] ;
unsigned short arr_40 [21] [21] ;
_Bool arr_41 [21] [21] ;
signed char arr_42 [21] [21] ;
unsigned char arr_45 [21] [16] ;
unsigned short arr_46 [21] [16] ;
signed char arr_47 [21] [16] ;
short arr_49 [21] [25] ;
unsigned char arr_50 [21] [25] ;
unsigned long long int arr_51 [21] [25] ;
long long int arr_52 [21] [25] ;
unsigned long long int arr_53 [21] [25] [17] ;
short arr_54 [21] [25] [17] ;
unsigned long long int arr_55 [21] [25] [17] ;
short arr_57 [21] [25] [17] [14] ;
unsigned long long int arr_59 [21] [25] [17] [14] ;
int arr_19 [17] [20] ;
long long int arr_20 [17] ;
short arr_21 [17] ;
long long int arr_22 [17] ;
signed char arr_26 [10] ;
unsigned short arr_27 [10] ;
_Bool arr_28 [10] ;
signed char arr_35 [10] [18] [19] ;
_Bool arr_36 [10] ;
unsigned int arr_39 [21] ;
_Bool arr_43 [21] [21] ;
signed char arr_44 [21] [21] ;
unsigned short arr_48 [21] [16] ;
signed char arr_60 [21] [25] [17] [14] ;

#define max(a,b) \
    ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
       _a > _b ? _a : _b; })
#define min(a,b) \
    ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
       _a < _b ? _a : _b; })
void test(signed char var_0, _Bool var_1, long long int var_2, signed char var_3, int var_4, _Bool var_5, signed char var_6, unsigned short var_7, short var_8, signed char var_9, int var_10, long long int var_11, int var_12, _Bool var_13, unsigned short var_14, long long int var_15, int var_16, short var_17, unsigned char arr_0 [22] , short arr_1 [22] , int arr_2 [22] , _Bool arr_3 [22] , unsigned int arr_4 [22] [20] , long long int arr_5 [22] [20] , short arr_6 [22] [20] [23] , _Bool arr_7 [22] [20] [23] , unsigned short arr_8 [17] , long long int arr_9 [17] , int arr_10 [17] [20] , unsigned long long int arr_11 [17] [20] , unsigned int arr_12 [17] [20] [22] , short arr_17 [17] [20] [22] [16] [15] , unsigned long long int arr_34 [10] [18] [19] , unsigned int arr_38 [21] , unsigned short arr_40 [21] [21] , _Bool arr_41 [21] [21] , signed char arr_42 [21] [21] , unsigned char arr_45 [21] [16] , unsigned short arr_46 [21] [16] , signed char arr_47 [21] [16] , short arr_49 [21] [25] , unsigned char arr_50 [21] [25] , unsigned long long int arr_51 [21] [25] , long long int arr_52 [21] [25] , unsigned long long int arr_53 [21] [25] [17] , short arr_54 [21] [25] [17] , unsigned long long int arr_55 [21] [25] [17] , short arr_57 [21] [25] [17] [14] , unsigned long long int arr_59 [21] [25] [17] [14]) {
    /* LoopNest 3 */
    for (unsigned char i_11 = ((/* implicit */int) ((/* implicit */unsigned char) ((long long int) (-2147483647 - 1))))/*0*/; i_11 < ((((/* implicit */int) ((/* implicit */unsigned char) var_3))) - (105))/*21*/; i_11 += ((((/* implicit */int) ((/* implicit */unsigned char) ((unsigned short) (+(((/* implicit */int) (unsigned char)143))))))) - (141))/*2*/) 
    {
        arr_39 [i_11] = ((/* implicit */unsigned int) (_Bool)1);
        /* LoopSeq 2 */
        for (unsigned long long int i_12 = ((((/* implicit */unsigned long long int) var_17)) - (14848ULL))/*0*/; i_12 < ((((/* implicit */unsigned long long int) ((((/* implicit */_Bool) ((((/* implicit */int) ((unsigned char) arr_1 [i_11]))) + (((/* implicit */int) arr_3 [i_11]))))) ? (((((/* implicit */_Bool) var_3)) ? (((/* implicit */int) arr_3 [i_11])) : (((/* implicit */int) var_5)))) : (((int) max((((/* implicit */unsigned int) arr_3 [i_11])), (arr_38 [i_11]))))))) + (20ULL))/*21*/; i_12 += ((((/* implicit */unsigned long long int) var_16)) - (18446744071673806855ULL))/*4*/) 
        {
            arr_43 [i_11] [i_12] = ((/* implicit */_Bool) arr_42 [i_11] [i_12]);
            arr_44 [i_11] [i_12] = ((/* implicit */signed char) ((((/* implicit */int) arr_42 [i_11] [i_12])) < (((/* implicit */int) var_8))));
        }
        for (unsigned short i_13 = (unsigned short)0/*0*/; i_13 < (unsigned short)16/*16*/; i_13 += ((((/* implicit */int) ((/* implicit */unsigned short) var_9))) - (123))/*1*/) 
        {
            arr_48 [i_11] [i_13] = ((/* implicit */unsigned short) ((((/* implicit */_Bool) ((((/* implicit */int) arr_42 [i_11] [i_13])) - (((((/* implicit */int) arr_45 [i_11] [i_13])) >> (((((/* implicit */int) (unsigned short)62162)) - (62156)))))))) && (((/* implicit */_Bool) ((((/* implicit */_Bool) max((228350614U), (((/* implicit */unsigned int) arr_42 [i_11] [i_13]))))) ? (((((/* implicit */_Bool) arr_45 [i_11] [i_13])) ? (16941145019982664891ULL) : (((/* implicit */unsigned long long int) ((/* implicit */int) arr_47 [i_11] [i_13]))))) : (((/* implicit */unsigned long long int) max((3256738989U), (((/* implicit */unsigned int) arr_47 [i_11] [i_13]))))))))));
            var_42 = ((/* implicit */unsigned long long int) min((((/* implicit */signed char) ((((/* implicit */int) arr_47 [i_11] [i_13])) > (((/* implicit */int) arr_47 [i_11] [i_13]))))), (min((arr_47 [i_11] [i_13]), (arr_47 [i_11] [i_13])))));
        }
        /* LoopNest 3 */
        for (long long int i_14 = 0LL/*0*/; i_14 < 25LL/*25*/; i_14 += 1LL/*1*/) 
        {
            for (unsigned char i_15 = (unsigned char)0/*0*/; i_15 < (unsigned char)17/*17*/; i_15 += (unsigned char)4/*4*/) 
            {
                for (unsigned int i_16 = 0U/*0*/; i_16 < 14U/*14*/; i_16 += 1U/*1*/) 
                {
                    {
                        var_43 = ((/* implicit */unsigned short) ((((/* implicit */_Bool) (~(((/* implicit */int) ((_Bool) var_17)))))) ? (((/* implicit */long long int) (+(((/* implicit */int) (unsigned short)53352))))) : (((long long int) var_6))));
                        var_44 = ((/* implicit */unsigned char) ((-1276214243810909816LL) + (((/* implicit */long long int) -392020199))));
                        arr_60 [i_11] [i_14] [i_15] [i_16] = ((/* implicit */signed char) (+(((arr_59 [i_11] [i_14] [i_15] [i_16]) / (arr_59 [i_11] [i_14] [i_15] [i_16])))));
                    }
                } 
            } 
        } 
    }
}

unsigned long long int seed = 0;
void hash(unsigned long long int *seed, unsigned long long int const v) {
    *seed ^= v + 0x9e3779b9 + ((*seed)<<6) + ((*seed)>>2);
}

void __attribute__((noinline)) init (){
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        arr_0 [i_0] = (unsigned char)106;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        arr_1 [i_0] = (short)-5301;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        arr_2 [i_0] = 1189702949;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        arr_3 [i_0] = (_Bool)1;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            arr_4 [i_0] [i_1] = 85903203U;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            arr_5 [i_0] [i_1] = 9064013761847121458LL;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            for (size_t i_2 = 0; i_2 < 23; ++i_2) 
                arr_6 [i_0] [i_1] [i_2] = (short)-27442;
    for (size_t i_0 = 0; i_0 < 22; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            for (size_t i_2 = 0; i_2 < 23; ++i_2) 
                arr_7 [i_0] [i_1] [i_2] = (_Bool)1;
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        arr_8 [i_0] = (unsigned short)23072;
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        arr_9 [i_0] = 5346357118588336085LL;
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            arr_10 [i_0] [i_1] = 179024656;
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            arr_11 [i_0] [i_1] = 16089413233175260353ULL;
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            for (size_t i_2 = 0; i_2 < 22; ++i_2) 
                arr_12 [i_0] [i_1] [i_2] = 1939512393U;
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            for (size_t i_2 = 0; i_2 < 22; ++i_2) 
                for (size_t i_3 = 0; i_3 < 16; ++i_3) 
                    for (size_t i_4 = 0; i_4 < 15; ++i_4) 
                        arr_17 [i_0] [i_1] [i_2] [i_3] [i_4] = (short)16848;
    for (size_t i_0 = 0; i_0 < 10; ++i_0) 
        for (size_t i_1 = 0; i_1 < 18; ++i_1) 
            for (size_t i_2 = 0; i_2 < 19; ++i_2) 
                arr_34 [i_0] [i_1] [i_2] = 4649060177745547059ULL;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        arr_38 [i_0] = 2631182830U;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 21; ++i_1) 
            arr_40 [i_0] [i_1] = (unsigned short)57532;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 21; ++i_1) 
            arr_41 [i_0] [i_1] = (_Bool)1;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 21; ++i_1) 
            arr_42 [i_0] [i_1] = (signed char)-116;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 16; ++i_1) 
            arr_45 [i_0] [i_1] = (unsigned char)153;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 16; ++i_1) 
            arr_46 [i_0] [i_1] = (unsigned short)59402;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 16; ++i_1) 
            arr_47 [i_0] [i_1] = (signed char)-62;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            arr_49 [i_0] [i_1] = (short)-10081;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            arr_50 [i_0] [i_1] = (unsigned char)187;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            arr_51 [i_0] [i_1] = 9264541432399000374ULL;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            arr_52 [i_0] [i_1] = 6149756306711255737LL;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            for (size_t i_2 = 0; i_2 < 17; ++i_2) 
                arr_53 [i_0] [i_1] [i_2] = 8347443449089475121ULL;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            for (size_t i_2 = 0; i_2 < 17; ++i_2) 
                arr_54 [i_0] [i_1] [i_2] = (short)-279;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            for (size_t i_2 = 0; i_2 < 17; ++i_2) 
                arr_55 [i_0] [i_1] [i_2] = 5965107530857000502ULL;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            for (size_t i_2 = 0; i_2 < 17; ++i_2) 
                for (size_t i_3 = 0; i_3 < 14; ++i_3) 
                    arr_57 [i_0] [i_1] [i_2] [i_3] = (short)22585;
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            for (size_t i_2 = 0; i_2 < 17; ++i_2) 
                for (size_t i_3 = 0; i_3 < 14; ++i_3) 
                    arr_59 [i_0] [i_1] [i_2] [i_3] = 12577208163329471017ULL;
}

void checksum() {
    hash(&seed, var_18);
    hash(&seed, var_19);
    hash(&seed, var_20);
    hash(&seed, var_21);
    hash(&seed, var_22);
    hash(&seed, var_23);
    hash(&seed, var_24);
    hash(&seed, var_25);
    hash(&seed, var_26);
    hash(&seed, var_27);
    hash(&seed, var_28);
    hash(&seed, var_29);
    hash(&seed, var_30);
    hash(&seed, var_31);
    hash(&seed, var_32);
    hash(&seed, var_33);
    hash(&seed, var_34);
    hash(&seed, var_35);
    hash(&seed, var_36);
    hash(&seed, var_37);
    hash(&seed, var_38);
    hash(&seed, var_39);
    hash(&seed, var_40);
    hash(&seed, var_41);
    hash(&seed, var_42);
    hash(&seed, var_43);
    hash(&seed, var_44);
    hash(&seed, var_45);
    hash(&seed, var_46);
    hash(&seed, var_47);
    hash(&seed, var_48);
    hash(&seed, var_49);
    hash(&seed, var_50);
    hash(&seed, var_51);
    hash(&seed, var_52);
    hash(&seed, var_53);
    hash(&seed, var_54);
    hash(&seed, var_55);
    hash(&seed, var_56);
    hash(&seed, var_57);
    hash(&seed, var_58);
    hash(&seed, var_59);
    hash(&seed, var_60);

    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        for (size_t i_1 = 0; i_1 < 20; ++i_1) 
            hash(&seed, arr_19 [i_0] [i_1] );
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        hash(&seed, arr_20 [i_0] );
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        hash(&seed, arr_21 [i_0] );
    for (size_t i_0 = 0; i_0 < 17; ++i_0) 
        hash(&seed, arr_22 [i_0] );
    for (size_t i_0 = 0; i_0 < 10; ++i_0) 
        hash(&seed, arr_26 [i_0] );
    for (size_t i_0 = 0; i_0 < 10; ++i_0) 
        hash(&seed, arr_27 [i_0] );
    for (size_t i_0 = 0; i_0 < 10; ++i_0) 
        hash(&seed, arr_28 [i_0] );
    for (size_t i_0 = 0; i_0 < 10; ++i_0) 
        for (size_t i_1 = 0; i_1 < 18; ++i_1) 
            for (size_t i_2 = 0; i_2 < 19; ++i_2) 
                hash(&seed, arr_35 [i_0] [i_1] [i_2] );
    for (size_t i_0 = 0; i_0 < 10; ++i_0) 
        hash(&seed, arr_36 [i_0] );
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        hash(&seed, arr_39 [i_0] );
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 21; ++i_1) 
            hash(&seed, arr_43 [i_0] [i_1] );
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 21; ++i_1) 
            hash(&seed, arr_44 [i_0] [i_1] );
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 16; ++i_1) 
            hash(&seed, arr_48 [i_0] [i_1] );
    for (size_t i_0 = 0; i_0 < 21; ++i_0) 
        for (size_t i_1 = 0; i_1 < 25; ++i_1) 
            for (size_t i_2 = 0; i_2 < 17; ++i_2) 
                for (size_t i_3 = 0; i_3 < 14; ++i_3) 
                    hash(&seed, arr_60 [i_0] [i_1] [i_2] [i_3] );
}

void test(signed char var_0, _Bool var_1, long long int var_2, signed char var_3, int var_4, _Bool var_5, signed char var_6, unsigned short var_7, short var_8, signed char var_9, int var_10, long long int var_11, int var_12, _Bool var_13, unsigned short var_14, long long int var_15, int var_16, short var_17, unsigned char arr_0 [22] , short arr_1 [22] , int arr_2 [22] , _Bool arr_3 [22] , unsigned int arr_4 [22] [20] , long long int arr_5 [22] [20] , short arr_6 [22] [20] [23] , _Bool arr_7 [22] [20] [23] , unsigned short arr_8 [17] , long long int arr_9 [17] , int arr_10 [17] [20] , unsigned long long int arr_11 [17] [20] , unsigned int arr_12 [17] [20] [22] , short arr_17 [17] [20] [22] [16] [15] , unsigned long long int arr_34 [10] [18] [19] , unsigned int arr_38 [21] , unsigned short arr_40 [21] [21] , _Bool arr_41 [21] [21] , signed char arr_42 [21] [21] , unsigned char arr_45 [21] [16] , unsigned short arr_46 [21] [16] , signed char arr_47 [21] [16] , short arr_49 [21] [25] , unsigned char arr_50 [21] [25] , unsigned long long int arr_51 [21] [25] , long long int arr_52 [21] [25] , unsigned long long int arr_53 [21] [25] [17] , short arr_54 [21] [25] [17] , unsigned long long int arr_55 [21] [25] [17] , short arr_57 [21] [25] [17] [14] , unsigned long long int arr_59 [21] [25] [17] [14]);

int main() {
    init();
    test(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, var_10, var_11, var_12, var_13, var_14, var_15, var_16, var_17, arr_0 , arr_1 , arr_2 , arr_3 , arr_4 , arr_5 , arr_6 , arr_7 , arr_8 , arr_9 , arr_10 , arr_11 , arr_12 , arr_17 , arr_34 , arr_38 , arr_40 , arr_41 , arr_42 , arr_45 , arr_46 , arr_47 , arr_49 , arr_50 , arr_51 , arr_52 , arr_53 , arr_54 , arr_55 , arr_57 , arr_59);
    checksum();
    if (seed != 4635925598123817158LL)
      abort ();
    return 0;
}
