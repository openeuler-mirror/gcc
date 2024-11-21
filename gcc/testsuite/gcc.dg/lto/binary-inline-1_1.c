/* { dg-options "-march=armv8.3-a+sve+f64mm+crc+crypto+fp16+i8mm+simd" } */

double multi_op (float x)
{
    return x * 2 + 10;
}
