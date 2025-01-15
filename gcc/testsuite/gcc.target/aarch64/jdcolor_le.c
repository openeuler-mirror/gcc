/* { dg-do compile } */
/* { dg-options "-O3" } */
/* It's a preprocessed part of libjpeg-turbo.  */

typedef int boolean;
typedef short INT16;
typedef unsigned short UINT16;
typedef long unsigned int size_t;
typedef long JLONG;
typedef unsigned int JDIMENSION;
typedef short J12SAMPLE;
typedef J12SAMPLE *J12SAMPROW;
typedef J12SAMPROW *J12SAMPARRAY;
typedef J12SAMPARRAY *J12SAMPIMAGE;

void
rgb_rgb565_convert_le(JDIMENSION num_cols, J12SAMPIMAGE input_buf,
                      JDIMENSION input_row, J12SAMPARRAY output_buf,
                      int num_rows)
{
  register J12SAMPROW outptr;
  register J12SAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;


  while (--num_rows >= 0) {
    JLONG rgb;
    unsigned int r, g, b;

    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    if ((((size_t)(outptr)) & 3)) {
      r = *inptr0++;
      g = *inptr1++;
      b = *inptr2++;
      rgb = ((((r) << 8) & 0xF800) | (((g) << 3) & 0x7E0) | ((b) >> 3));
      *(INT16 *)outptr = (INT16)rgb;
      outptr += 2;
      num_cols--;
    }
    for (col = 0; col < (num_cols >> 1); col++) {
      r = *inptr0++;
      g = *inptr1++;
      b = *inptr2++;
      rgb = ((((r) << 8) & 0xF800) | (((g) << 3) & 0x7E0) | ((b) >> 3));

      r = *inptr0++;
      g = *inptr1++;
      b = *inptr2++;
      rgb = ((((((r) << 8) & 0xF800) | (((g) << 3) & 0x7E0) | ((b) >> 3)) << 16) | rgb);

      ((*(int *)(outptr)) = rgb);
      outptr += 4;
    }
    if (num_cols & 1) {
      r = *inptr0;
      g = *inptr1;
      b = *inptr2;
      rgb = ((((r) << 8) & 0xF800) | (((g) << 3) & 0x7E0) | ((b) >> 3));
      *(INT16 *)outptr = (INT16)rgb;
    }
  }
}
/* We should not generate shll[2] for this test case.  */
/* { dg-final { scan-assembler-not "shll" } } */
