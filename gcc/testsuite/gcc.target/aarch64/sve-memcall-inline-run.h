#ifndef GCC_AARCH64_SVE_MEMCALL_INLINE_RUN_H
#define GCC_AARCH64_SVE_MEMCALL_INLINE_RUN_H

typedef __SIZE_TYPE__ size_t;

#define SVE_MEMCALL_BUFFER_SIZE 1024
#define SVE_MEMCALL_GUARD_VALUE 0x3c

static unsigned char src[SVE_MEMCALL_BUFFER_SIZE]
  __attribute__ ((aligned (64)));
static unsigned char dst[SVE_MEMCALL_BUFFER_SIZE]
  __attribute__ ((aligned (64)));

static unsigned char
source_value (size_t i)
{
  unsigned char value = (unsigned char) (i * 37 + 11);
  return (value == SVE_MEMCALL_GUARD_VALUE
	  ? (unsigned char) ~value : value);
}

static void * __attribute__ ((noipa))
copy_bytes (void *d, const void *s, size_t n)
{
  return __builtin_memcpy (d, s, n);
}

static void * __attribute__ ((noipa))
set_bytes (void *d, int value, size_t n)
{
  return __builtin_memset (d, value, n);
}

static void
reset_buffers (void)
{
  size_t i;

  for (i = 0; i < SVE_MEMCALL_BUFFER_SIZE; ++i)
    {
      src[i] = source_value (i);
      dst[i] = SVE_MEMCALL_GUARD_VALUE;
      asm volatile ("" ::: "memory");
    }
}

static void
check_memcpy (size_t n, size_t dst_offset, size_t src_offset)
{
  size_t i;
  void *result;

  reset_buffers ();
  result = copy_bytes (dst + dst_offset, src + src_offset, n);

  if (result != dst + dst_offset)
    __builtin_abort ();

  for (i = 0; i < SVE_MEMCALL_BUFFER_SIZE; ++i)
    {
      unsigned char expected = SVE_MEMCALL_GUARD_VALUE;

      if (i >= dst_offset && i < dst_offset + n)
	expected = src[src_offset + i - dst_offset];

      if (dst[i] != expected || src[i] != source_value (i))
	__builtin_abort ();
      asm volatile ("" ::: "memory");
    }
}

static void
check_memset (size_t n, size_t dst_offset, int value)
{
  size_t i;
  void *result;

  reset_buffers ();
  result = set_bytes (dst + dst_offset, value, n);

  if (result != dst + dst_offset)
    __builtin_abort ();

  for (i = 0; i < SVE_MEMCALL_BUFFER_SIZE; ++i)
    {
      unsigned char expected = SVE_MEMCALL_GUARD_VALUE;

      if (i >= dst_offset && i < dst_offset + n)
	expected = (unsigned char) value;

      if (dst[i] != expected || src[i] != source_value (i))
	__builtin_abort ();
      asm volatile ("" ::: "memory");
    }
}

int
main (void)
{
  static const size_t sizes[] = { SVE_MEMCALL_TEST_SIZES };
  static const int values[] = { 0, 0x5a, 0x80, -1 };
  size_t i, j;

  for (i = 0; i < sizeof (sizes) / sizeof (sizes[0]); ++i)
    {
      check_memcpy (sizes[i], 0, 0);
      check_memcpy (sizes[i], 1, 3);

      for (j = 0; j < sizeof (values) / sizeof (values[0]); ++j)
	{
	  check_memset (sizes[i], 0, values[j]);
	  check_memset (sizes[i], 1, values[j]);
	}
    }

  return 0;
}

#endif
