/* { dg-do link } */
/* { dg-options "-O3 -fipa-ic -fipa-prefetch -flto -flto-partition=one -fdump-ipa-ipa_prefetch -fdump-ipa-icp" } */
/* { dg-require-effective-target lto } */

/* Based on opensource xz code.  */

#include <stdlib.h>
#include <string.h>

typedef long int ptrdiff_t;
typedef long unsigned int size_t;
typedef unsigned int wchar_t;

typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;

typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;

typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;

typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;

typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;

typedef long int intptr_t;
typedef unsigned long int uintptr_t;

static inline uint16_t
read16ne(const uint8_t *buf)
{
 uint16_t num;
 memcpy(&num, buf, sizeof(num));
 return num;
}

static inline uint32_t
read32ne(const uint8_t *buf)
{
 uint32_t num;
 memcpy(&num, buf, sizeof(num));
 return num;
}

static inline uint16_t
aligned_read16ne(const uint8_t *buf)
{
 uint16_t num;
 memcpy(&num, __builtin_assume_aligned(buf, sizeof(num)), sizeof(num));
 return num;
}


static inline uint32_t
aligned_read32ne(const uint8_t *buf)
{
 uint32_t num;
 memcpy(&num, __builtin_assume_aligned(buf, sizeof(num)), sizeof(num));
 return num;
}

static inline uint64_t
aligned_read64ne(const uint8_t *buf)
{
 uint64_t num;
 memcpy(&num, __builtin_assume_aligned(buf, sizeof(num)), sizeof(num));
 return num;
}

typedef unsigned char lzma_bool;

typedef enum {
 LZMA_RESERVED_ENUM = 0
} lzma_reserved_enum;

typedef enum {
 LZMA_OK = 0,
 LZMA_STREAM_END = 1,
 LZMA_NO_CHECK = 2,
 LZMA_UNSUPPORTED_CHECK = 3,
 LZMA_GET_CHECK = 4,
 LZMA_MEM_ERROR = 5,
 LZMA_MEMLIMIT_ERROR = 6,
 LZMA_FORMAT_ERROR = 7,
 LZMA_OPTIONS_ERROR = 8,
 LZMA_DATA_ERROR = 9,
 LZMA_BUF_ERROR = 10,
 LZMA_PROG_ERROR = 11,
} lzma_ret;

typedef enum {
 LZMA_RUN = 0,
 LZMA_SYNC_FLUSH = 1,
 LZMA_FULL_FLUSH = 2,
 LZMA_FULL_BARRIER = 4,
 LZMA_FINISH = 3
} lzma_action;

typedef struct {
 void *( *alloc)(void *opaque, size_t nmemb, size_t size);

 void ( *free)(void *opaque, void *ptr);

 void *opaque;
} lzma_allocator;

typedef uint64_t lzma_vli;

typedef enum {
 LZMA_CHECK_NONE = 0,
 LZMA_CHECK_CRC32 = 1,
 LZMA_CHECK_CRC64 = 4,
 LZMA_CHECK_SHA256 = 10
} lzma_check;

typedef struct {
 lzma_vli id;
 void *options;
} lzma_filter;

typedef enum {
 LZMA_MF_HC3 = 0x03,
 LZMA_MF_HC4 = 0x04,
 LZMA_MF_BT2 = 0x12,
 LZMA_MF_BT3 = 0x13,
 LZMA_MF_BT4 = 0x14
} lzma_match_finder;

typedef struct lzma_next_coder_s lzma_next_coder;

typedef struct lzma_filter_info_s lzma_filter_info;

typedef lzma_ret (*lzma_init_function)(
  lzma_next_coder *next, const lzma_allocator *allocator,
  const lzma_filter_info *filters);

typedef lzma_ret (*lzma_code_function)(
  void *coder, const lzma_allocator *allocator,
  const uint8_t *restrict in, size_t *restrict in_pos,
  size_t in_size, uint8_t *restrict out,
  size_t *restrict out_pos, size_t out_size,
  lzma_action action);

typedef void (*lzma_end_function)(
  void *coder, const lzma_allocator *allocator);

struct lzma_filter_info_s {
 lzma_vli id;
 lzma_init_function init;
 void *options;
};

struct lzma_next_coder_s {
 void *coder;
 lzma_vli id;
 uintptr_t init;

 lzma_code_function code;
 lzma_end_function end;
 void (*get_progress)(void *coder,
   uint64_t *progress_in, uint64_t *progress_out);

 lzma_check (*get_check)(const void *coder);
 lzma_ret (*memconfig)(void *coder, uint64_t *memusage,
   uint64_t *old_memlimit, uint64_t new_memlimit);
 lzma_ret (*update)(void *coder, const lzma_allocator *allocator,
   const lzma_filter *filters, const lzma_filter *reversed_filters);
};

typedef struct {
 uint32_t len;
 uint32_t dist;
} lzma_match;

typedef struct lzma_mf_s lzma_mf;
struct lzma_mf_s {
 uint8_t *buffer;
 uint32_t size;
 uint32_t keep_size_before;
 uint32_t keep_size_after;
 uint32_t offset;
 uint32_t read_pos;
 uint32_t read_ahead;
 uint32_t read_limit;
 uint32_t write_pos;
 uint32_t pending;
 uint32_t (*find)(lzma_mf *mf, lzma_match *matches);
 void (*skip)(lzma_mf *mf, uint32_t num);
 uint32_t *hash;
 uint32_t *son;
 uint32_t cyclic_pos;
 uint32_t cyclic_size;
 uint32_t hash_mask;
 uint32_t depth;
 uint32_t nice_len;
 uint32_t match_len_max;
 lzma_action action;
 uint32_t hash_count;
 uint32_t sons_count;
};

typedef struct {
 size_t before_size;
 size_t dict_size;
 size_t after_size;
 size_t match_len_max;
 size_t nice_len;
 lzma_match_finder match_finder;
 uint32_t depth;
 const uint8_t *preset_dict;
 uint32_t preset_dict_size;
} lzma_lz_options;

typedef struct {
 void *coder;
 lzma_ret (*code)(void *coder,
   lzma_mf *restrict mf, uint8_t *restrict out,
   size_t *restrict out_pos, size_t out_size);
 void (*end)(void *coder, const lzma_allocator *allocator);
 lzma_ret (*options_update)(void *coder, const lzma_filter *filter);
} lzma_lz_encoder;

static inline const uint8_t *
mf_ptr(const lzma_mf *mf)
{
 return mf->buffer + mf->read_pos;
}

static inline uint32_t
mf_avail(const lzma_mf *mf)
{
 return mf->write_pos - mf->read_pos;
}

typedef struct {
 uint32_t state[8];
 uint64_t size;
} lzma_sha256_state;

typedef struct {
 union {
  uint8_t u8[64];
  uint32_t u32[16];
  uint64_t u64[8];
 } buffer;
 union {
  uint32_t crc32;
  uint64_t crc64;
  lzma_sha256_state sha256;
 } state;
} lzma_check_state;

// The table is constantly initialized in the original code.
// Skip it in the test.
const uint32_t lzma_crc32_table[8][256];

static inline uint32_t __attribute__((__always_inline__))
lzma_memcmplen(const uint8_t *buf1, const uint8_t *buf2,
  uint32_t len, uint32_t limit)
{
 while (len < limit) {
  uint32_t x = read32ne(buf1 + len) - read32ne(buf2 + len);
  if (x != 0) {
   if ((x & 0xFFFF) == 0) {
    len += 2;
    x >>= 16;
   }

   if ((x & 0xFF) == 0)
    ++len;

   return ((len) < (limit) ? (len) : (limit));
  }

  len += 4;
 }

 return limit;
}

extern uint32_t
lzma_mf_find(lzma_mf *mf, uint32_t *count_ptr, lzma_match *matches)
{
 const uint32_t count = mf->find(mf, matches);
 uint32_t len_best = 0;

 if (count > 0) {
  len_best = matches[count - 1].len;
  if (len_best == mf->nice_len) {
   uint32_t limit = mf_avail(mf) + 1;
   if (limit > mf->match_len_max)
    limit = mf->match_len_max;
   const uint8_t *p1 = mf_ptr(mf) - 1;
   const uint8_t *p2 = p1 - matches[count - 1].dist - 1;
   len_best = lzma_memcmplen(p1, p2, len_best, limit);
  }
 }

 *count_ptr = count;
 ++mf->read_ahead;

 return len_best;
}

static void
normalize(lzma_mf *mf)
{
 const uint32_t subvalue = ((4294967295U) - mf->cyclic_size);

 for (uint32_t i = 0; i < mf->hash_count; ++i) {
  if (mf->hash[i] <= subvalue)
   mf->hash[i] = 0;
  else
   mf->hash[i] -= subvalue;
 }

 for (uint32_t i = 0; i < mf->sons_count; ++i) {
  if (mf->son[i] <= subvalue)
   mf->son[i] = 0;
  else
   mf->son[i] -= subvalue;
 }

 mf->offset -= subvalue;
 return;
}

static void
move_pos(lzma_mf *mf)
{
 if (++mf->cyclic_pos == mf->cyclic_size)
  mf->cyclic_pos = 0;
 ++mf->read_pos;
 if (__builtin_expect(mf->read_pos + mf->offset == (4294967295U), 0 ))
  normalize(mf);
}

static void
move_pending(lzma_mf *mf)
{
 ++mf->read_pos;
 ++mf->pending;
}

static lzma_match *
hc_find_func(
  const uint32_t len_limit,
  const uint32_t pos,
  const uint8_t *const cur,
  uint32_t cur_match,
  uint32_t depth,
  uint32_t *const son,
  const uint32_t cyclic_pos,
  const uint32_t cyclic_size,
  lzma_match *matches,
  uint32_t len_best)
{
 son[cyclic_pos] = cur_match;

 while (1) {
  const uint32_t delta = pos - cur_match;
  if (depth-- == 0 || delta >= cyclic_size)
   return matches;

  const uint8_t *const pb = cur - delta;
  cur_match = son[cyclic_pos - delta
    + (delta > cyclic_pos ? cyclic_size : 0)];

  if (pb[len_best] == cur[len_best] && pb[0] == cur[0]) {
   uint32_t len = lzma_memcmplen(pb, cur, 1, len_limit);

   if (len_best < len) {
    len_best = len;
    matches->len = len;
    matches->dist = delta - 1;
    ++matches;

    if (len == len_limit)
     return matches;
   }
  }
 }
}

extern uint32_t
lzma_mf_hc3_find(lzma_mf *mf, lzma_match *matches)
{
 uint32_t len_limit = mf_avail(mf);
 if (mf->nice_len <= len_limit) {
  len_limit = mf->nice_len;
 } else if (len_limit < (3)) {
  move_pending(mf);
  return 0;
 }
 const uint8_t *cur = mf_ptr(mf);
 const uint32_t pos = mf->read_pos + mf->offset;
 uint32_t matches_count = 0;

 const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
 const uint32_t hash_2_value = temp & ((1U << 10) - 1);
 const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)) & mf->hash_mask;

 const uint32_t delta2 = pos - mf->hash[hash_2_value];
 const uint32_t cur_match = mf->hash[((1U << 10)) + hash_value];

 mf->hash[hash_2_value] = pos;
 mf->hash[((1U << 10)) + hash_value] = pos;

 uint32_t len_best = 2;

 if (delta2 < mf->cyclic_size && *(cur - delta2) == *cur) {
  len_best = lzma_memcmplen(cur - delta2, cur, len_best, len_limit);

  matches[0].len = len_best;
  matches[0].dist = delta2 - 1;
  matches_count = 1;

  if (len_best == len_limit) {
   mf->son[mf->cyclic_pos] = cur_match;
   move_pos(mf);
   return 1;
  }
 }

 matches_count = hc_find_func(len_limit, pos, cur, cur_match, mf->depth,
			      mf->son, mf->cyclic_pos, mf->cyclic_size,
			      matches + matches_count, len_best) - matches;
 move_pos(mf);
 return matches_count;
}

extern void
lzma_mf_hc3_skip(lzma_mf *mf, uint32_t amount)
{
 do {
  if (mf_avail(mf) < 3) {
   move_pending(mf);
   continue;
  }

  const uint8_t *cur = mf_ptr(mf);
  const uint32_t pos = mf->read_pos + mf->offset;

  const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
  const uint32_t hash_2_value = temp & ((1U << 10) - 1);
  const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)) & mf->hash_mask;

  const uint32_t cur_match
    = mf->hash[((1U << 10)) + hash_value];

  mf->hash[hash_2_value] = pos;
  mf->hash[((1U << 10)) + hash_value] = pos;

  do { mf->son[mf->cyclic_pos] = cur_match; move_pos(mf); } while (0);

 } while (--amount != 0);
}

extern uint32_t
lzma_mf_hc4_find(lzma_mf *mf, lzma_match *matches)
{
 uint32_t len_limit = mf_avail(mf);
 if (mf->nice_len <= len_limit) {
  len_limit = mf->nice_len;
 } else if (len_limit < (4)) {
  move_pending(mf);
  return 0;
 }
 const uint8_t *cur = mf_ptr(mf);
 const uint32_t pos = mf->read_pos + mf->offset;
 uint32_t matches_count = 0;

 const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
 const uint32_t hash_2_value = temp & ((1U << 10) - 1);
 const uint32_t hash_3_value = (temp ^ ((uint32_t)(cur[2]) << 8))
				& ((1U << 16) - 1);
 const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)
				      ^ (lzma_crc32_table[0][cur[3]] << 5))
			      & mf->hash_mask;
 uint32_t delta2 = pos - mf->hash[hash_2_value];
 const uint32_t delta3
   = pos - mf->hash[((1U << 10)) + hash_3_value];
 const uint32_t cur_match = mf->hash[((1U << 10) + (1U << 16)) + hash_value];

 mf->hash[hash_2_value ] = pos;
 mf->hash[((1U << 10)) + hash_3_value] = pos;
 mf->hash[((1U << 10) + (1U << 16)) + hash_value] = pos;

 uint32_t len_best = 1;

 if (delta2 < mf->cyclic_size && *(cur - delta2) == *cur) {
  len_best = 2;
  matches[0].len = 2;
  matches[0].dist = delta2 - 1;
  matches_count = 1;
 }

 if (delta2 != delta3 && delta3 < mf->cyclic_size
   && *(cur - delta3) == *cur) {
  len_best = 3;
  matches[matches_count++].dist = delta3 - 1;
  delta2 = delta3;
 }

 if (matches_count != 0) {
  len_best = lzma_memcmplen(cur - delta2, cur,
    len_best, len_limit);

  matches[matches_count - 1].len = len_best;

  if (len_best == len_limit) {
   mf->son[mf->cyclic_pos] = cur_match; move_pos(mf);
   return matches_count;
  }
 }

 if (len_best < 3)
  len_best = 3;

 matches_count = hc_find_func(len_limit, pos, cur, cur_match, mf->depth,
			      mf->son, mf->cyclic_pos, mf->cyclic_size,
			      matches + matches_count, len_best) - matches;
 move_pos(mf);
 return matches_count;
}

extern void
lzma_mf_hc4_skip(lzma_mf *mf, uint32_t amount)
{
 do {
  if (mf_avail(mf) < 4) {
   move_pending(mf);
   continue;
  }

  const uint8_t *cur = mf_ptr(mf);
  const uint32_t pos = mf->read_pos + mf->offset;

  const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
  const uint32_t hash_2_value = temp & ((1U << 10) - 1);
  const uint32_t hash_3_value = (temp ^ ((uint32_t)(cur[2]) << 8)) & ((1U << 16) - 1);
  const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)
				       ^ (lzma_crc32_table[0][cur[3]] << 5))
			       & mf->hash_mask;

  const uint32_t cur_match
    = mf->hash[((1U << 10) + (1U << 16)) + hash_value];

  mf->hash[hash_2_value] = pos;
  mf->hash[((1U << 10)) + hash_3_value] = pos;
  mf->hash[((1U << 10) + (1U << 16)) + hash_value] = pos;

  mf->son[mf->cyclic_pos] = cur_match;
  move_pos(mf);
 } while (--amount != 0);
}

static lzma_match *
bt_find_func(
  const uint32_t len_limit,
  const uint32_t pos,
  const uint8_t *const cur,
  uint32_t cur_match,
  uint32_t depth,
  uint32_t *const son,
  const uint32_t cyclic_pos,
  const uint32_t cyclic_size,
  lzma_match *matches,
  uint32_t len_best)
{
 uint32_t *ptr0 = son + (cyclic_pos << 1) + 1;
 uint32_t *ptr1 = son + (cyclic_pos << 1);

 uint32_t len0 = 0;
 uint32_t len1 = 0;

 while (1) {
  const uint32_t delta = pos - cur_match;
  if (depth-- == 0 || delta >= cyclic_size) {
   *ptr0 = 0;
   *ptr1 = 0;
   return matches;
  }

  uint32_t *const pair = son + ((cyclic_pos - delta
    + (delta > cyclic_pos ? cyclic_size : 0))
    << 1);

  const uint8_t *const pb = cur - delta;
  uint32_t len = ((len0) < (len1) ? (len0) : (len1));

  if (pb[len] == cur[len]) {
   len = lzma_memcmplen(pb, cur, len + 1, len_limit);

   if (len_best < len) {
    len_best = len;
    matches->len = len;
    matches->dist = delta - 1;
    ++matches;

    if (len == len_limit) {
     *ptr1 = pair[0];
     *ptr0 = pair[1];
     return matches;
    }
   }
  }

  if (pb[len] < cur[len]) {
   *ptr1 = cur_match;
   ptr1 = pair + 1;
   cur_match = *ptr1;
   len1 = len;
  } else {
   *ptr0 = cur_match;
   ptr0 = pair;
   cur_match = *ptr0;
   len0 = len;
  }
 }
}


static void
bt_skip_func(
  const uint32_t len_limit,
  const uint32_t pos,
  const uint8_t *const cur,
  uint32_t cur_match,
  uint32_t depth,
  uint32_t *const son,
  const uint32_t cyclic_pos,
  const uint32_t cyclic_size)
{
 uint32_t *ptr0 = son + (cyclic_pos << 1) + 1;
 uint32_t *ptr1 = son + (cyclic_pos << 1);

 uint32_t len0 = 0;
 uint32_t len1 = 0;

 while (1) {
  const uint32_t delta = pos - cur_match;
  if (depth-- == 0 || delta >= cyclic_size) {
   *ptr0 = 0;
   *ptr1 = 0;
   return;
  }

  uint32_t *pair = son + ((cyclic_pos - delta
    + (delta > cyclic_pos ? cyclic_size : 0))
    << 1);
  const uint8_t *pb = cur - delta;
  uint32_t len = ((len0) < (len1) ? (len0) : (len1));

  if (pb[len] == cur[len]) {
   len = lzma_memcmplen(pb, cur, len + 1, len_limit);

   if (len == len_limit) {
    *ptr1 = pair[0];
    *ptr0 = pair[1];
    return;
   }
  }

  if (pb[len] < cur[len]) {
   *ptr1 = cur_match;
   ptr1 = pair + 1;
   cur_match = *ptr1;
   len1 = len;
  } else {
   *ptr0 = cur_match;
   ptr0 = pair;
   cur_match = *ptr0;
   len0 = len;
  }
 }
}

extern uint32_t
lzma_mf_bt2_find(lzma_mf *mf, lzma_match *matches)
{
 uint32_t len_limit = mf_avail(mf);
 if (mf->nice_len <= len_limit) {
  len_limit = mf->nice_len;
 } else if (len_limit < (2) || (mf->action == LZMA_SYNC_FLUSH)) {
  move_pending(mf);
  return 0;
 }
 const uint8_t *cur = mf_ptr(mf);
 const uint32_t pos = mf->read_pos + mf->offset;
 uint32_t matches_count = 0;
 const uint32_t hash_value = read16ne(cur);
 const uint32_t cur_match = mf->hash[hash_value];
 mf->hash[hash_value] = pos;

 matches_count = bt_find_func(len_limit, pos, cur, cur_match, mf->depth,
                              mf->son, mf->cyclic_pos, mf->cyclic_size,
                              matches + matches_count, 1) - matches;
 move_pos(mf);
 return matches_count;
}

extern void
lzma_mf_bt2_skip(lzma_mf *mf, uint32_t amount)
{
 do {
  uint32_t len_limit = mf_avail(mf);
  if (mf->nice_len <= len_limit) {
   len_limit = mf->nice_len;
  } else if (len_limit < (2) || (mf->action == LZMA_SYNC_FLUSH)) { 
   move_pending(mf);
   continue;
  }
  const uint8_t *cur = mf_ptr(mf);
  const uint32_t pos = mf->read_pos + mf->offset;

  const uint32_t hash_value = read16ne(cur);
  const uint32_t cur_match = mf->hash[hash_value];
  mf->hash[hash_value] = pos;

  bt_skip_func(len_limit, pos, cur, cur_match, mf->depth, mf->son,
	       mf->cyclic_pos, mf->cyclic_size);
  move_pos(mf);
 } while (--amount != 0);
}

extern uint32_t
lzma_mf_bt3_find(lzma_mf *mf, lzma_match *matches)
{
 uint32_t len_limit = mf_avail(mf);
 if (mf->nice_len <= len_limit) {
  len_limit = mf->nice_len;
 } else if (len_limit < (3) || (1 && mf->action == LZMA_SYNC_FLUSH)) { 
  move_pending(mf);
  return 0;
 }
 const uint8_t *cur = mf_ptr(mf);
 const uint32_t pos = mf->read_pos + mf->offset;
 uint32_t matches_count = 0;

 const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
 const uint32_t hash_2_value = temp & ((1U << 10) - 1);
 const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)) & mf->hash_mask;

 const uint32_t delta2 = pos - mf->hash[hash_2_value];
 const uint32_t cur_match = mf->hash[((1U << 10)) + hash_value];

 mf->hash[hash_2_value] = pos;
 mf->hash[((1U << 10)) + hash_value] = pos;

 uint32_t len_best = 2;

 if (delta2 < mf->cyclic_size && *(cur - delta2) == *cur) {
  len_best = lzma_memcmplen(
    cur, cur - delta2, len_best, len_limit);

  matches[0].len = len_best;
  matches[0].dist = delta2 - 1;
  matches_count = 1;

  if (len_best == len_limit) {
   bt_skip_func(len_limit, pos, cur, cur_match, mf->depth, mf->son,
		mf->cyclic_pos, mf->cyclic_size);
   move_pos(mf);
   return 1;
  }
 }

 matches_count = bt_find_func(len_limit, pos, cur, cur_match, mf->depth,
			      mf->son, mf->cyclic_pos, mf->cyclic_size,
			      matches + matches_count, len_best) - matches;
 move_pos(mf);
 return matches_count;
}


extern void
lzma_mf_bt3_skip(lzma_mf *mf, uint32_t amount)
{
 do {
  uint32_t len_limit = mf_avail(mf);
  if (mf->nice_len <= len_limit) {
    len_limit = mf->nice_len; }
  else if (len_limit < (3) || (1 && mf->action == LZMA_SYNC_FLUSH)) { 
    move_pending(mf);
    continue;
  }
  const uint8_t *cur = mf_ptr(mf);
  const uint32_t pos = mf->read_pos + mf->offset;

  const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
  const uint32_t hash_2_value = temp & ((1U << 10) - 1);
  const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)) & mf->hash_mask;

  const uint32_t cur_match = mf->hash[((1U << 10)) + hash_value];

  mf->hash[hash_2_value] = pos;
  mf->hash[((1U << 10)) + hash_value] = pos;

  bt_skip_func(len_limit, pos, cur, cur_match, mf->depth, mf->son,
	       mf->cyclic_pos, mf->cyclic_size); 
  move_pos(mf);
 } while (--amount != 0);
}

extern uint32_t
lzma_mf_bt4_find(lzma_mf *mf, lzma_match *matches)
{
 uint32_t len_limit = mf->write_pos - mf->read_pos;
 if (mf->nice_len <= len_limit) {
  len_limit = mf->nice_len;
 } else if (len_limit < (4) || (mf->action == LZMA_SYNC_FLUSH)) {
  ++mf->read_pos;
  ++mf->pending;
  return 0;
 }

 const uint8_t *cur = mf->buffer + mf->read_pos;
 const uint32_t pos = mf->read_pos + mf->offset;
 uint32_t matches_count = 0;

 const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
 const uint32_t hash_2_value = temp & ((1U << 10) - 1);
 const uint32_t hash_3_value = (temp ^ ((uint32_t)(cur[2]) << 8)) & ((1U << 16) - 1);
 const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)
				      ^ (lzma_crc32_table[0][cur[3]] << 5))
			      & mf->hash_mask;

 uint32_t delta2 = pos - mf->hash[hash_2_value];
 const uint32_t delta3 = pos - mf->hash[((1U << 10)) + hash_3_value];
 const uint32_t cur_match = mf->hash[((1U << 10) + (1U << 16)) + hash_value];

 mf->hash[hash_2_value] = pos;
 mf->hash[((1U << 10)) + hash_3_value] = pos;
 mf->hash[((1U << 10) + (1U << 16)) + hash_value] = pos;

 uint32_t len_best = 1;

 if (delta2 < mf->cyclic_size && *(cur - delta2) == *cur) {
  len_best = 2;
  matches[0].len = 2;
  matches[0].dist = delta2 - 1;
  matches_count = 1;
 }

 if (delta2 != delta3 && delta3 < mf->cyclic_size && *(cur - delta3) == *cur) {
  len_best = 3;
  matches[matches_count++].dist = delta3 - 1;
  delta2 = delta3;
 }

 if (matches_count != 0) {
  len_best = lzma_memcmplen(cur, cur - delta2, len_best, len_limit);

  matches[matches_count - 1].len = len_best;

  if (len_best == len_limit) {
    bt_skip_func(len_limit, pos, cur, cur_match, mf->depth, mf->son,
		 mf->cyclic_pos, mf->cyclic_size);
    move_pos(mf);
    return matches_count;
  }
 }

 if (len_best < 3)
  len_best = 3;

 matches_count = bt_find_func(len_limit, pos, cur, cur_match, mf->depth, mf->son,
                              mf->cyclic_pos, mf->cyclic_size,
                              matches + matches_count, len_best) - matches;
 move_pos(mf);
 return matches_count;
}

extern void
lzma_mf_bt4_skip(lzma_mf *mf, uint32_t amount)
{
 do {
  uint32_t len_limit = mf_avail(mf);
  if (mf->nice_len <= len_limit) {
   len_limit = mf->nice_len;
  } else if (len_limit < (4) || (mf->action == LZMA_SYNC_FLUSH)) {
   move_pending(mf);
   continue;
  }

  const uint8_t *cur = mf->buffer + mf->read_pos;
  const uint32_t pos = mf->read_pos + mf->offset;

  const uint32_t temp = lzma_crc32_table[0][cur[0]] ^ cur[1];
  const uint32_t hash_2_value = temp & ((1U << 10) - 1);
  const uint32_t hash_3_value = (temp ^ ((uint32_t)(cur[2]) << 8))
				& ((1U << 16) - 1);
  const uint32_t hash_value = (temp ^ ((uint32_t)(cur[2]) << 8)
				       ^ (lzma_crc32_table[0][cur[3]] << 5))
			       & mf->hash_mask;

  const uint32_t cur_match = mf->hash[((1U << 10) + (1U << 16)) + hash_value];

  mf->hash[hash_2_value] = pos;
  mf->hash[((1U << 10)) + hash_3_value] = pos;
  mf->hash[((1U << 10) + (1U << 16)) + hash_value] = pos;

  bt_skip_func(len_limit, pos, cur, cur_match, mf->depth, mf->son,
	       mf->cyclic_pos, mf->cyclic_size);
  move_pos(mf);
 } while (--amount != 0);
}

static inline void
mf_skip(lzma_mf *mf, uint32_t amount)
{
 if (amount != 0) {
  mf->skip(mf, amount);
  mf->read_ahead += amount;
 }
}

typedef struct lzma_lzma1_encoder_s lzma_lzma1_encoder;
typedef uint16_t probability;

typedef struct {
 probability choice;
 probability choice2;
 probability low[(1 << 4)][(1 << 3)];
 probability mid[(1 << 4)][(1 << 3)];
 probability high[(1 << 8)];
 uint32_t prices[(1 << 4)][((1 << 3) + (1 << 3) + (1 << 8))];
 uint32_t table_size;
 uint32_t counters[(1 << 4)];
} lzma_length_encoder;

typedef struct {
 uint64_t low;
 uint64_t cache_size;
 uint32_t range;
 uint8_t cache;
 size_t count;
 size_t pos;

 enum {
  RC_BIT_0,
  RC_BIT_1,
  RC_DIRECT_0,
  RC_DIRECT_1,
  RC_FLUSH,
 } symbols[58];

 probability *probs[58];
} lzma_range_encoder;


typedef enum {
 STATE_LIT_LIT,
 STATE_MATCH_LIT_LIT,
 STATE_REP_LIT_LIT,
 STATE_SHORTREP_LIT_LIT,
 STATE_MATCH_LIT,
 STATE_REP_LIT,
 STATE_SHORTREP_LIT,
 STATE_LIT_MATCH,
 STATE_LIT_LONGREP,
 STATE_LIT_SHORTREP,
 STATE_NONLIT_MATCH,
 STATE_NONLIT_REP,
} lzma_lzma_state;

typedef struct {
 lzma_lzma_state state;
 _Bool prev_1_is_literal;
 _Bool prev_2;

 uint32_t pos_prev_2;
 uint32_t back_prev_2;

 uint32_t price;
 uint32_t pos_prev;
 uint32_t back_prev;

 uint32_t backs[4];
} lzma_optimal;

struct lzma_lzma1_encoder_s {
 lzma_range_encoder rc;
 lzma_lzma_state state;
 uint32_t reps[4];
 lzma_match matches[(2 + ((1 << 3) + (1 << 3) + (1 << 8)) - 1) + 1];
 uint32_t matches_count;
 uint32_t longest_match_length;
 _Bool fast_mode;
 _Bool is_initialized;
 _Bool is_flushed;
 uint32_t pos_mask;
 uint32_t literal_context_bits;
 uint32_t literal_pos_mask;

 probability literal[(1 << 4)][0x300];
 probability is_match[12][(1 << 4)];
 probability is_rep[12];
 probability is_rep0[12];
 probability is_rep1[12];
 probability is_rep2[12];
 probability is_rep0_long[12][(1 << 4)];
 probability dist_slot[4][(1 << 6)];
 probability dist_special[(1 << (14 / 2)) - 14];
 probability dist_align[(1 << 4)];

 lzma_length_encoder match_len_encoder;
 lzma_length_encoder rep_len_encoder;

 uint32_t dist_slot_prices[4][(1 << 6)];
 uint32_t dist_prices[4][(1 << (14 / 2))];
 uint32_t dist_table_size;
 uint32_t match_price_count;

 uint32_t align_prices[(1 << 4)];
 uint32_t align_price_count;
 uint32_t opts_end_index;
 uint32_t opts_current_index;
 lzma_optimal opts[(1 << 12)];
};

extern void
lzma_lzma_optimum_fast(lzma_lzma1_encoder *restrict coder,
  lzma_mf *restrict mf,
  uint32_t *restrict back_res, uint32_t *restrict len_res)
{
 const uint32_t nice_len = mf->nice_len;

 uint32_t len_main;
 uint32_t matches_count;
 if (mf->read_ahead == 0) {
  len_main = lzma_mf_find(mf, &matches_count, coder->matches);
 } else {
  len_main = coder->longest_match_length;
  matches_count = coder->matches_count;
 }

 const uint8_t *buf = mf_ptr(mf) - 1;
 const uint32_t buf_avail
   = ((mf_avail(mf) + 1) < ((2 + ((1 << 3) + (1 << 3) + (1 << 8)) - 1))
      ? (mf_avail(mf) + 1) : ((2 + ((1 << 3) + (1 << 3) + (1 << 8)) - 1)));

 if (buf_avail < 2) {
  *back_res = (4294967295U);
  *len_res = 1;
  return;
 }

 uint32_t rep_len = 0;
 uint32_t rep_index = 0;

 for (uint32_t i = 0; i < 4; ++i) {
  const uint8_t *const buf_back = buf - coder->reps[i] - 1;
  if ((read16ne(buf) != read16ne(buf_back)))
   continue;
  const uint32_t len = lzma_memcmplen(buf, buf_back, 2, buf_avail);
  if (len >= nice_len) {
   *back_res = i;
   *len_res = len;
   mf_skip(mf, len - 1);
   return;
  }
  if (len > rep_len) {
   rep_index = i;
   rep_len = len;
  }
 }
 if (len_main >= nice_len) {
  *back_res = coder->matches[matches_count - 1].dist + 4;
  *len_res = len_main;
  mf_skip(mf, len_main - 1);
  return;
 }

 uint32_t back_main = 0;
 if (len_main >= 2) {
  back_main = coder->matches[matches_count - 1].dist;
  while (matches_count > 1 && len_main ==
    coder->matches[matches_count - 2].len + 1) {
   if (!(((back_main) >> 7) > (coder->matches[ matches_count - 2].dist)))
    break;
   --matches_count;
   len_main = coder->matches[matches_count - 1].len;
   back_main = coder->matches[matches_count - 1].dist;
  }
  if (len_main == 2 && back_main >= 0x80)
   len_main = 1;
 }

 if (rep_len >= 2) {
  if (rep_len + 1 >= len_main
    || (rep_len + 2 >= len_main
     && back_main > (1U << 9))
    || (rep_len + 3 >= len_main
     && back_main > (1U << 15))) {
   *back_res = rep_index;
   *len_res = rep_len;
   mf_skip(mf, rep_len - 1);
   return;
  }
 }

 if (len_main < 2 || buf_avail <= 2) {
  *back_res = (4294967295U);
  *len_res = 1;
  return;
 }

 coder->longest_match_length = lzma_mf_find(mf,
   &coder->matches_count, coder->matches);

 if (coder->longest_match_length >= 2) {
  const uint32_t new_dist = coder->matches[
    coder->matches_count - 1].dist;

  if ((coder->longest_match_length >= len_main
     && new_dist < back_main)
    || (coder->longest_match_length == len_main + 1
     && !(((new_dist) >> 7) > (back_main)))
    || (coder->longest_match_length > len_main + 1)
    || (coder->longest_match_length + 1 >= len_main
     && len_main >= 3
     && (((back_main) >> 7) > (new_dist)))) {
   *back_res = (4294967295U);
   *len_res = 1;
   return;
  }
 }
 ++buf;
 const uint32_t limit = ((2) > (len_main - 1) ? (2) : (len_main - 1));
 for (uint32_t i = 0; i < 4; ++i) {
  if (memcmp(buf, buf - coder->reps[i] - 1, limit) == 0) {
   *back_res = (4294967295U);
   *len_res = 1;
   return;
  }
 }

 *back_res = back_main + 4;
 *len_res = len_main;
 mf_skip(mf, len_main - 2);
 return;
}

static inline void
rc_bit(lzma_range_encoder *rc, probability *prob, uint32_t bit)
{
 rc->symbols[rc->count] = bit;
 rc->probs[rc->count] = prob;
 ++rc->count;
}

static inline void
rc_bittree(lzma_range_encoder *rc, probability *probs,
  uint32_t bit_count, uint32_t symbol)
{
 uint32_t model_index = 1;

 do {
  const uint32_t bit = (symbol >> --bit_count) & 1;
  rc_bit(rc, &probs[model_index], bit);
  model_index = (model_index << 1) + bit;
 } while (bit_count != 0);
}

static _Bool
encode_init(lzma_lzma1_encoder *coder, lzma_mf *mf)
{
 if (mf->read_pos == mf->read_limit) {
  if (mf->action == LZMA_RUN)
   return 0;
 } else {
  mf_skip(mf, 1);
  mf->read_ahead = 0;
  rc_bit(&coder->rc, &coder->is_match[0][0], 0);
  rc_bittree(&coder->rc, coder->literal[0], 8, mf->buffer[0]);
 }

 coder->is_initialized = 1;

 return 1;
}

static inline uint32_t
mf_position(const lzma_mf *mf)
{
 return mf->read_pos - mf->read_ahead;
}

static inline _Bool
rc_shift_low(lzma_range_encoder *rc,
  uint8_t *out, size_t *out_pos, size_t out_size)
{
 if ((uint32_t)(rc->low) < (uint32_t)(0xFF000000)
   || (uint32_t)(rc->low >> 32) != 0) {
  do {
   if (*out_pos == out_size)
    return 1;

   out[*out_pos] = rc->cache + (uint8_t)(rc->low >> 32);
   ++*out_pos;
   rc->cache = 0xFF;
  } while (--rc->cache_size != 0);
  rc->cache = (rc->low >> 24) & 0xFF;
 }

 ++rc->cache_size;
 rc->low = (rc->low & 0x00FFFFFF) << 8;
 return 0;
}

static inline void
rc_reset(lzma_range_encoder *rc)
{
 rc->low = 0;
 rc->cache_size = 1;
 rc->range = (4294967295U);
 rc->cache = 0;
 rc->count = 0;
 rc->pos = 0;
}

static inline _Bool
rc_encode(lzma_range_encoder *rc,
  uint8_t *out, size_t *out_pos, size_t out_size)
{
 while (rc->pos < rc->count) {
  if (rc->range < (1U << 24)) {
   if (rc_shift_low(rc, out, out_pos, out_size))
    return 1;
   rc->range <<= 8;
  }

  switch (rc->symbols[rc->pos]) {
  case RC_BIT_0: {
   probability prob = *rc->probs[rc->pos];
   rc->range = (rc->range >> 11)
     * prob;
   prob += ((1U << 11) - prob) >> 5;
   *rc->probs[rc->pos] = prob;
   break;
  }

  case RC_BIT_1: {
   probability prob = *rc->probs[rc->pos];
   const uint32_t bound = prob * (rc->range
     >> 11);
   rc->low += bound;
   rc->range -= bound;
   prob -= prob >> 5;
   *rc->probs[rc->pos] = prob;
   break;
  }

  case RC_DIRECT_0:
   rc->range >>= 1;
   break;

  case RC_DIRECT_1:
   rc->range >>= 1;
   rc->low += rc->range;
   break;

  case RC_FLUSH:
   rc->range = (4294967295U);
   do {
    if (rc_shift_low(rc, out, out_pos, out_size))
     return 1;
   } while (++rc->pos < rc->count);

   rc_reset(rc);
   return 0;

  default:
   break;
  }
  ++rc->pos;
 }

 rc->count = 0;
 rc->pos = 0;
 return 0;
}

static inline uint64_t
rc_pending(const lzma_range_encoder *rc)
{
 return rc->cache_size + 5 - 1;
}

static inline void
literal_matched(lzma_range_encoder *rc, probability *subcoder,
  uint32_t match_byte, uint32_t symbol)
{
 uint32_t offset = 0x100;
 symbol += 1U << 8;

 do {
  match_byte <<= 1;
  const uint32_t match_bit = match_byte & offset;
  const uint32_t subcoder_index
    = offset + match_bit + (symbol >> 8);
  const uint32_t bit = (symbol >> 7) & 1;
  rc_bit(rc, &subcoder[subcoder_index], bit);

  symbol <<= 1;
  offset &= ~(match_byte ^ symbol);

 } while (symbol < (1U << 16));
}

static inline void
literal(lzma_lzma1_encoder *coder, lzma_mf *mf, uint32_t position)
{
 const uint8_t cur_byte = mf->buffer[mf->read_pos - mf->read_ahead];
 probability *subcoder  = ((coder->literal)[
   (((position) & (coder->literal_pos_mask))
    << (coder->literal_context_bits))
   + ((uint32_t)(mf->buffer[mf->read_pos - mf->read_ahead - 1])
   >> (8U - (coder->literal_context_bits)))]);

 if (((coder->state) < 7)) {
  rc_bittree(&coder->rc, subcoder, 8, cur_byte);
 } else {
  const uint8_t match_byte
    = mf->buffer[mf->read_pos - coder->reps[0] - 1 - mf->read_ahead];
  literal_matched(&coder->rc, subcoder, match_byte, cur_byte);
 }
 coder->state
   = ((coder->state) <= STATE_SHORTREP_LIT_LIT
      ? STATE_LIT_LIT : ((coder->state) <= STATE_LIT_SHORTREP
			 ? (coder->state) - 3 : (coder->state) - 6));
}

const uint8_t lzma_rc_prices[] = {
         128, 103,  91,  84,  78,  73,  69,  66,
          63,  61,  58,  56,  54,  52,  51,  49,
          48,  46,  45,  44,  43,  42,  41,  40,
          39,  38,  37,  36,  35,  34,  34,  33,
          32,  31,  31,  30,  29,  29,  28,  28,
          27,  26,  26,  25,  25,  24,  24,  23,
          23,  22,  22,  22,  21,  21,  20,  20,
          19,  19,  19,  18,  18,  17,  17,  17,
          16,  16,  16,  15,  15,  15,  14,  14,
          14,  13,  13,  13,  12,  12,  12,  11,
          11,  11,  11,  10,  10,  10,  10,   9,
           9,   9,   9,   8,   8,   8,   8,   7,
           7,   7,   7,   6,   6,   6,   6,   5,
           5,   5,   5,   5,   4,   4,   4,   4,
           3,   3,   3,   3,   3,   2,   2,   2,
           2,   2,   2,   1,   1,   1,   1,   1
};

static inline uint32_t
rc_bit_price(const probability prob, const uint32_t bit)
{
 return lzma_rc_prices[(prob ^ ((0U - bit)
   & ((1U << 11) - 1))) >> 4];
}

static inline uint32_t
rc_bit_0_price(const probability prob)
{
 return lzma_rc_prices[prob >> 4];
}

static inline uint32_t
rc_bit_1_price(const probability prob)
{
 return lzma_rc_prices[(prob ^ ((1U << 11) - 1))
   >> 4];
}

static inline uint32_t
rc_bittree_price(const probability *const probs,
  const uint32_t bit_levels, uint32_t symbol)
{
 uint32_t price = 0;
 symbol += 1U << bit_levels;

 do {
  const uint32_t bit = symbol & 1;
  symbol >>= 1;
  price += rc_bit_price(probs[symbol], bit);
 } while (symbol != 1);

 return price;
}

static void
length_update_prices(lzma_length_encoder *lc, const uint32_t pos_state)
{
 const uint32_t table_size = lc->table_size;
 lc->counters[pos_state] = table_size;

 const uint32_t a0 = rc_bit_0_price(lc->choice);
 const uint32_t a1 = rc_bit_1_price(lc->choice);
 const uint32_t b0 = a1 + rc_bit_0_price(lc->choice2);
 const uint32_t b1 = a1 + rc_bit_1_price(lc->choice2);
 uint32_t *const prices = lc->prices[pos_state];

 uint32_t i;
 for (i = 0; i < table_size && i < (1 << 3); ++i)
  prices[i] = a0 + rc_bittree_price(lc->low[pos_state],
    3, i);

 for (; i < table_size && i < (1 << 3) + (1 << 3); ++i)
  prices[i] = b0 + rc_bittree_price(lc->mid[pos_state],
    3, i - (1 << 3));

 for (; i < table_size; ++i)
  prices[i] = b1 + rc_bittree_price(lc->high, 8,
    i - (1 << 3) - (1 << 3));

 return;
}

static inline void
length(lzma_range_encoder *rc, lzma_length_encoder *lc,
  const uint32_t pos_state, uint32_t len, const _Bool fast_mode)
{
 len -= 2;

 if (len < (1 << 3)) {
  rc_bit(rc, &lc->choice, 0);
  rc_bittree(rc, lc->low[pos_state], 3, len);
 } else {
  rc_bit(rc, &lc->choice, 1);
  len -= (1 << 3);

  if (len < (1 << 3)) {
   rc_bit(rc, &lc->choice2, 0);
   rc_bittree(rc, lc->mid[pos_state], 3, len);
  } else {
   rc_bit(rc, &lc->choice2, 1);
   len -= (1 << 3);
   rc_bittree(rc, lc->high, 8, len);
  }
 }

 if (!fast_mode)
  if (--lc->counters[pos_state] == 0)
   length_update_prices(lc, pos_state);
}

static inline void
rep_match(lzma_lzma1_encoder *coder, const uint32_t pos_state,
  const uint32_t rep, const uint32_t len)
{
 if (rep == 0) {
  rc_bit(&coder->rc, &coder->is_rep0[coder->state], 0);
  rc_bit(&coder->rc,
    &coder->is_rep0_long[coder->state][pos_state],
    len != 1);
 } else {
  const uint32_t distance = coder->reps[rep];
  rc_bit(&coder->rc, &coder->is_rep0[coder->state], 1);

  if (rep == 1) {
   rc_bit(&coder->rc, &coder->is_rep1[coder->state], 0);
  } else {
   rc_bit(&coder->rc, &coder->is_rep1[coder->state], 1);
   rc_bit(&coder->rc, &coder->is_rep2[coder->state],
     rep - 2);

   if (rep == 3)
    coder->reps[3] = coder->reps[2];

   coder->reps[2] = coder->reps[1];
  }

  coder->reps[1] = coder->reps[0];
  coder->reps[0] = distance;
 }

 if (len == 1) {
  coder->state = ((coder->state) < 7 ? STATE_LIT_SHORTREP : STATE_NONLIT_REP);
 } else {
  length(&coder->rc, &coder->rep_len_encoder, pos_state, len,
    coder->fast_mode);
  coder->state = ((coder->state) < 7 ? STATE_LIT_LONGREP : STATE_NONLIT_REP);
 }
}

// This array is constantly initialized in the original code. It's quite big
// so we skip it.
const uint8_t lzma_fastpos[1 << 13];

static inline uint32_t
get_dist_slot(uint32_t dist)
{
 if (dist < (1U << (13 + ((0) + (0) * (13 - 1)))))
  return lzma_fastpos[dist];

 if (dist < (1U << (13 + ((0) + (1) * (13 - 1)))))
  return (uint32_t)(lzma_fastpos[(dist) >> ((0) + (1) * (13 - 1))]) + 2 * ((0) + (1) * (13 - 1));

 return (uint32_t)(lzma_fastpos[(dist) >> ((0) + (2) * (13 - 1))]) + 2 * ((0) + (2) * (13 - 1));
}

static inline void
rc_bittree_reverse(lzma_range_encoder *rc, probability *probs,
  uint32_t bit_count, uint32_t symbol)
{
 uint32_t model_index = 1;
 do {
  const uint32_t bit = symbol & 1;
  symbol >>= 1;
  rc_bit(rc, &probs[model_index], bit);
  model_index = (model_index << 1) + bit;
 } while (--bit_count != 0);
}

static inline void
rc_direct(lzma_range_encoder *rc, uint32_t value, uint32_t bit_count)
{
 do {
  rc->symbols[rc->count++]
    = RC_DIRECT_0 + ((value >> --bit_count) & 1);
 } while (bit_count != 0);
}

static inline void
match(lzma_lzma1_encoder *coder, const uint32_t pos_state,
      const uint32_t distance, const uint32_t len)
{
 coder->state = ((coder->state) < 7 ? STATE_LIT_MATCH : STATE_NONLIT_MATCH);

 length(&coder->rc, &coder->match_len_encoder, pos_state, len,
	coder->fast_mode);

 const uint32_t dist_slot = get_dist_slot(distance);
 const uint32_t dist_state = ((len) < 4 + 2 ? (len) - 2 : 4 - 1);
 rc_bittree(&coder->rc, coder->dist_slot[dist_state], 6, dist_slot);

 if (dist_slot >= 4) {
  const uint32_t footer_bits = (dist_slot >> 1) - 1;
  const uint32_t base = (2 | (dist_slot & 1)) << footer_bits;
  const uint32_t dist_reduced = distance - base;

  if (dist_slot < 14) {
   rc_bittree_reverse(&coder->rc, coder->dist_special + base - dist_slot - 1,
		     footer_bits, dist_reduced);
  } else {
   rc_direct(&coder->rc, dist_reduced >> 4,
     footer_bits - 4);
   rc_bittree_reverse(
     &coder->rc, coder->dist_align,
     4, dist_reduced & ((1 << 4) - 1));
   ++coder->align_price_count;
  }
 }

 coder->reps[3] = coder->reps[2];
 coder->reps[2] = coder->reps[1];
 coder->reps[1] = coder->reps[0];
 coder->reps[0] = distance;
 ++coder->match_price_count;
}

static void
encode_symbol(lzma_lzma1_encoder *coder, lzma_mf *mf,
  uint32_t back, uint32_t len, uint32_t position)
{
 const uint32_t pos_state = position & coder->pos_mask;

 if (back == (4294967295U)) {
  rc_bit(&coder->rc,
    &coder->is_match[coder->state][pos_state], 0);
  literal(coder, mf, position);
 } else {
  rc_bit(&coder->rc,
   &coder->is_match[coder->state][pos_state], 1);

  if (back < 4) {
   rc_bit(&coder->rc, &coder->is_rep[coder->state], 1);
   rep_match(coder, pos_state, back, len);
  } else {
   rc_bit(&coder->rc, &coder->is_rep[coder->state], 0);
   match(coder, pos_state, back - 4, len);
  }
 }
 mf->read_ahead -= len;
}

static void
encode_eopm(lzma_lzma1_encoder *coder, uint32_t position)
{
 const uint32_t pos_state = position & coder->pos_mask;
 rc_bit(&coder->rc, &coder->is_match[coder->state][pos_state], 1);
 rc_bit(&coder->rc, &coder->is_rep[coder->state], 0);
 match(coder, pos_state, (4294967295U), 2);
}

static inline void
rc_flush(lzma_range_encoder *rc)
{
 for (size_t i = 0; i < 5; ++i)
  rc->symbols[rc->count++] = RC_FLUSH;
}

extern void exit (int __status)
 __attribute__ ((__nothrow__ , __leaf__ , __noreturn__));

extern lzma_ret
lzma_lzma_encode(lzma_lzma1_encoder *restrict coder, lzma_mf *restrict mf,
  uint8_t *restrict out, size_t *restrict out_pos,
  size_t out_size, uint32_t limit)
{

 if (!coder->is_initialized && !encode_init(coder, mf))
  return LZMA_OK;

 uint32_t position = mf_position(mf);

 while (1) {
  if (rc_encode(&coder->rc, out, out_pos, out_size)) {
   return LZMA_OK;
  }

  if (limit != (4294967295U)
      && (mf->read_pos - mf->read_ahead >= limit
	 || *out_pos + rc_pending(&coder->rc)
	    >= (1U << 16) - ((1 << 12) + 1)))
   break;

  if (mf->read_pos >= mf->read_limit) {
   if (mf->action == LZMA_RUN)
    return LZMA_OK;


   if (mf->read_ahead == 0)
    break;
  }
  uint32_t len;
  uint32_t back;

  if (coder->fast_mode)
   lzma_lzma_optimum_fast(coder, mf, &back, &len);
  else
   // The original code contains the  call to
   // lzma_lzma_optimum_normal(coder, mf, &back, &len, position);
   exit (-1);

  encode_symbol(coder, mf, back, len, position);

  position += len;
 }

 if (!coder->is_flushed) {
  coder->is_flushed = 1;
  if (limit == (4294967295U))
   encode_eopm(coder, position);

  rc_flush(&coder->rc);

  if (rc_encode(&coder->rc, out, out_pos, out_size)) {
   return LZMA_OK;
  }
 }

 coder->is_flushed = 0;
 return LZMA_STREAM_END;
}

extern void
lzma_free(void *ptr, const lzma_allocator *allocator)
{
 if (allocator != ((void *)0) && allocator->free != ((void *)0))
  allocator->free(allocator->opaque, ptr);
 else
  free(ptr);
 return;
}

static _Bool
lz_encoder_prepare(lzma_mf *mf, const lzma_allocator *allocator,
  const lzma_lz_options *lz_options)
{
 if (lz_options->dict_size < 4096U
   || lz_options->dict_size
    > (1U << 30) + (1U << 29)
   || lz_options->nice_len > lz_options->match_len_max)
  return 1;

 mf->keep_size_before = lz_options->before_size + lz_options->dict_size;
 mf->keep_size_after = lz_options->after_size
   + lz_options->match_len_max;
 uint32_t reserve = lz_options->dict_size / 2;
 if (reserve > (1U << 30))
  reserve /= 2;

 reserve += (lz_options->before_size + lz_options->match_len_max
   + lz_options->after_size) / 2 + (1U << 19);

 const uint32_t old_size = mf->size;
 mf->size = mf->keep_size_before + reserve + mf->keep_size_after;

 if ((mf->buffer != ((void *)0)) && old_size != mf->size) {
  lzma_free(mf->buffer, allocator);
  mf->buffer = ((void *)0);
 }

 mf->match_len_max = lz_options->match_len_max;
 mf->nice_len = lz_options->nice_len;
 mf->cyclic_size = lz_options->dict_size + 1;

 switch (lz_options->match_finder) {
 case LZMA_MF_HC3:
  mf->find = &lzma_mf_hc3_find;
  mf->skip = &lzma_mf_hc3_skip;
  break;

 case LZMA_MF_HC4:
  mf->find = &lzma_mf_hc4_find;
  mf->skip = &lzma_mf_hc4_skip;
  break;

 case LZMA_MF_BT2:
  mf->find = &lzma_mf_bt2_find;
  mf->skip = &lzma_mf_bt2_skip;
  break;

 case LZMA_MF_BT3:
  mf->find = &lzma_mf_bt3_find;
  mf->skip = &lzma_mf_bt3_skip;
  break;

 case LZMA_MF_BT4:
  mf->find = &lzma_mf_bt4_find;
  mf->skip = &lzma_mf_bt4_skip;
  break;

 default:
  return 1;
 }

 const uint32_t hash_bytes = lz_options->match_finder & 0x0F;
 if (hash_bytes > mf->nice_len)
  return 1;

 const _Bool is_bt = (lz_options->match_finder & 0x10) != 0;
 uint32_t hs;

 if (hash_bytes == 2) {
  hs = 0xFFFF;
 } else {
  hs = lz_options->dict_size - 1;
  hs |= hs >> 1;
  hs |= hs >> 2;
  hs |= hs >> 4;
  hs |= hs >> 8;
  hs >>= 1;
  hs |= 0xFFFF;

  if (hs > (1U << 24)) {
   if (hash_bytes == 3)
    hs = (1U << 24) - 1;
   else
    hs >>= 1;
  }
 }

 mf->hash_mask = hs;

 ++hs;
 if (hash_bytes > 2)
  hs += (1U << 10);
 if (hash_bytes > 3)
  hs += (1U << 16);

 const uint32_t old_hash_count = mf->hash_count;
 const uint32_t old_sons_count = mf->sons_count;
 mf->hash_count = hs;
 mf->sons_count = mf->cyclic_size;
 if (is_bt)
  mf->sons_count *= 2;

 if (old_hash_count != mf->hash_count
   || old_sons_count != mf->sons_count) {
  lzma_free(mf->hash, allocator);
  mf->hash = ((void *)0);

  lzma_free(mf->son, allocator);
  mf->son = ((void *)0);
 }

 mf->depth = lz_options->depth;
 if (mf->depth == 0) {
  if (is_bt)
   mf->depth = 16 + mf->nice_len / 2;
  else
   mf->depth = 4 + mf->nice_len / 4;
 }

 return 0;
}

int
main ()
{
  lzma_mf mf;
  lzma_allocator allocator;
  lzma_lz_options lz_options;

  void *coder;
  uint8_t *restrict out;
  size_t *restrict out_pos;
  size_t out_size;

  lz_encoder_prepare(&mf, &allocator, &lz_options);
  return (int) lzma_lzma_encode(coder, &mf, out, out_pos, out_size, (4294967295U));
}


/* { dg-final { scan-wpa-ipa-dump "Save results of indirect call analysis." "icp"} } */
/* { dg-final { scan-wpa-ipa-dump-times "For call" 2 "icp"} } */
/* { dg-final { scan-wpa-ipa-dump-times "Insert 0 prefetch stmt:" 5 "ipa_prefetch"} } */
/* { dg-final { scan-wpa-ipa-dump-times "Insert 1 prefetch stmt:" 4 "ipa_prefetch"} } */
/* { dg-final { scan-wpa-ipa-dump-times "Insert 2 prefetch stmt:" 2 "ipa_prefetch"} } */
