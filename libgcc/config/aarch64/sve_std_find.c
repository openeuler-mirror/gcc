#include <arm_sve.h>
#include <stdint.h>

#pragma GCC target ("+sve")

uint64_t *__sve_optimized_find_u64 (uint64_t *first, uint64_t *last,
	uint64_t const *value, uint8_t threshold)
{
	if (first + threshold > last)
	{
		goto Tail;
	}

	uint64_t m = svcntd ();
	uint64_t n = (last - first) / m;
	svbool_t TRUE = svptrue_b64 ();
	for (; n-- > 0;)
	{
		svuint64_t v3 = svld1_u64 (TRUE, (uint64_t *)first);
		svbool_t v4 = svcmpeq_n_u64 (TRUE, v3, (uint64_t)*value);
		if (svptest_any (TRUE, v4))
		{
			break;
		}
		first += m;
	}

Tail:
	while (first < last)
	{
		if (*first == *value)
		{
			return first;
		}
		++first;
	}
	return last;
}
