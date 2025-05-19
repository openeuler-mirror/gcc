/* { dg-do compile } */
/* { dg-options "-std=c++11 -O3 -ffind-with-sve -march=armv8-a+sve -fdump-tree-optimized" } */

#include <algorithm>
#include <vector>
#include <iostream>
#include <cstdint>
#include <cassert>
#include <string>
#include <set>

void test_u64()
{
    std::vector<std::uint64_t> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};

    std::uint64_t x = 100;
    std::cin >> x;

    auto it = std::find(v.begin(), v.end(), x);  // matched : No.1

    if (it != v.end())
	std::cout << "ok!\n";
    else
	std::cout << "fail!\n";
}

void test_s64()
{
    std::vector<std::int64_t> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};

    std::int64_t x = 100;
    std::cin >> x;

    auto it = std::find(v.begin(), v.end(), x);  // matched : No.2

    if (it != v.end())
	std::cout << "ok!\n";
    else
	std::cout << "fail!\n";
}

void test_array()
{
    const unsigned N = 1024 * 1024 * 16;
    long *arr = new long[N];
    long *p;
    for (unsigned i = 0; i < N; ++i)
	arr[i] = i;
    for (unsigned i = N - 1000; i < N - 1; ++i) {
	p = std::find(arr, arr + N, arr[i]);  // matched : No.3
	assert(p == arr + i);
	unsigned j = i - 10;
	p = std::find(arr + j, arr + j + 1, arr[j]);  // matched : No.4
	assert(p == arr + j);
	p = std::find(arr + j + 1, arr + j + 1, arr[j + 2]);  // matched : No.5
	assert(p == arr + j + 1);
	p = std::find(arr + j + 2, arr + j + 1, arr[j + 2]);  // matched : No.6
	assert(p == arr + j + 1);
    }
    p = std::find(arr, arr + N, (long)-1);  // matched : No.7
    assert(p == arr + N);
}

void test_string()
{
    std::vector<std::string> v;
    for (int i = 0; i < 5; i++)
	v.push_back(std::to_string(123 + i));

    for (int i = 0; i < 5; i++) {
	auto it = std::find(v.begin(), v.end(), std::to_string(124 + i));  // not matched

	if (it != v.end())
	    std::cout << "ok!\n";
	else
	    std::cout << "failed!\n";
    }
}

void test_s32()
{
    std::vector<std::int32_t> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};

    std::int32_t x = 100;

    auto it = std::find(v.begin(), v.end(), x);  // not matched

    if (it != v.end())
	std::cout << "ok!\n";
    else
	std::cout << "failed!\n";
}

void test_u16()
{
    std::vector<std::uint16_t> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};

    std::uint16_t x = 100;

    auto it = std::find(v.begin(), v.end(), x);  // not matched

    if (it != v.end())
	std::cout << "ok!\n";
    else
	std::cout << "failed!\n";
}

void test_u16_point()
{
    std::vector<std::uint16_t> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    std::vector<std::uint16_t *> v_ptr;

    for (auto &item : v)
	v_ptr.push_back(&item);

    std::uint16_t x = 100;

    auto it = std::find(v_ptr.begin(), v_ptr.end(), &x);  // matched : No.8

    if (it != v_ptr.end())
	std::cout << "ok!\n";
    else
	std::cout << "fail!\n";
}

void test_set()
{
	std::set<std::uint64_t> s = {1, 3, 5, 7, 9};
	std::uint64_t ask = 4;

	if (auto it = std::find (s.begin(), s.end(), ask); it != s.end()) // not matched
		std::cout << "ok!\n";
	else
		std::cout << "fail!\n";
}

namespace myspace
{
    namespace std
    {
        struct Basic
        {
            ::std::uint64_t id;
        };

        ::std::uint64_t find(Basic *, Basic *, ::std::uint64_t &x)
        {
            return x;
        }
    }
}

void test_namespace()
{
    myspace::std::Basic b {1};
    std::uint64_t y = 1;
    std::uint64_t x = find(nullptr, &b, y);
    printf("x = %d\n", x);
}

int main()
{
    test_u64();
    test_s64();
    test_array();
    test_string();
    test_s32();
    test_u16();
    test_u16_point();
    test_set();
    test_namespace();
    return 0;
}

/* { dg-final { scan-tree-dump-times "__sve_optimized_find_u64" 8 "optimized" } } */
