// test of C++ code for reading asdl::integer pickles
//
// to build:
//	clang++ -g --std=c++14 -o read-test read-test.cxx -L../../../lib -lasdl

#include "../../../../include/asdl/asdl.hxx"
#include <iostream>

struct trial {
    std::string name;
    int nbytes;
    unsigned char data[8];
};

#define N_TRIALS 10

trial trials[N_TRIALS] = {
	{ "-1", 1, { 0x41 } },
	{ "0", 1, { 0x00 } },
	{ "1", 1, { 0x01 } },
	{ "15", 1, { 0x0F } },
	{ "63", 1, { 0x3F } },
	{ "64", 2, { 0x80, 0x40 } },
	{ "127", 2, { 0x80, 0x7F } },
	{ "128", 2, { 0x81, 0x00 } },
	{ "1000", 2, { 0x87, 0x68 } },
	{ "2147483647", 5, { 0x87, 0xFF, 0xFF, 0xFF, 0x7F } },
    };

void test (trial &t)
{
    std::string data((char *)t.data, t.nbytes);
    asdl::memory_instream is(data);
    auto n = asdl::read_integer (is);
    std::cout << t.name << " == " << n.toInt32() << "\n";
}

int main ()
{
    for (int i = 0;  i < N_TRIALS;  ++i) {
	test (trials[i]);
    }

    return 0;
}
