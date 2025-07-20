/*! \file test-id.cpp
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#include <iostream>
#include  "../asdl-identifier.hpp"

asdl::identifier::table_t asdl::identifier::_Tbl;

int main ()
{
    asdl::identifier id1("hello");
    asdl::identifier id2("world");
    asdl::identifier id3("hello");

    std::cout << std::boolalpha
	<< "(id1 == id1) = " << (id1 == id1) << "\n"
	<< "(id1 == id2) = " << (id1 == id2) << "\n"
	<< "(id1 == id3) = " << (id1 == id3) << "\n";

    return 0;
}
