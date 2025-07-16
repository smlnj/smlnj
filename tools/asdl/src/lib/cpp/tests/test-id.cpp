/*! \file test-id.cxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include <iostream>
#include  "../asdl-identifier.hxx"

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
