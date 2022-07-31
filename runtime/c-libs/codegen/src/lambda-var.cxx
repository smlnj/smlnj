/// \file lambda-var.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Implementation of the primitive LambdaVar module from cfg.asdl
///
/// \author John Reppy
///

#include "asdl/asdl.hxx"
#include "lambda-var.hxx"
#include <cstdint>
#include <vector>

namespace LambdaVar {

    // LVars are represented by positive integers (the Int63.int type on 64-bit
    // machines).  We use the top 3 bits of the first byte to specify the number
    // of additional bytes in the representation, and the other 5 bits are the
    // most-significant bits in the value.
    //
    lvar read_lvar (asdl::instream & is)
    {
        unsigned char b0 = is.getb();
        uint64_t v = b0 & 0x1F;
        switch (b0 >> 5) {
          case 7: v = (v << 8) | is.getb();
          case 6: v = (v << 8) | is.getb();
          case 5: v = (v << 8) | is.getb();
          case 4: v = (v << 8) | is.getb();
          case 3: v = (v << 8) | is.getb();
          case 2: v = (v << 8) | is.getb();
          case 1: v = (v << 8) | is.getb();
          default:
            break;
        }
	return static_cast<int>(v);
    }

    std::vector<lvar> read_lvar_seq (asdl::instream & is)
    {
        unsigned int len = asdl::read_uint(is);
	std::vector<lvar> result;
        result.reserve(len);
        for (unsigned int i = 0;  i < len;  i++) {
            result.push_back(read_lvar(is));
        }
        return result;
    }


} // namespace LambdaVar
