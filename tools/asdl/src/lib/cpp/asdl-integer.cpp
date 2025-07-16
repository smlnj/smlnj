/*! \file asdl-integer.cpp
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#include "asdl/asdl.hpp"

#include <iostream> //DEBUG

namespace asdl {

#if defined(ASDL_USE_GNU_MP)

#  error GNU MP library not supported yet

#else // !ASDL_USE_GNU_MP

    integer::integer (uint32_t n)
      : _sign(false)
    {
	if (n != 0) this->_digits.push_back (n);
    }

    integer::integer (int32_t n)
      : _sign(n < 0)
    {
	if (n != 0) this->_digits.push_back (static_cast<uint32_t>(abs(n)));
    }

    integer::integer (uint64_t n)
      : _sign(false)
    {
	if (n != 0) {
	    if (n < 0x100000000) {
		this->_digits.push_back(uint32_t(n));
	    }
	    else {
		this->_digits.push_back(uint32_t(n >> 32));
		this->_digits.push_back(uint32_t(n));
	    }
	}
    }

    integer::integer (int64_t n)
      : _sign(n < 0)
    {
	if (n != 0) {
	    uint64_t ui = static_cast<uint64_t>(std::abs(n));
	    if (ui < 0x100000000) {
		this->_digits.push_back(uint32_t(ui));
	    }
	    else {
		this->_digits.push_back(uint32_t(ui >> 32));
		this->_digits.push_back(uint32_t(ui));
	    }
	}
    }

    void integer::write (outstream & os) const
    {
/* FIXME */
    }

    integer::integer (instream &is)
    {
      // first we read the sequence of 7-bit digits
	std::vector<unsigned char> digits;
	unsigned char b = is.getb();
	this->_sign = ((b & 0x40) != 0);
	digits.push_back(b & 0x3f);
	while ((b & 0x80) != 0) {
	    b = is.getb();
	    digits.push_back(b & 0x7f);
	};

	b = digits[0];

      // the number of significant bits in the first byte
	int firstBits;
	if (b > 0x1f) firstBits = 6;
	else if (b > 0xf) firstBits = 5;
	else if (b > 0x7) firstBits = 4;
	else if (b > 0x3) firstBits = 3;
	else if (b > 0x2) firstBits = 2;
	else if (b > 0x0) firstBits = 1;
	else firstBits = 0;

      // the total number of bits (not counting the sign) is computed
      // as 7 bits per continuation byte plus the bits required for the
      // first byte
	int nbits = (digits.size() - 1) * 7 + firstBits;

	if (nbits > 0) {
	    this->_digits.reserve((nbits + 31) >> 5);
	  // the number of bits available in the current digit
	    uint32_t availBits = nbits % 32;
	    if (availBits == 0) { availBits = 32; }
	  // initialize the current result digit to the first byte
	    uint32_t w = digits[0];
	    availBits -= firstBits;
	    int idx = 1;
	    while (idx < digits.size()) {
		b = digits[idx];
		if (availBits >= 7) {
		    w = (w << 7) | b;
		    availBits -= 7;
		} else {
		    uint32_t excessBits = 7 - availBits;
		  // first we extract the high availBits from b
		    w = (w << availBits) | (b >> excessBits);
		    this->_digits.push_back(w);
		    availBits = 32 - excessBits;
		    w = b & ((1 << excessBits) - 1);
		}
		idx++;
	    }
	    this->_digits.push_back(w);
	  // here availBits == 0
	}
	// else zero

    }

    bool integer::toInt (int &n) const
    {
#if (SIZEOF_INT == 4)
	return integer::toInt32 (n);
#elif (SIZEOF_INT == 8)
	return integer::toInt64 (n);
#else
#  error unsupported integer size (SIZEOF_INT)
#endif

    } // integer::toInt

    bool integer::toInt32 (int32_t &n) const
    {
	if (this->_digits.size() == 0) {
	    n = 0;
	    return false;
	}
	else if (this->_digits.size() > 1) {
	    return true;
	}
	else if (this->_sign) {
	    if (this->_digits[0] <= 0x80000000) {
		n = - static_cast<int32_t>(this->_digits[0]);
		return false;
	    }
	    else {
		return true;
	    }
	}
	else if (this->_digits[0] < 0x80000000) {
	    n = static_cast<int32_t>(this->_digits[0]);
	    return false;
	}
	else {
	    return true;
	}

    } // integer::toInt32

    bool integer::toInt64 (int64_t &n) const
    {
	if (this->_digits.size() == 0) {
	    n = 0;
	    return false;
	}
	else if (this->_digits.size() == 1) {
	    n = this->_sign
		? - static_cast<int64_t>(this->_digits[0])
		: static_cast<int64_t>(this->_digits[0]);
	    return false;
	}
	else if (this->_digits.size() == 2) {
	  // negative number
	    uint64_t un = (static_cast<uint64_t>(this->_digits[0]) << 32)
		+ static_cast<uint64_t>(this->_digits[1]);
	    if (this->_sign) {
	      // negative number
		if (un <= 0x8000000000000000) {
		    n = - static_cast<int64_t>(un);
		    return false;
		}
	    }
	    else if (un <= 0x7fffffffffffffff) {
		n = static_cast<int64_t>(un);
		return false;
	    }
	}

	return true;

    } // integer::toInt64

    bool integer::toUInt (unsigned int &n) const
    {
#if (SIZEOF_INT == 4)
	return integer::toUInt32 (n);
#elif (SIZEOF_INT == 8)
	return integer::toUInt64 (n);
#else
#  error unsupported integer size (SIZEOF_INT)
#endif

    } // integer::toUInt

    bool integer::toUInt32 (uint32_t &n) const
    {
	if (this->_digits.size() == 0) {
	    n = 0;
	    return false;
	}
	else if (this->_digits.size() == 1) {
	    n = this->_digits[0];
	    return false;
	}

	return true;

    } // integer::toUInt32

    bool integer::toUInt64 (uint64_t &n) const
    {
	if (this->_digits.size() == 0) {
	    n = 0;
	    return false;
	}
	else if (this->_digits.size() == 1) {
	    n = static_cast<uint64_t>(this->_digits[0]);
	    return false;
	}
	else if (this->_digits.size() == 2) {
	    n = (static_cast<uint64_t>(this->_digits[0]) << 32)
		+ static_cast<uint64_t>(this->_digits[1]);
	    return false;
	}

	return true;

    } // integer::toUInt64

#endif // !ASDL_USE_GNU_MP

} // namespace asdl
