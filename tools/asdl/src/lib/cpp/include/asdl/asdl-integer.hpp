/*! \file asdl-integer.hpp
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_INTEGER_HPP_
#define _ASDL_INTEGER_HPP_

#ifndef _ASDL_HPP_
#  error do not include "asdl-integer.hpp" directly; instead include "asdl.hpp"
#endif

#if defined(ASDL_USE_GNU_MP)
#  include <gmp.h>
#endif

namespace asdl {

  // multiprecision integers
    class integer {
      public:
#if defined(ASDL_USE_GNU_MP)
	integer () { mpz_init(this->_rep); }
#else
	integer () : _sign(false), _digits{0} { }
#endif
	integer (int32_t a);
	integer (uint32_t a);
	integer (int64_t a);
	integer (uint64_t a);

	integer (instream &is);

	~integer () {
#if defined(ASDL_USE_GNU_MP)
	    mpz_clear(this->_rep);
#endif
	}

	void write (outstream &os) const;

      // return -1 if the number is negative and +1 otherwise
	int getSign () const { return (this->_sign ? -1 : 1); }

      // convert to fixed size integers; the boolean is true if the value is too large
      // to fit in the specified type.
	bool toInt (int &n) const;
	bool toUInt (unsigned int &n) const;
	bool toInt32 (int32_t &n) const;
	bool toInt64 (int64_t &n) const;
	bool toUInt32 (uint32_t &n) const;
	bool toUInt64 (uint64_t &n) const;

      // convert to fixed size integers; if the value is too large, then the result is
      // undefined
	int toInt () const { int n; toInt(n); return n; }
	unsigned int toUInt () const { unsigned int n; toUInt(n); return n; }
	int32_t toInt32 () const { int32_t n; toInt32(n); return n; }
	int64_t toInt64 () const { int64_t n; toInt64(n); return n; }
	uint32_t toUInt32 () const { uint32_t n; toUInt32(n); return n; }
	uint64_t toUInt64 () const { uint64_t n; toUInt64(n); return n; }

      private:
#if defined(ASDL_USE_GNU_MP)
	mpz_ptr _rep;
#else
      // default representation of MP ints is sign + digits in big-endian (MSB to LSB) order
	bool _sign;
	std::vector<uint32_t> _digits;
#endif

	friend  bool operator== (integer const &a, integer const &b);
	friend  bool operator!= (integer const &a, integer const &b);
    };

// FIXME: this function should be a static method of the integer class, but we need to fix the
// reader/writer names in the C++ view to get that to work
    inline integer read_integer (instream & is)
    {
	return integer(is);
    }
    inline void write_integer (outstream & os, integer const &n)
    {
	n.write (os);
    }

#ifdef XXX
    inline integer operator+ (integer const &a)
    {
    }
    inline integer operator- (integer const &a)
    {
    }
    inline integer operator~ (integer const &a)
    {
    }

    inline integer operator+ (integer const &a, integer const &b)
    {
    }
    inline integer operator- (integer const &a, integer const &b)
    {
    }
    inline integer operator* (integer const &a, integer const &b)
    {
    }
    inline integer operator/ (integer const &a, integer const &b)
    {
    }
    inline integer operator% (integer const &a, integer const &b)
    {
    }
    inline integer operator& (integer const &a, integer const &b)
    {
    }
    inline integer operator| (integer const &a, integer const &b)
    {
    }
    inline integer operator^ (integer const &a, integer const &b)
    {
    }

//    __GMP_DEFINE_BINARY_FUNCTION_UI(operator<<, __gmp_binary_lshift)
//    __GMP_DEFINE_BINARY_FUNCTION_UI(operator>>, __gmp_binary_rshift)
#endif // XXX

    inline bool operator== (integer const &a, integer const &b)
    {
	if ((a._sign != b._sign) || (a._digits.size() != b._digits.size()))
	    return false;
	for (int i = 0;  i < a._digits.size();  ++i) {
	    if (a._digits[i] != b._digits[i])
		return false;
	}
	return true;
    }
    inline bool operator!= (integer const &a, integer const &b)
    {
	if ((a._sign != b._sign) || (a._digits.size() != b._digits.size()))
	    return true;
	for (int i = 0;  i < a._digits.size();  ++i) {
	    if (a._digits[i] != b._digits[i])
		return true;
	}
	return false;
    }

#ifdef XXX
    inline bool operator< (integer const &a, integer const &b)
    {
    }
    inline bool operator<= (integer const &a, integer const &b)
    {
    }
    inline bool operator> (integer const &a, integer const &b)
    {
    }
    inline bool operator>= (integer const &a, integer const &b)
    {
    }

    inline integer & operator+= (integer const &a, integer const &b)
    {
    }
    inline integer & operator-= (integer const &a, integer const &b)
    {
    }
    inline integer & operator*= (integer const &a, integer const &b)
    {
    }
    inline integer & operator/= (integer const &a, integer const &b)
    {
    }
    inline integer & operator%= (integer const &a, integer const &b)
    {
    }

    inline integer & operator&= (integer const &a, integer const &b)
    {
    }
    inline integer & operator|= (integer const &a, integer const &b)
    {
    }
    inline integer & operator^= (integer const &a, integer const &b)
    {
    }

//    __GMPZ_DEFINE_COMPOUND_OPERATOR_UI(operator<<=, __gmp_binary_lshift)
//    __GMPZ_DEFINE_COMPOUND_OPERATOR_UI(operator>>=, __gmp_binary_rshift)

//    __GMPZ_DEFINE_INCREMENT_OPERATOR(operator++, __gmp_unary_increment)
//    __GMPZ_DEFINE_INCREMENT_OPERATOR(operator--, __gmp_unary_decrement)

#endif

} // namespace asdl

#endif //! _ASDL_INTEGER_HPP_
