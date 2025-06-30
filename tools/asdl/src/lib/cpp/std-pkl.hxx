/*! \file std-pkl.hxx
 *
 * Input/output sources for ASDL pickles.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_STD_PKL_HXX_
#define _ASDL_STD_PKL_HXX_

#include <cstdint>

namespace asdl {

    //! byte source for unpickling
    class in_stream {
      public:
	int8_t int8 ()   { return static_cast<int8_t>(this->_signed32()); }
	int16_t int16 () { return static_cast<int16_t>(this->_signed32()); }
	int32_t int32 () { return this->_signed32(); }
	int64_t int64 () { return this->_signed64(); }

	uint8_t uint8 ()   { return static_cast<uint8_t>(this->_unsigned32()); }
	uint16_t uint16 () { return static_cast<uint16_t>(this->_unsigned32()); }
	uint32_t uint32 () { return this->_unsigned32(); }
	uint64_t uint64 () { return this->_unsigned64(); }

	big_int big_int ();
	bool boolean ();
	float float32 ();
	double float64 ();

	in_stream (std::instream *inS);

      private:
	std::in_stream *_inS;

	int32_t _signed32 ();
	uint32_t _unsigned32 ();
	int64_t _signed64 ();
	uint64_t _unsigned64 ();
    };

    //! byte source from memory
    class in_memory {
      public:

      private:
	uint8_t *_datap;	//!< current data pointer
	uint8_t *_end;		//!< end of input buffer

    };

    //! byte sink for pickling
    class out_stream {
      public:
	void int8 (int8_t v);
	void int16 (int16_t v);
	void int32 (int32_t v);
	void int64 (int64_t v);

	void uint8 (uint8_t v);
	void uint16 (uint16_t v);
	void uint32 (uint32_t v);
	void uint64 (uint64_t v);

	void big_int (big_int v);
	void boolean (bool v);
	void float32 (float v);
	void float64 (double v);

	out_stream (std::outstream *outS);

      private:
	std::out_stream *_outS;
    };

} // namespace asdl

#endif // !_ASDL_STD_PKL_HXX_
