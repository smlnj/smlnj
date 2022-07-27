/*! \file asdl.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_HXX_
#define _ASDL_HXX_

#include "config.h"

#include <string>
#include <vector>
#include <memory>

#include "asdl-stream.hxx"
#include "asdl-integer.hxx"
/* TODO: asdl-identifier.hxx */

namespace asdl {

  //! forward declarations of identifier classes
    class identifier;

  //! exception for decoding error
    class decode_exception {
    };

  //! wrapper for immediate values
    template <typename T>
    class box {
      public:
	box (T v) : _v(v) { }
	box (instream &is);
	~box () { }
	T value () const { return this->_v; }

      private:
	T _v;
    };

  //! optional values; the generic instance works for enums
    template <typename T> class option {
      public:
	option () : _v(0) { }
	option (T v) : _v(static_cast<unsigned int>(v)) { }
	~option () { }
	bool isEmpty () const { return (this->_v == 0); }
	T valOf () const { return static_cast<T>(this->_v); }
      private:
	T _v;
    };

  //! optional int values
    template <> class option <int> {
      public:
	option () : _isEmpty(true), _v(0) { }
	option (int v) : _isEmpty(false), _v(v) { }
	~option () { }
	bool isEmpty () const { return this->_isEmpty; }
	int valOf () const { return this->_v; }
      private:
	int _v;
	bool _isEmpty;
    };

  //! optional unsigned int values
    template <> class option <unsigned int> {
      public:
	option () : _isEmpty(true), _v(0) { }
	option (unsigned int v) : _isEmpty(false), _v(v) { }
	~option () { }
	bool isEmpty () const { return this->_isEmpty; }
	unsigned int valOf () const { return this->_v; }
      private:
	unsigned int _v;
	bool _isEmpty;
    };

  //! optional pointer values
    template <typename T> class option <T *> {
      public:
	option () : _v(nullptr) { }
	option (T *v) : _v(v) { }
	~option () { }
	bool isEmpty () const { return (this->_v == nullptr); }
	T valOf () const { return this->_v; }
      private:
	T *_v;
    };

  //! optional integer values
    template <> class option <integer> {
      public:
	option () : _isEmpty(true), _v() { }
	option (integer const &n) : _isEmpty(false), _v(n) { }
	~option () { }
	bool isEmpty () const { return this->_isEmpty; }
	integer valOf () const { return this->_v; }
      private:
	integer _v;
	bool _isEmpty;
    };

  //! optional string values
    template <> class option <std::string> {
      public:
	option () : _isEmpty(true), _v("") { }
	option (std::string const &s) : _isEmpty(false), _v(s) { }
	~option () { }
	bool isEmpty () const { return this->_isEmpty; }
	std::string valOf () const { return this->_v; }
      private:
	std::string _v;
	bool _isEmpty;
    };

  /***** functions *****/

  // encode basic values
    void write_int (outstream & os, int i);
    void write_uint (outstream & os, unsigned int ui);
    inline void write_tag8 (outstream & os, unsigned int ui)
    {
	os.putb(static_cast<unsigned char>(ui));
    }
    inline void write_bool (outstream & os, bool b)
    {
	if (b) { write_tag8(os, 2); } else { write_tag8(os, 1); }
    }
    inline void write_tag (outstream & os, unsigned int ui)
    {
	write_uint(os, ui + 1);
    }
    void write_string (outstream & os, std::string const & s);
    void write_integer (outstream & os, integer const & i);
  // encode optional basic values
    inline void write_int_option (outstream & os, option<int> & optB)
    {
	if (optB.isEmpty()) {
	    write_tag8 (os, 0);
	} else {
	    write_int (os, optB.valOf());
	}
    }
    inline void write_uint_option (outstream & os, option<unsigned int> & optB)
    {
	if (optB.isEmpty()) {
	    write_tag8 (os, 0);
	} else {
	    write_uint (os, optB.valOf());
	}
    }
    inline void write_bool_option (outstream & os, option<bool> & optB)
    {
	if (optB.isEmpty()) {
	    write_tag8 (os, 0);
	} else {
	    write_bool (os, optB.valOf());
	}
    }
    inline void write_integer_option (outstream & os, option<integer> & optB)
    {
	if (optB.isEmpty()) {
	    write_tag8 (os, 0);
	} else {
	    write_integer (os, optB.valOf());
	}
    }
    inline void write_string_option (outstream & os, option<std::string> & optB)
    {
	if (optB.isEmpty()) {
	    write_tag8 (os, 0);
	} else {
	    write_string (os, optB.valOf());
	}
    }
    void write_integer_option (outstream & os, option<integer> & optB);
  // generic pickler for boxed options
    template <typename T>
    inline void write_option (outstream & os, T *v)
    {
	if (v == nullptr) {
	    os.putb (0);
	} else {
	    os.putb (1);
	    v->write (os);
	}
    }
  // generic pickler for enumeration sequences with fewer than 256 constructors
    template <typename T>
    inline void write_small_enum_seq (outstream & os, std::vector<T> & seq)
    {
	write_uint (os, seq.size());
	for (auto it = seq.cbegin(); it != seq.cend(); ++it) {
	    write_tag8 (os, static_cast<unsigned int>(*it));
	}
    }
  // generic pickler for enumeration sequences with more than 256 constructors
    template <typename T>
    inline void write_big_enum_seq (outstream & os, std::vector<T> & seq)
    {
	write_uint (os, seq.size());
	for (auto it = seq.cbegin(); it != seq.cend(); ++it) {
	    write_uint (os, static_cast<unsigned int>(*it));
	}
    }
  // generic pickler for boxed sequences
    template <typename T>
    inline void write_seq (outstream & os, std::vector<T> & seq)
    {
	write_uint (os, seq.size());
	for (auto it = seq.cbegin(); it != seq.cend(); ++it) {
	    (*it)->write (os);
	}
    }

  // decode basic values
    int read_int (instream & is);
    unsigned int read_uint (instream & is);
    inline unsigned int read_tag8 (instream & is)
    {
	return is.getb();
    }
    inline bool read_bool (instream & is)
    {
	return (read_tag8(is) != 1);
    }
    inline unsigned int read_tag (instream & is)
    {
	return read_uint (is);
    }
    std::string read_string (instream & is);
    integer read_integer (instream & is);
  // decode optional basic values
    inline option<int> read_int_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return option<int>();
	} else {
	    return option<int>(read_int(is));
	}
    }
    inline option<uint> read_uint_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return option<uint>();
	} else {
	    return option<uint>(read_uint(is));
	}
    }
    inline option<bool> read_bool_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return option<bool>();
	} else {
	    return option<bool>(v != 1);
	}
    }
    inline option<std::string> read_string_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return option<std::string>();
	} else {
	    return option<std::string>(read_string(is));
	}
    }
    inline option<integer> read_integer_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return option<integer>();
	} else {
	    return option<integer>(read_integer(is));
	}
    }
  // generic unpickler for boxed options
    template <typename T>
    inline T *read_option (instream & is)
    {
	unsigned int tag = is.getb ();
	if (tag == 0) {
	    return nullptr;
	} else {
	    return T::read (is);
	}
    }
  // generic pickler for enumeration sequences with fewer than 256 constructors
    template <typename T>
    inline std::vector<T> read_small_enum_seq (instream & is)
    {
	unsigned int len = read_uint (is);
	std::vector<T> seq;
	seq.reserve(len);
	for (unsigned int i = 0;  i < len;  ++i) {
	    seq.push_back (static_cast<T>(read_tag8(is)));
	}
	return seq;
    }
  // generic pickler for enumeration sequences with more than 256 constructors
    template <typename T>
    inline std::vector<T> read_big_enum_seq (instream & is)
    {
	unsigned int len = read_uint (is);
	std::vector<T> seq;
	seq.reserve(len);
	for (unsigned int i = 0;  i < len;  ++i) {
	    seq.push_back (static_cast<T>(read_uint(is)));
	}
	return seq;
    }
  // generic unpickler for boxed sequences
    template <typename T>
    inline std::vector<T *> read_seq (instream & is)
    {
	unsigned int len = read_uint (is);
	std::vector<T *> seq;
	seq.reserve(len);
	for (unsigned int i = 0;  i < len;  ++i) {
	    seq.push_back (T::read(is));
	}
	return seq;
    }
    inline std::vector<int> read_int_seq (instream & is)
    {
	unsigned int len = read_uint (is);
	std::vector<int> seq;
	seq.reserve(len);
	for (unsigned int i = 0;  i < len;  ++i) {
	    seq.push_back (read_int(is));
	}
	return seq;
    }
    inline std::vector<unsigned int> read_uint_seq (instream & is)
    {
	unsigned int len = read_uint (is);
	std::vector<unsigned int> seq;
	seq.reserve(len);
	for (unsigned int i = 0;  i < len;  ++i) {
	    seq.push_back (read_uint(is));
	}
	return seq;
    }

} // namespace asdl

#endif /* !_ASDL_HXX_ */
