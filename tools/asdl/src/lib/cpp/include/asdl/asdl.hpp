/*! \file asdl.hpp
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_HPP_
#define _ASDL_HPP_

#include "config.h"

#include <string>
#include <string_view>
#include <vector>
#include <optional>
#include <memory>
#include <ios>
#include <istream>
#include <ostream>
#include <cstdint>

#include "asdl-stream.hpp"
#include "asdl-integer.hpp"
/* TODO: asdl-identifier.hpp */

namespace asdl {

    //! forward declarations of identifier classes
    class identifier;

    //! the "unit" type, which is used to represent singleton enums
    struct unit {
        unit () { }
    };

  /***** functions *****/

    /** encode basic values **/

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

    /** encode optional basic values **/

    //! optional singleton enum
    inline void write_tag0_option (outstream & os, std::optional<unit> const & optB)
    {
	if (optB.has_value()) {
	    write_tag8 (os, 1);
	} else {
	    write_tag8 (os, 0);
	}
    }
    inline void write_tag8_option (outstream & os, std::optional<unsigned int> const & optB)
    {
	if (optB.has_value()) {
	    write_tag8 (os, optB.value());
	} else {
	    write_tag8 (os, 0);
	}
    }
    inline void write_tag_option (outstream & os, std::optional<unsigned int> const & optB)
    {
	if (optB.has_value()) {
	    write_tag (os, optB.value());
	} else {
	    write_tag (os, 0);
	}
    }
    inline void write_int_option (outstream & os, std::optional<int> const & optB)
    {
	if (optB.has_value()) {
	    write_tag8 (os, 1);
	    write_int (os, optB.value());
	} else {
	    write_tag8 (os, 0);
	}
    }
    inline void write_uint_option (outstream & os, std::optional<unsigned int> const & optB)
    {
	if (optB.has_value()) {
	    write_tag8 (os, 1);
	    write_uint (os, optB.value());
	} else {
	    write_tag8 (os, 0);
	}
    }
    inline void write_bool_option (outstream & os, std::optional<bool> const & optB)
    {
	if (optB.has_value()) {
	    write_bool (os, optB.value());
	} else {
	    write_tag8 (os, 0);
	}
    }
    inline void write_integer_option (outstream & os, std::optional<integer> const & optB)
    {
	if (optB.has_value()) {
	    write_integer (os, optB.value());
	} else {
	    write_tag8 (os, 0);
	}
    }
    inline void write_string_option (outstream & os, std::optional<std::string> const & optB)
    {
	if (optB.has_value()) {
	    write_string (os, optB.value());
	} else {
	    write_tag8 (os, 0);
	}
    }
    void write_integer_option (outstream & os, std::optional<integer> const & optB);
  // generic pickler for optional enumerations with fewer than 256 constructors
    template <typename T>
    inline void write_small_enum_option (outstream & os, std::optional<T> & opt)
    {
        if (opt.has_value()) {
            write_tag8 (os, static_cast<unsigned int>(opt.value()));
        } else {
	    write_tag8 (os, 0);
	}
    }
  // generic pickler for optional enumerations with more than 255 constructors
    template <typename T>
    inline void write_big_enum_option (outstream & os, std::optional<T> & opt)
    {
        if (opt.has_value()) {
            write_uint (os, static_cast<unsigned int>(opt.value()));
        } else {
	    write_uint (os, 0);
	}
    }
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
  // generic pickler for enumeration sequences with more than 255 constructors
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
        unsigned int tag = read_tag8(is);
        switch (tag) {
            case 1: return false;
            case 2: return true;
            default: is.invalidTag(tag, "bool");
        }
    }
    inline unsigned int read_tag (instream & is)
    {
	return read_uint (is);
    }
    std::string read_string (instream & is);
    integer read_integer (instream & is);
  // decode optional basic values
    inline std::optional<unit> read_tag0_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return std::optional<unit>();
	} else {
	    return std::optional<unit>(unit());
	}
    }
    inline std::optional<unsigned int> read_tag8_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return std::optional<unsigned int>();
	} else {
	    return std::optional<unsigned int>(v);
	}
    }
    inline std::optional<unsigned int> read_tag_option (instream & is)
    {
	unsigned int v = read_tag8(is);
	if (v == 0) {
	    return std::optional<unsigned int>();
	} else {
	    return std::optional<unsigned int>(v);
	}
    }
    inline std::optional<int> read_int_option (instream & is)
    {
	unsigned int tag = read_tag8(is);
        switch (tag) {
            case 0: return std::optional<int>();
            case 1: return std::optional<int>(read_int(is));
            default: is.invalidTag(tag, "int?");
        }
    }
    inline std::optional<unsigned int> read_uint_option (instream & is)
    {
	unsigned int tag = read_tag8(is);
        switch (tag) {
            case 0: return std::optional<unsigned int>();
            case 1: return std::optional<unsigned int>(read_uint(is));
            default: is.invalidTag(tag, "uint?");
        }
    }
    inline std::optional<bool> read_bool_option (instream & is)
    {
	unsigned int v = read_tag8(is);
        switch (v) {
            case 0: return std::optional<bool>();
            case 1: return std::optional<bool>(false);
            case 2: return std::optional<bool>(true);
            default: is.invalidTag(v, "bool?");
        }
    }
    inline std::optional<std::string> read_string_option (instream & is)
    {
	unsigned int tag = read_tag8(is);
        switch (tag) {
            case 0: return std::optional<std::string>();
            case 1: return std::optional<std::string>(read_string(is));
            default: is.invalidTag(tag, "string?");
        }
    }
    inline std::optional<integer> read_integer_option (instream & is)
    {
	unsigned int tag = read_tag8(is);
        switch (tag) {
            case 0: return std::optional<integer>();
            case 1: return std::optional<integer>(read_integer(is));
            default: is.invalidTag(tag, "integer?");
        }
    }
  // generic pickler for optional enumerations with fewer than 256 constructors
    template <typename T>
    inline std::optional<T> read_small_enum_option (instream & is)
    {
	unsigned int tag = read_tag8(is);
        if (tag != 0) {
            return std::optional<T>(static_cast<T>(tag));
        } else {
	    return std::optional<T>();
	}
    }
  // generic pickler for optional enumerations with more than 255 constructors
    template <typename T>
    inline std::optional<T> read_big_enum_option (instream & is)
    {
	unsigned int tag = read_uint(is);
        if (tag != 0) {
            return std::optional<T>(static_cast<T>(tag));
        } else {
	    return std::optional<T>();
	}
    }
  // generic unpickler for boxed options
    template <typename T>
    inline T *read_option (instream & is)
    {
	unsigned int tag = read_tag8(is);
        switch (tag) {
            case 0: return nullptr;
            case 1: return T::read (is);
            default: is.invalidTag(tag, "option");
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

#endif /* !_ASDL_HPP_ */
