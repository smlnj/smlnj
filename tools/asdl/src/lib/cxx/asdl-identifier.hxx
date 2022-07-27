/*! \file asdl-identifier.hxx
 *
 * \author John Reppy
 *
 * C++ representation of the ASDL identifier type, which is a string type with
 * fast equality checking.
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_IDENTIFIER_HXX_
#define _ASDL_IDENTIFIER_HXX_

#include <unordered_map>
#include <memory>
#include <string>

#include "asdl-stream.hxx"

namespace asdl {

    class identifier {
      public:
	identifier (std::string const &s) : _s(_resolve(s)) { }
	identifier (const char *cp, int n) : _s (_resolve(std::string(cp, n))) { }

      // pickle/unpickle operations
	asdl::ostream pickle (asdl::ostream &os) { return (os.wr_string(this->_str()); }

	identifier (asdl::istream &is)
	  : _s(_resolve(is.rd_string()))
	{ }

      // get a copy of the identifier
	std::string to_string () const { return std::string(this->_str()); }

      // wrappers around std::string operations
	size_t size() const noexcept { return this->_str().size(); }
	size_t length() const noexcept { return this->_str().length(); }
	const char& operator[] (size_t pos) const { return this->_str()[pos]; }
	const char& at (size_t pos) const { return this->_str().at(pos); }

	int compare (const identifier& id) const noexcept
	{
	    return (this->_s == id._s) ? 0 : this->_str().compare(id._str());
	}
	int compare (const std::string& str) const noexcept
	{
	    return this->_str().compare(str);
	}
	int compare (size_t pos, size_t len, const std::string& str) const
	{
	    return this->_str().compare(pos, len, str);
	}
	int compare (size_t pos, size_t len, const std::string& str, size_t subpos, size_t sublen) const
	{
	    return this->_str().compare(pos, len, str, subpos, sublen);
	}
	int compare (const char* s) const
	{
	    return this->_str().compare(s);
	}
	int compare (size_t pos, size_t len, const char* s) const
	{
	    return this->_str().compare(pos, len, s);
	}
	int compare (size_t pos, size_t len, const char* s, size_t n) const
	{
	    return this->_str().compare(pos, len, s, n);
	}

      private:
      // a thin wrapper around the canonical representation of the identifier.
      // we need this wrapper for its destructor, which is invoked by shared_ptr
      // when the last reference to the identifier is destroyed
	struct wrapper {
	    const std::string _s;
	    wrapper (std::string const &s) : _s(s) { }
	    ~wrapper ()
	    {
		_Tbl.erase (this->_s);
	    }
	};

	typedef std::shared_ptr<const wrapper> rep_t;
	typedef std::unordered_map<std::string, std::weak_ptr<const wrapper> > table_t;

	rep_t _s;

	std::string const &_str () const { return this->_s->_s; }

      // global hash table for uniquifying identifiers
	static table_t _Tbl;

      // resolve a string to its unique representation
	static rep_t _resolve (std::string const &s)
	{
	    auto got = _Tbl.find(s);
	    if (got == _Tbl.end()) {
		rep_t rep = std::make_shared<const wrapper>(s);
		_Tbl.insert(std::pair<std::string,rep_t>(s, std::weak_ptr<const wrapper>(rep)));
		return rep;
	    }
	    else {
		return got->second.lock();
	    }
	}

	friend bool operator== (identifier &id1, identifier &id2);
	friend bool operator!= (identifier &id1, identifier &id2);
    };

    bool operator== (identifier &id1, identifier &id2) { return id1._s == id2._s; }
    bool operator!= (identifier &id1, identifier &id2) { return id1._s != id2._s; }

  // output an identifier to a text stream
    std::ostream &operator<< (std::ostream &os, identifier const &g)
    {
	return os << g._str
}

#endif // !_ASDL_IDENTIFIER_HXX_
