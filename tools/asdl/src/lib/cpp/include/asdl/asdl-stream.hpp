/// \file asdl-stream.hpp
///
/// \copyright 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
/// All rights reserved.
///
/// \brief Defines the input and output stream types for ASDL picklers.
///
/// \author John Reppy
///

#ifndef _ASDL_STREAM_HPP_
#define _ASDL_STREAM_HPP_

#ifndef _ASDL_HPP_
#  error do not include "asdl-stream.hpp" directly; instead include "asdl.hpp"
#endif

namespace asdl {

//! ASDL output stream
class outstream {
public:
    explicit outstream (std::ostream *os) : _os(os) { }

  // no copying allowed!
    outstream (outstream const &) = delete;
    outstream &operator= (outstream const &) = delete;

  // move operations
    outstream (outstream &&os) noexcept
    {
        this->_os = os._os;
        os._os = nullptr;
    }
    outstream &operator= (outstream &&rhs) noexcept
    {
        if (this != &rhs) {
            this->_os = rhs._os;
            rhs._os = nullptr;
        }
        return *this;
    }

    ~outstream ()
    {
        if (this->_os != nullptr) {
            delete this->_os;
        }
    }

    void putc (char c) { this->_os->put(c); }
    void putb (unsigned char c) { this->_os->put(c); }

  protected:
    std::ostream *_os;
};

//! ASDL file outstream
class file_outstream : public outstream {
public:
    explicit file_outstream (std::string const &file);

  // no copying allowed!
    file_outstream (file_outstream const &) = delete;
    file_outstream &operator= (file_outstream const &) = delete;

    void close () { this->_os->flush(); }
};

//! ASDL memory outstream
class memory_outstream : public outstream {
public:
    explicit memory_outstream ();

  // no copying allowed!
    memory_outstream (memory_outstream const &) = delete;
    memory_outstream &operator= (memory_outstream const &) = delete;

    std::string get_pickle () const;
};

//! ASDL input stream
class instream {
public:
    explicit instream (std::string const &name, std::istream *is)
    : _is(is), _name(name)
    { }

  // no copying allowed!
    instream (instream const &) = delete;
    instream &operator= (instream const &) = delete;

  // move operations
    instream (instream &&is) noexcept
    {
        this->_is = is._is;
        is._is = nullptr;
    }
    instream &operator= (instream &&rhs) noexcept
    {
        if (this != &rhs) {
            this->_is = rhs._is;
            rhs._is = nullptr;
        }
        return *this;
    }

    virtual ~instream ()
    {
        if (this->_is != nullptr) {
            delete this->_is;
        }
    }

    char getc () {
        if (this->_is->good()) {
            return this->_is->get();
        } else {
            this->invalidIOState (this->_is->rdstate());
        }
    }
    unsigned char getb ()
    {
        if (this->_is->good()) {
            return static_cast<unsigned char>(this->_is->get());
        } else {
            this->invalidIOState (this->_is->rdstate());
        }
    }

    /// report an invalid enum/constructor tag
    [[noreturn]] virtual void invalidTag (unsigned int tag, std::string_view tyName);

    /// report an input error
    [[noreturn]] virtual void invalidIOState (std::ios_base::iostate st);

  protected:
    std::string _name;
    std::istream *_is;
};

//! ASDL file instream
class file_instream : public instream {
public:
    explicit file_instream (std::string const &file);

  // no copying allowed!
    file_instream (file_instream const &) = delete;
    file_instream &operator= (file_instream const &) = delete;
};

//! ASDL memory instream
class memory_instream : public instream {
public:
    explicit memory_instream (std::string const &data);

  // no copying allowed!
    memory_instream (memory_instream const &) = delete;
    memory_instream &operator= (memory_instream const &) = delete;
};

} // namespace asdl

#endif //! _ASDL_STREAM_HPP_
