/// \file objfile-pwrite-stream.hxx
///
/// \copyright 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file defines a subclass of llvm::raw_pwrite_stream
///        that we use to generate in-memory object files.
///
/// \author John Reppy
///

#ifndef _OBJFILE_PWRITE_STREAM_HXX_
#define _OBJFILE_PWRITE_STREAM_HXX_

#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"

class ObjfilePWriteStream : public llvm::raw_pwrite_stream {
public:
    /// Construct a new ObjfilePWriteStream.
    explicit ObjfilePWriteStream ()
      : llvm::raw_pwrite_stream(true), _data(nullptr), _nBytes(0), _capacity(0)
    { }

    ~ObjfilePWriteStream () override;

    void flush () = delete;

    /// Return a StringRef for the vector contents.
    llvm::StringRef str ()
    {
        return llvm::StringRef (this->_data, this->_nBytes);
    }

    /// clear the buffer so that it is empty
    void clear () { this->_nBytes = 0; }

    /// return the number of bytes that have been written into the stream
    size_t size () const { return this->_nBytes; }

private:
    char *_data;        //!< pointer to the data buffer
    size_t _nBytes;     //!< current number of bytes in the buffer
    size_t _capacity;   //!< size of the buffer

    /// See raw_ostream::write_impl.
    void write_impl (const char *ptr, size_t size) override;

    void pwrite_impl (const char *ptr, size_t size, uint64_t offset) override;

    /// grow the capacity by at least `amount` bytes
    /// \param amount the required extra space in bytes
    void _grow (size_t amount);

    /// Return the current position within the stream.
    uint64_t current_pos() const override;

};

#endif //!_OBJFILE_OSTREAM_HXX_
