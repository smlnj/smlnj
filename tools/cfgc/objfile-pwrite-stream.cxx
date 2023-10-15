/// \file objfile-pwrite-stream.cxx
///
/// \copyright 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief This file implements a subclass of llvm::raw_pwrite_stream
///        that we use to generate in-memory object files.
///
/// \author John Reppy
///

#include "objfile-pwrite-stream.hxx"
#include <cstring>

ObjfilePWriteStream::~ObjfilePWriteStream ()
{
    delete[] this->_data;
}

void ObjfilePWriteStream::write_impl (const char *ptr, size_t size)
{
    // check that there is sufficient space in the buffer for the write.
    if (this->_nBytes + size > this->_capacity) {
        this->_grow ((this->_nBytes + size) - this->_capacity);
    }

    ::memcpy(this->_data + this->_nBytes, ptr, size);
    this->_nBytes += size;
}

void ObjfilePWriteStream::pwrite_impl (const char *ptr, size_t size, uint64_t offset)
{
    // see llvm/Support/raw_ostream.h
    assert (offset + size <= this->_nBytes
        && "We don't support extending the stream");

    ::memcpy(this->_data + offset, ptr, size);
}

uint64_t ObjfilePWriteStream::current_pos () const
{
    return this->_nBytes;
}

// we allocate space in multiples of 16Kb
//
constexpr static uint64_t kAlignMask = 16*1024 - 1;

void ObjfilePWriteStream::_grow (size_t amount)
{
    size_t newSz = (this->_capacity + amount + kAlignMask) & ~kAlignMask;
    char *newBuf = new char[newSz];
    if (this->_data != nullptr) {
        ::memcpy(newBuf, this->_data, this->_nBytes);
        delete[] this->_data;
    }
    this->_data = newBuf;
    this->_capacity = newSz;
}
