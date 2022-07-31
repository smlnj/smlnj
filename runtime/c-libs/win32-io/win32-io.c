/* win32-io.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * interface to win32 io functions
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#include "win32-fault.h"

#define EOF_char           '\x01a'           /* ^Z is win32 eof */
#define CTRL_C_char        '\x003'

/* macro to check if h is a console that hasn't been redirected */
#define IS_CONIN(h) (((h) == win32_stdin_handle) && \
		     (GetFileType(h) == FILE_TYPE_CHAR))

/* _ml_win32_IO_get_std_handle : word32 -> handle
 * interface to win32 GetStdHandle
 */
ml_val_t _ml_win32_IO_get_std_handle (ml_state_t *msp, ml_val_t arg)
{
    DWORD w = WORD32_MLtoC(arg);
    HANDLE h = GetStdHandle(w);
    ml_val_t res;

#ifdef DEBUG_WIN32
    SayDebug("getting std handle for %x as %p\n", w, h);
#endif

    return HANDLE_CtoML(msp, h);
}

/* _ml_win32_IO_close : handle -> unit
 * close a handle
 */
ml_val_t _ml_win32_IO_close (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(arg);

    if (CloseHandle(h)) {
	return ML_unit;
    } else {
#ifdef DEBUG_WIN32
	SayDebug("_ml_win32_IO_close(%p): failing: error = %d\n", h, GetLastError());
#endif
        return RAISE_SYSERR(msp,-1);
    }
}


/* _ml_win32_IO_set_file_pointer : (handle * Position.int * word32) -> Position.int
 *                                  handle   dist           how
 */
ml_val_t _ml_win32_IO_set_file_pointer (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(REC_SEL(arg,0));
    DWORD how = WORD32_MLtoC(REC_SEL(arg,2));
    LARGE_INTEGER dist, pos;

    dist.QuadPart = INT64_MLtoC(REC_SEL(arg,1));

    if (SetFilePointerEx(h, dist, &pos, how)) {
	return ML_AllocInt64(msp, pos.QuadPart);
    }
    else {
#ifdef DEBUG_WIN32
        SayDebug("_ml_win32_IO_set_file_pointer(%p, %lld, %d): failing: error = %d\n",
	    h, dist.QuadPart, how, GetLastError());
#endif
	return RAISE_SYSERR(msp, -1);
    }
}

/* remove CRs ('\r') from buf of size *np; sets *np to be the new buf size
 */
PVT rm_CRs (char *buf,int *np)
{
    int i, j = 0;
    int n = *np;

    for (i = 0; i < n; i++) {
	if (buf[i] != '\r') {
	    buf[j++] = buf[i];
	}
    }
    *np = j;
}


/* translate CRs ('\r') to newlines ('\n'), removing existing LFs (also '\n').
 * process backspace (BS)
 * sets *np to the new buffer size
 * returns TRUE if the buffer contains the EOF character
 */
PVT bool_t CRLF_EOFscan (char *buf, int *np)
{
    int i, j = 0;
    int n = *np;
    bool_t sawEOF = FALSE;

    for (i = 0; i<n; i++) {
	if (buf[i] == '\r') {             /* translate CRs */
	    buf[j++] = '\n';
	} else if (buf[i] == '\b') {      /* process BSes */
	    if (j) j--;
	} else if (buf[i] != '\n') {
	    if (buf[i] == EOF_char) {
		sawEOF = TRUE;
	    }
	    buf[j++] = buf[i];
	}
    }
    *np = j;
    return sawEOF;
}

/* _ml_win32_IO_read_vec : (handle * int) -> word8vector.vector
 *                          handle   nbytes
 *
 * Read the specified number of bytes from the specified handle,
 * returning them in a vector.
 *
 * Note: Read operations on console devices do not trap ctrl-C.
 *       ctrl-Cs are placed in the input buffer.
 */
ml_val_t _ml_win32_IO_read_vec (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(REC_SEL(arg, 0));
    DWORD nbytes = (DWORD) REC_SELINT(arg, 1);
    ml_val_t vec, res;
    DWORD n;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw (msp, BYTES_TO_WORDS(nbytes));
    if (ReadFile(h, PTR_MLtoC(void, vec), nbytes, &n, NULL)) {
        if (n == 0) {
#ifdef DEBUG_WIN32
	    SayDebug("_ml_win32_IO_read_vec(%p, %d): eof\n", h, nbytes);
#endif
	    return ML_string0;
	}
	if (n < nbytes) {
	  /* we need to shrink the vector */
	    ML_ShrinkRaw (msp, vec, BYTES_TO_WORDS(n));
	}
      /* allocate header */
	SEQHDR_ALLOC (msp, res, DESC_string, vec, n);
	return res;
    }
    else {
#ifdef DEBUG_WIN32
        SayDebug("_ml_win32_IO_read_vec(%p, %d) failed; n = %d, error = %d\n",
	    h, nbytes, n, GetLastError());
#endif
        return RAISE_SYSERR(msp,-1);
    }
}

PVT bool_t check_cntrl_c (BOOL read_OK, int bytes_read)
{
  /* this is a rude hack */
  /* under NT and default console mode, on
   *  EOF: read_OK is true, and n > 0
   *   ^C: read_OK is true, and n == 0.  However, the cntrl_c handler is
   *       not always invoked before ReadConsole returns.
   */
  /* under 95 and default console mode, on
   *  EOF: read_OK is true and n is 0
   *   ^C: read_OK is true, n is 0, but handler seems to always have been run
   */
    if (read_OK && (bytes_read == 0) && win32_isNT) {
      /* guaranteed that a cntrl_c has occurred and has not been reset */
      /* wait for it to happen */
        wait_for_cntrl_c();
        return TRUE;
    }
    return FALSE;
}

/*
 * Since we're not setting console mode to processed input (as that
 * causes other issues around no longer getting an async event while
 * executing), we need to append the ^C into the input stream
 * manually. But since ^C isn't handled nicely (illegal character), we
 * instead just prepend a space into the stream.
 */
PVT void append_cntrl_c (char *buf, int *np, int max)
{
    /* Out of space in buffer; exit without adding a character. This should
     * be fine (provided max>0, which it always is), as all we're trying to
     * prevent is returning zero bytes and causing the runtime to think we
     * got an EOF on the input stream.
     */
    if (*np == max) return;

    buf[(*np)++] = ' ';
}

/* _ml_win32_IO_read_vec_txt : (handle * int) -> char8vector
 *                              handle   nbytes
 *
 * Read the specified number of bytes from the specified handle,
 * returning them in a vector.
 *
 * reflect changes in _ml_win32_IO_read_arr_txt
 */
ml_val_t _ml_win32_IO_read_vec_txt(ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(REC_SEL(arg, 0));
    DWORD nbytes = (DWORD) REC_SELINT(arg, 1);
    ml_val_t vec, res;
    DWORD	n;
    BOOL flag = FALSE;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (nbytes));

    if (IS_CONIN(h)) {
	flag = ReadConsole(h, PTR_MLtoC(void,vec), nbytes, &n, NULL);
	if (check_cntrl_c(flag, n)) {
	    append_cntrl_c(PTR_MLtoC(void,vec), &n, nbytes);
	}
    } else {
	flag = ReadFile(h,PTR_MLtoC(void,vec),nbytes,&n,NULL);
    }
    if (flag) {
	if (IS_CONIN(h)) {
	    if (CRLF_EOFscan((char *)vec,&n)) {
		n = 0;
	    }
	}
	else {
	    rm_CRs((char *)vec,&n);
	}

	if (n == 0) {
#ifdef DEBUG_WIN32
	    SayDebug("_ml_win32_IO_read_vec_txt(%p, %d): eof\n", h, nbytes);
#endif
	    return ML_string0;
	}
	if (n < nbytes) {
	  /* shrink buffer */
	    ML_ShrinkRaw (msp, vec, BYTES_TO_WORDS(n));
	}
      /* allocate header */
	SEQHDR_ALLOC (msp, res, DESC_string, vec, n);
#ifdef DEBUG_WIN32
	SayDebug("_ml_win32_IO_read_vec_txt: read %d\n",n);
#endif
	return res;
    }
    else if ((h == win32_stdin_handle)		/* input from stdin */
    && (GetFileType(h) == FILE_TYPE_PIPE)	/* but not console */
    && (GetLastError() == ERROR_BROKEN_PIPE))	/* and pipe broken */
    {
      /* this is an EOF on redirected stdin (ReadFile failed) */
	return ML_string0;
    }
    else {
#ifdef DEBUG_WIN32
        SayDebug("_ml_win32_IO_read_vec_txt: failing on handle %p\n", h);
#endif
        return RAISE_SYSERR(msp,-1);
    }
}

/* _ml_win32_IO_read_arr : (handle * word8array * int * int) -> int
 *                          handle   buffer       n     start
 *
 * Read n bytes of data from the specified handle into the given array,
 * starting at start. Return the number of bytes read. Assume bounds
 * have been checked.
 *
 * Note: Read operations on console devices do not trap ctrl-C.
 *       ctrl-Cs are placed in the input buffer.
 */
ml_val_t _ml_win32_IO_read_arr (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(REC_SEL(arg, 0));
    ml_val_t buf = REC_SEL(arg,1);
    DWORD nbytes = (DWORD) REC_SELINT(arg, 2);
    Byte_t *start = STR_MLtoC(buf) + REC_SELINT(arg,3);
    DWORD n;

    if (ReadFile(h, PTR_MLtoC(void,start), nbytes, &n, NULL)) {
#ifdef DEBUG_WIN32
        if (n == 0) SayDebug("_ml_win32_IO_read_arr(%p, %d): eof\n", h, nbytes);
#endif
        return INT_CtoML(n);
    }
#ifdef DEBUG_WIN32
    SayDebug("_ml_win32_IO_read_arr: failing\n");
#endif
    return RAISE_SYSERR(msp,-1);
}

/* _ml_win32_IO_read_arr_txt : (handle * char8array * int * int) -> int
 *                              handle   buffer       n     start
 *
 * Read n bytes of data from the specified handle into the given array,
 * starting at start. Return the number of bytes read. Assume bounds
 * have been checked.
 *
 * reflect changes in _ml_win32_IO_read_vec_txt
 */
ml_val_t _ml_win32_IO_read_arr_txt (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(REC_SEL(arg, 0));
    ml_val_t buf = REC_SEL(arg,1);
    DWORD nbytes = (DWORD) REC_SELINT(arg, 2);
    Byte_t *start = STR_MLtoC(buf) + REC_SELINT(arg,3);
    DWORD n;
    BOOL flag;

    if (IS_CONIN(h)) {
	flag = ReadConsole(h,PTR_MLtoC(void,start),nbytes,&n,NULL);
	if (check_cntrl_c(flag,n)) {
	    append_cntrl_c(PTR_MLtoC(void,start),&n,nbytes);
	}
    } else {
	flag = ReadFile(h,PTR_MLtoC(void,start),nbytes,&n,NULL);
    }
    if (flag) {
	if (IS_CONIN(h)) {
	    if (CRLF_EOFscan((char *)start,&n)) {
		n = 0;
	    }
	}
	else {
	    rm_CRs((char *)buf,&n);
	}
#ifdef DEBUG_WIN32
        SayDebug("_ml_win32_IO_read_arr_txt(%p, %d): eof\n", h, nbytes);
#endif
        return INT_CtoML(n);
    } else {
	if ((h == win32_stdin_handle)                /* input from stdin */
	&&  (GetFileType(h) == FILE_TYPE_PIPE)       /* but not console */
	&&  (GetLastError() == ERROR_BROKEN_PIPE))   /* and pipe broken */
        {
	  /* this is an EOF on redirected stdin (ReadFile failed) */
	    return INT_CtoML(0);
	}
    }

#ifdef DEBUG_WIN32
    SayDebug("_ml_win32_IO_read_arr_txt: failing\n");
#endif
    return RAISE_SYSERR(msp, -1);
}


/* _ml_win32_IO_create_file : (string * word32 * word32 * word32 * word32) -> handle
 *                             name     access   share    create   attr       handle
 *
 * create file "name" with access, share, create, and attr flags
 */
ml_val_t _ml_win32_IO_create_file (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t fname = REC_SEL(arg,0);
    char *name = STR_MLtoC(fname);
    DWORD access = WORD32_MLtoC(REC_SEL(arg,1));
    DWORD share = WORD32_MLtoC(REC_SEL(arg,2));
    DWORD create = WORD32_MLtoC(REC_SEL(arg,3));
    DWORD attr = WORD32_MLtoC(REC_SEL(arg,4));
    HANDLE h =  CreateFile(name, access, share, NULL, create, attr, INVALID_HANDLE_VALUE);
    ml_val_t res;

#ifdef DEBUG_WIN32
    if (h == INVALID_HANDLE_VALUE) {
        SayDebug("create_file(\"%s\", %x, %x, %x, %x) failed; error = %d\n",
	    name, access, share, create, attr, GetLastError());
    }
    else {
        SayDebug("create_file(\"%s\", %x, %x, %x, %x) = %p\n",
	    name, access, share, create, attr, h);
    }
#endif

    return HANDLE_CtoML(msp, h);
}

/* _ml_win32_IO_write_buf : (handle * word8vector.vector * int * int) -> int
 *                           handle   buf                  n     offset
 *
 * generic routine for writing n byes from buf to handle starting at offset
 *
 * A maximum print size is used to avoid exceeding maximum buffer thresholds
 * with handles corresponding to console output. Technically, we can use
 * larger values, but this will also support several other devices that have
 * output limits if we decide to open up the range of supported file handles
 * through interop.
 */
#define MAX_PRINT_SIZE 30000
ml_val_t _ml_win32_IO_write_buf (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = HANDLE_MLtoC(REC_SEL(arg,0));
    ml_val_t buf = REC_SEL(arg,1);
    size_t nbytes = REC_SELINT(arg,2);
    Byte_t *start = (Byte_t *) (STR_MLtoC(buf) + REC_SELINT(arg, 3));
    DWORD n, remaining, total;
    char *buffer = PTR_MLtoC(void,start);
    int err;

#ifdef DEBUG_WIN32
    SayDebug("_ml_win32_IO_write_buf(%p, -, %d, %d)\n", h, nbytes, REC_SELINT(arg, 3));
#endif

    remaining = nbytes;
    total = 0;

    while (remaining > 0) {
	nbytes = min (MAX_PRINT_SIZE, remaining);
	    if (WriteFile(h, buffer, nbytes, &n, NULL)) {
#ifdef DEBUG_WIN32
		if (n == 0) SayDebug("_ml_win32_IO_write_buf(%h, %d): eof\n", h, nbytes);
#endif
		total += n;
		remaining -= n;
		buffer += n;
	    } else {
#ifdef DEBUG_WIN32
		SayDebug("_ml_win32_IO_write_buf: failing\n");
#endif
		return RAISE_SYSERR(msp,-1);
	    }
    }

    return INT_CtoML(total);
}

ml_val_t _ml_win32_IO_write_vec (ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_IO_write_buf(msp, arg);
}

ml_val_t _ml_win32_IO_write_arr (ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_IO_write_buf(msp, arg);
}

ml_val_t _ml_win32_IO_write_vec_txt (ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_IO_write_buf(msp, arg);
}

ml_val_t _ml_win32_IO_write_arr_txt (ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_IO_write_buf(msp, arg);
}

/* end of win32-io.c */
