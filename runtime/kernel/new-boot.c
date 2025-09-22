/*! \file boot.c
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the bootstrap loader for booting from binfiles.
 *
 * See dev-notes/binfile.adoc for a description of the binfile format.
 * This file must be kept in sync with compiler/Execution/binfile/binfile.sml.
 */

#include "ml-osdep.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/_types/_off_t.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "cache-flush.h"
#include "bin-file.h"
#include "ml-objects.h"
#include "gc.h"
#include "ml-globals.h"

/* the new on-disk file header */
typedef struct {
    char        kind[8];        /* file kind (either "BinFile " or "StabArch" */
    Unsigned32_t version;       /* file version (little endian) */
    char        smlnjVersion[16]; /* SML/NJ compiler version */
    Unsigned32_t numSects;      /* size of the section table */
} binfile_hdr_t;

/* the on-disk section descriptor layout */
typedef struct {
    Unsigned32_t id;
    Unsigned32_t flags;
    Unsigned32_t offsetW;
    Unsigned32_t szW;
} binfile_sect_desc_t;

typedef struct {
    off_t offset;
    size_t size;
} sect_desc_t;

/* the useful information contained in the header */
typedef struct {
    Unsigned32_t version;       /* binfile version; will be 0 for old format files */
    sect_desc_t importSect;     /* import PIDs */
    sect_desc_t exportSect;     /* the location of the export PID (if present).  For
                                 * the new binfile format, the export PID is located
                                 * at the beginning of the static environment section.
                                 */
    sect_desc_t litsSect;	/* the literal section */
    sect_desc_t codeSect;	/* the code section */
    Int32_t     entry;          /* code entrypoint */
    bool_t      isNative;       /* true if the code is native machine code; false if */
                                /* it is a CFG pickle) */
} binfile_info_t;


/* llvm_codegen:
 *
 * Given the source-file name and ASDL pickle of the CFG IR, generate
 * native machine code and return the corresponding ML code object and
 * entry-point offset as a heap-allocated pair.
 */
ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t szb);

#ifndef SEEK_SET
#  define SEEK_SET      0
#endif

/* The persistent ID list is stored in the PervStruct refcell.  It has
 * the following ML type:
 *
 *    datatype runDynEnv
 *      = NILrde
 *      | CONSrde of (Word8Vector.vector * Object.object * runDynEnv)
 */
#define PerIDList       (*PTR_MLtoC(ml_val_t, PervStruct))

PVT ml_val_t    BinFileList = LIST_nil; /* A list of bin files to load */


/* local routines */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *bootlist,
                            int *max_boot_path_len_ptr);
PVT FILE *OpenBinFile (const char *fname, bool_t isBinary);
PVT void ReadBinFile (FILE *file, void *buf, int nbytes, const char *fname);
PVT void LoadBinFile (ml_state_t *msp, char *fname);
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj);
PVT ml_val_t LookupPerID (pers_id_t *perID);
PVT void ShowPerID (char *buf, pers_id_t *perID);

static int HEX(int c)
{
    if (isdigit(c)) return c - '0';
    if (c >= 'a' && c <= 'z') return c - 'a' + 10;
    return c - 'A' + 10;
}

static void seek (FILE *file, off_t offset, const char *fname)
{
    if (fseeko(file, offset, SEEK_SET) == -1) {
        Die ("cannot seek to \"%s@%ul\"", fname, (unsigned long)offset);
    }
}


/* BootML:
 *
 * Boot the system using the items read from the "bootlist" file.
 */
void BootML (const char *bootlist, heap_params_t *heapParams)
{
    ml_state_t  *msp;
    int         max_boot_path_len;
    char        *fname;
    int         rts_init = 0;

/*DEBUG*/ SilentLoad=FALSE;/* */
    msp = AllocMLState (TRUE, heapParams);

#ifdef HEAP_MONITOR
    if (HeapMon_Init(CmdLineArgs, msp->ml_heap) == FAILURE)
        Die("unable to start heap monitor");
#endif

    InitFaultHandlers ();
    AllocGlobals (msp);

    /* construct the list of files to be loaded */
    BinFileList = BuildFileList (msp, bootlist, &max_boot_path_len);

    /* this space is ultimately wasted */
    if ((fname = MALLOC (max_boot_path_len)) == NULL) {
        Die ("unable to allocate space for boot file names");
    }

    /* boot the system */
    while (BinFileList != LIST_nil) {
      /* need to make a copy of the path name because LoadBinFile is
       * going to scribble into it */
        strncpy(fname, STR_MLtoC(LIST_hd(BinFileList)), max_boot_path_len);
        BinFileList = LIST_tl(BinFileList);
        if (fname[0] == '#') {
            if (rts_init) {
                Die ("runtime system registered more than once\n");
            }
            else {
              /* register the runtime system under the given pers id */
                pers_id_t pid;
                int i, l = strlen (fname + 1);
                for (i = 0; i < PERID_LEN; i++) {
                    int i2 = 2 * i;
                    if (i2 + 1 < l) {
                        int c1 = fname[i2+1];
                        int c2 = fname[i2+2];
                        pid.bytes[i] = (HEX(c1) << 4) + HEX(c2);
                    }
                }
                if (!SilentLoad) {
                    Say ("[Registering runtime system as %s]\n", fname+1);
                }
                EnterPerID (msp, &pid, RunTimeCompUnit);
                rts_init = 1;   /* make sure we do this only once */
            }
        }
        else {
            LoadBinFile (msp, fname);
        }
    }

    FreeMLState (msp);

} /* end of BootML */


/* BuildFileList:
 *
 * Given the directory path, build a list of the .bin files in the
 * heap.
 */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *bootlist, int *mbplp)
{
    FILE        *listF;
    ml_val_t    *fileNames = NULL;
    char        *nameBuf = NULL;
    int         max_num_boot_files = MAX_NUM_BOOT_FILES;
    int         max_boot_path_len = MAX_BOOT_PATH_LEN;
    int         i, j, numFiles;
    ml_val_t    fileList;
# define SIZE_BUF_LEN   128     /* this should be plenty for two numbers */
    char        sizeBuf[SIZE_BUF_LEN];
    char        c;

    numFiles = 0;

    listF = OpenBinFile (bootlist, FALSE);

    if (listF != NULL) {
        c = getc (listF);
        if (c == EOF) {
            Die ("bootlist file \"%s\" is empty", bootlist);
        }
        if (c == '%') {
            if (fgets (sizeBuf, SIZE_BUF_LEN, listF) != NIL(char *)) {
              /* hardly any checking here... */
                char *space = strchr (sizeBuf, ' ');
                *space = '\0';
                max_num_boot_files = strtoul(sizeBuf, NULL, 0);
                max_boot_path_len = strtoul(space+1, NULL, 0) + 2;
            }
            else {
                Die ("unable to read first line in \"%s\" after %%", bootlist);
            }
        }
        else {
          /* size spec is missing -- use defaults */
            ungetc (c, listF);
        }

        *mbplp = max_boot_path_len; /* tell the calling function... */

        if ((nameBuf = MALLOC(max_boot_path_len)) == NIL(char *)) {
            Die ("unable to allocate space for boot file names");
        }

        if ((fileNames = MALLOC(max_num_boot_files * sizeof(char *))) == NULL) {
            Die ("unable to allocate space for boot file name table");
        }

      /* read in the file names, converting them to ML strings. */
        while (fgets (nameBuf, max_boot_path_len, listF) != NIL(char *)) {
            j = strlen(nameBuf)-1;
            if (nameBuf[j] == '\n') nameBuf[j] = '\0';  /* remove "\n" */
            if (numFiles < max_num_boot_files) {
                fileNames[numFiles++] = ML_CString(msp, nameBuf);
            } else {
                Die ("too many files\n");
            }
        }
        fclose (listF);
    }

  /* create the in-heap list */
    for (fileList = LIST_nil, i = numFiles;  --i >= 0; ) {
        LIST_cons(msp, fileList, fileNames[i], fileList);
    }

    /* these guys are no longer needed from now on */
    if (fileNames) {
        FREE (fileNames);
    }
    if (nameBuf) {
        FREE (nameBuf);
    }

    return fileList;

} /* end of BuildFileList */


/* OpenBinFile:
 *
 * Open a file in the bin file directory.
 */
PVT FILE *OpenBinFile (const char *fname, bool_t isBinary)
{
    FILE        *file;

    if ((file = fopen (fname, isBinary ? "rb" : "r")) == NULL) {
        Error ("unable to open \"%s\"\n", fname);
    }

    return file;

} /* end of OpenBinFile */

/* ReadBinFile:
 */
PVT void ReadBinFile (FILE *file, void *buf, int nbytes, const char *fname)
{
    if (fread(buf, nbytes, 1, file) == -1) {
        Die ("cannot read file \"%s\"", fname);
    }

} /* end of ReadBinFile */

/* ReadLEB128Signed:
 *
 * Read a signed packed integer in LEB128 format.
 */
PVT Int32_t ReadLEB128Signed (FILE *file, const char *fname)
{
    Int32_t n = 0;
    Unsigned32_t shift = 0;
    Unsigned32_t slice;
    Byte_t c;

    do {
        ReadBinFile (file, &c, sizeof(c), fname);
        slice = c & 0x7f;
        if ((shift >= 31)
        && ((shift == 31 && slice != 0 && slice != 0x7f)
           || (shift > 31 && slice != (n < 0 ? 0x7f : 0x00))))
        {
/* TODO: overflow */
        }
        n |= slice << shift;
        shift += 7;
    } while ((c & 0x80) != 0);

    // sign extend negative numbers
    if (shift < 32 && ((c & 0x40) != 0)) {
        n |= (0xffffffff << shift);
    }

    return n;

} /* end of ReadLEB128Signed */

/* ReadLEB128Unsigned:
 *
 * Read an unsigned packed integer in LEB128 format.
 */
PVT Unsigned32_t ReadLEB128Unsigned (FILE *file, const char *fname)
{
    Unsigned32_t n = 0;
    Unsigned32_t shift = 0;
    Unsigned32_t slice;
    Byte_t       c;

    do {
        ReadBinFile (file, &c, sizeof(c), fname);
        slice = c & 0x7f;
        if ((shift >= 31)
        && ((shift == 31 && (slice << shift >> shift) != slice)
            || (shift > 31 && slice != 0)))
        {
/* TODO: overflow */
        }
        n += slice << shift;
        shift += 7;
    } while ((c & 0x80) != 0);

    return n;

} /* end of ReadLEB128Unsigned */

/* ReadPackedInt32:
 *
 * Read an integer in "packed" format.  (Small numbers only require 1 byte.)
 */
PVT Int32_t ReadPackedInt32 (FILE *file, const char *fname)
{
    Unsigned32_t        n;
    Byte_t              c;

    n = 0;
    do {
        ReadBinFile (file, &c, sizeof(c), fname);
        n = (n << 7) | (c & 0x7f);
    } while ((c & 0x80) != 0);

    return ((Int32_t)n);

} /* end of ReadPackedInt32 */

/* ReadHeader:
 *
 * Read the header of a binfile.  This code supports multiple binfile formats.
 */
PVT void ReadHeader (FILE *file, binfile_info_t *info, const char *fname)
{
  /* a buffer that is large enough to hold either form of header */
    Byte_t buf[sizeof(new_binfile_hdr_t)];

  /* we read the first 8 bytes to see if this is a old-style or new-style header */
    ReadBinFile (file, buf, 8, fname);
    if (memcmp(buf, "BinFile ", 8) == 0) {
      /* check the version number */
        Unsigned32_t bfVersion;
        ReadBinFile (file, &bfVersion, sizeof(bfVersion), fname);
        if (LITTLE_TO_HOST32(bfVersion) == 0x20250801) {
          /* this is the new, container-style, binfile format */
            info->version = LITTLE_TO_HOST32(bfVersion);
            Die ("new binfile format not supported yet");
        } else if (BIG_TO_HOST32(bfVersion) == BINFILE_VERSION) {
          /* this is the old (but not original) binfile format */
            new_binfile_hdr_t *p = (new_binfile_hdr_t *)buf;
            ReadBinFile (file, &(buf[12]), sizeof(new_binfile_hdr_t) - 12, fname);
            info->version = BIG_TO_HOST32(bfVersion);
            info->importSect.offset = sizeof(new_binfile_hdr_t);
            info->importSect.size = sizeof(pers_id_t) * BIG_TO_HOST32(p->importCnt);
            info->exportSect.offset = info->importSect.offset + info->importSect.size;
            info->exportSect.size = sizeof(pers_id_t) * BIG_TO_HOST32(p->exportCnt);
            /* for the literals and code, we need to read the lengths */
            {
                Int32_t tmp;
                /* the start of the literals+code section */
                off_t codeOff = ftello(file)
                    + info->importSect.size
                    + info->exportSect.size
                    + BIG_TO_HOST32(p->cmInfoSzB)
                    + BIG_TO_HOST32(p->guidSzB)
                    + BIG_TO_HOST32(p->pad);
                seek (file, codeOff, fname);
                /* get the literals offset and size */
                ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
                info->litsSect.size = BIG_TO_HOST32(tmp);
                ReadBinFile (file, &tmp, sizeof(Int32_t), fname); /* ignored */
                info->litsSect.offset = info->exportSect.offset
                    + info->exportSect.size
                    + BIG_TO_HOST32(p->cmInfoSzB)
                    + BIG_TO_HOST32(p->guidSzB)
                    + BIG_TO_HOST32(p->pad);
                /* get the code offset and size */
                info->codeSect.offset = info->litsSect.offset
                    + 2 * sizeof(Int32_t);
                ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
                info->codeSect.size = BIG_TO_HOST32(tmp);
                ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
                info->entry = BIG_TO_HOST32(tmp);
                info->isNative = TRUE;
            }
        } else {
            Die ("invalid binfile version %0x for \"%s\"", bfVersion, fname);
        }
    } else {
      /* old-style header */
        old_binfile_hdr_t *p = (old_binfile_hdr_t *)buf;
        ReadBinFile (file, &(buf[8]), sizeof(old_binfile_hdr_t) - 8, fname);
        info->version = 0;
        info->importSect.offset = sizeof(old_binfile_hdr_t);
        info->importSect.size = sizeof(pers_id_t) * BIG_TO_HOST32(p->importCnt);
        info->exportSect.offset = info->importSect.offset + info->importSect.size;
        info->exportSect.size = sizeof(pers_id_t) * BIG_TO_HOST32(p->exportCnt);
        /* for the literals and code, we need to read the lengths */
        {
            Int32_t tmp;
            /* the start of the literals+code section */
            off_t codeOff = ftello(file)
                + info->importSect.size
                + info->exportSect.size
                + BIG_TO_HOST32(p->cmInfoSzB)
                + BIG_TO_HOST32(p->guidSzB)
                + BIG_TO_HOST32(p->pad);
            seek(file, codeOff, fname);
            /* get the literals offset and size */
            ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
            info->litsSect.size = BIG_TO_HOST32(tmp);
            ReadBinFile (file, &tmp, sizeof(Int32_t), fname); /* ignored */
            info->litsSect.offset = info->exportSect.offset
                + info->exportSect.size
                + BIG_TO_HOST32(p->cmInfoSzB)
                + BIG_TO_HOST32(p->guidSzB)
                + BIG_TO_HOST32(p->pad);
            /* get the code offset and size */
            info->codeSect.offset = info->litsSect.offset
                + 2 * sizeof(Int32_t);
            ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
            info->codeSect.size = BIG_TO_HOST32(tmp);
            ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
            info->entry = BIG_TO_HOST32(tmp);
            info->isNative = TRUE;
        }
    }

} /* end of ReadHeader */

/* ImportSelection:
 *
 * Select out the interesting bits from the imported object.
 */
PVT void ImportSelection (ml_state_t *msp, FILE *file, const char *fname,
                          int *importVecPos, ml_val_t tree)
{
    Int32_t cnt = ReadPackedInt32 (file, fname);
    if (cnt == 0) {
        ML_AllocWrite (msp, *importVecPos, tree);
        (*importVecPos)++;
    }
    else {
        while (cnt-- > 0) {
            Int32_t selector = ReadPackedInt32 (file, fname);
            ImportSelection (msp, file, fname, importVecPos,
                             REC_SEL(tree, selector));
        }
    }

} /* end of ImportSelection */

/* LoadBinFile:
 */
PVT void LoadBinFile (ml_state_t *msp, char *fname)
{
    FILE            *file;
    int             i, remainingCode, importRecLen;
    int             exportSzB = 0;
    ml_val_t        codeObj, importRec, closure, val;
    binfile_info_t hdr;
    pers_id_t       exportPerID;
    Int32_t         thisSzB, thisEntryPoint;
    size_t          archiveOffset;
    char            *atptr, *colonptr;
    char            *objname = fname;

    /* an entry in the boot-file list should have the following syntax:
     *
     *  <filename> [ '@' <offset> [ ':' <objname> ] ]
     *
     * where <filename> is the name of the binfile, <offset> is an optional offset
     * into the binfile (for stable libraries), and <objname> is the optional name
     * of the object being loaded.  The <offset> defaults to 0 and the <objname>
     * defaults to the <filename>.  Note that the <objname> typically includes both
     * the filename and the offset, as well as the name of the source file being
     * loaded.
     */
    if ((atptr = strchr (fname, '@')) == NULL) {
        archiveOffset = 0;
    } else {
        if ((colonptr = strchr (atptr + 1, ':')) != NULL) {
            objname = colonptr + 1;
            *colonptr = '\0';
        }
      /* not a lot of extensive checking here... */
        archiveOffset = strtoul (atptr + 1, NULL, 0);
        *atptr = '\0';
    }

    if (!SilentLoad) {
        Say ("[Loading %s]\n", objname);
    }

  /* open the file */
    file = OpenBinFile (fname, TRUE);
    if (file == NULL)
        Exit (1);

  /* if an offset is given (i.e., we are probably dealing with a stable
   * archive), then seek to the beginning of the section that contains
   * the binfile
   */
    if (archiveOffset != 0) {
        seek (file, archiveOffset, fname);
    }

  /* get the header */
    ReadHeader (file, &hdr, fname);

  /* read the import PerIDs, and create the import vector */
    {
        seek (file, archiveOffset + hdr.importSect.offset, fname);

        int     importVecPos;

        importRecLen = hdr.importSect.size / sizeof(pers_id_t) + 1;

        if (NeedGC (msp, REC_SZB(importRecLen)))
            InvokeGCWithRoots (msp, 0, &BinFileList, NIL(ml_val_t *));

        ML_AllocWrite (msp, 0, MAKE_DESC(importRecLen, DTAG_record));
        for (importVecPos = 1; importVecPos < importRecLen; ) {
            pers_id_t   importPid;
            ReadBinFile (file, &importPid, sizeof(pers_id_t), fname);
            ImportSelection (msp, file, fname, &importVecPos,
                             LookupPerID(&importPid));
        }
        ML_AllocWrite(msp, importRecLen, ML_nil); /* placeholder for literals */
        importRec = ML_Alloc(msp, importRecLen);
    }

  /* read the export PerID */
    if (hdr.exportSect.size == sizeof(pers_id_t)) {
        seek (file, archiveOffset + hdr.exportSect.offset, fname);
        ReadBinFile (file, &exportPerID, sizeof(pers_id_t), fname);
    }
    else if (hdr.exportSect.size != 0) {
        Die ("# size of export pids is %d (should be 0 or %d)",
            (int)hdr.exportSect.size, (int)sizeof(pers_id_t));
    }

  /* read the literals */
    if (hdr.litsSect.size > 0) {
        seek (file, archiveOffset + hdr.litsSect.offset, fname);
        Byte_t *dataObj = NEW_VEC(Byte_t, thisSzB);
        ReadBinFile (file, dataObj, thisSzB, fname);
        SaveCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
        val = BuildLiterals (msp, dataObj, thisSzB);
        FREE(dataObj);
        RestoreCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
    } else {
        val = ML_unit;
    }

  /* do a functional update of the last element of the importRec. */
    for (i = 0;  i < importRecLen;  i++) {
        ML_AllocWrite(msp, i, PTR_MLtoC(ml_val_t, importRec)[i-1]);
    }
    ML_AllocWrite(msp, importRecLen, val);
    val = ML_Alloc(msp, importRecLen);
  /* do a GC, if necessary */
    if (NeedGC (msp, PERID_LEN+REC_SZB(5))) {
        InvokeGCWithRoots (msp, 0, &BinFileList, &val, NIL(ml_val_t *));
    }

  /* read the code */
    if (hdr.codeSect.size > 0) {
        seek (file, archiveOffset + hdr.codeSect.offset, fname);
        if (hdr.isNative) {
            thisSzB = hdr.codeSect.size;
            char *buffer = MALLOC(thisSzB);
            ReadBinFile (file, buffer, thisSzB, fname);
            /* allocate a code object and initialize with the code from the binfile */
            ENABLE_CODE_WRITE
                codeObj = ML_AllocCode (msp, PTR_MLtoC(void, buffer), thisSzB);
            DISABLE_CODE_WRITE
            FlushICache (PTR_MLtoC(char, codeObj), thisSzB);
            if (memcmp(PTR_MLtoC(char, codeObj), buffer, thisSzB) != 0) {
                Die("!!!!! code object corruption !!!!!\n");
            }
            FREE(buffer);
            thisEntryPoint = hdr.entry;
        } else {
            /* get the CFG pickle from the binfile */
            char *pkl = MALLOC(hdr.codeSect.size);
            ReadBinFile (file, pkl, hdr.codeSect.size, fname);

            if (!SilentLoad) {
                Say ("  [generate native code]\n");
            }

            /* generate code; we get a (WordVector.vector * int) value as a result */
            ml_val_t pair = llvm_codegen (msp, objname, pkl, hdr.codeSect.size);
            ml_val_t code = GET_SEQ_DATA(REC_SEL(pair, 0));
            thisEntryPoint = REC_SELINT(pair, 1);
            thisSzB = GET_SEQ_LEN(REC_SEL(pair, 0));

            /* allocate a code object and initialize with the generated code */
            ENABLE_CODE_WRITE
                codeObj = ML_AllocCode (msp, PTR_MLtoC(void, code), thisSzB);
            DISABLE_CODE_WRITE
            FlushICache (PTR_MLtoC(char, codeObj), thisSzB);

            FREE(pkl);
        }

        if (!SilentLoad) {
            Say ("  [addr: %p, size: %d]\n", PTR_MLtoC(char, codeObj), thisSzB);
        }

      /* create closure (taking entry point into account) */
        REC_ALLOC1 (msp, closure, PTR_CtoML (PTR_MLtoC(char, codeObj) + thisEntryPoint));

      /* apply the closure to the import PerID vector */
        SaveCState (msp, &BinFileList, NIL(ml_val_t *));
        val = ApplyMLFn (msp, closure, val, TRUE);
        RestoreCState (msp, &BinFileList, NIL(ml_val_t *));

      /* do a GC, if necessary */
        if (NeedGC (msp, PERID_LEN+REC_SZB(5))) {
            InvokeGCWithRoots (msp, 0, &BinFileList, &val, NIL(ml_val_t *));
        }
    }

  /* record the resulting exported PerID */
    if (hdr.exportSect.size > 0) {
        EnterPerID (msp, &exportPerID, val);
    }

    fclose (file);

} /* end of LoadBinFile */

/* EnterPerID:
 *
 * Enter a PerID/object binding in the heap allocated list of PerIDs.
 */
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj)
{
    ml_val_t        mlPerID;

  /* Allocate space for the PerID */
    mlPerID = ML_AllocString (msp, PERID_LEN);
    memcpy (STR_MLtoC(mlPerID), (char *)perID, PERID_LEN);

  /* Allocate the list element */
    REC_ALLOC3(msp, PerIDList, mlPerID, obj, PerIDList);

}

/* LookupPerID:
 */
PVT ml_val_t LookupPerID (pers_id_t *perID)
{
    ml_val_t        p, id;

    for (p = PerIDList;  p != ML_unit;  p = REC_SEL(p, 2)) {
        id = REC_SEL(p, 0);
        if (memcmp((char *)perID, STR_MLtoC(id), PERID_LEN) == 0)
            return (REC_SEL(p, 1));
    }

  /* here we were unable to find the PerID */
    {
        char    buf[64];
        ShowPerID (buf, perID);
        Die ("unable to find PerID %s", buf);
    }

} /* end of LookupPerID */


/* ShowPerID:
 */
PVT void ShowPerID (char *buf, pers_id_t *perID)
{
    char        *cp = buf;
    int         i;

    *cp++ = '[';
    for (i = 0;  i < PERID_LEN;  i++) {
        sprintf (cp, "%02x", perID->bytes[i]);
        cp += 2;
    }
    *cp++ = ']';
    *cp++ = '\0';

} /* end of ShowPerID */
