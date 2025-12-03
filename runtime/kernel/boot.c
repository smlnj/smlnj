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
    Unsigned32_t offsetB;
    Unsigned32_t szB;
} binfile_sect_desc_t;

typedef struct {
    off_t offset;
    size_t size;
} sect_desc_t;

/* section tags */
#if defined(BYTE_ORDER_BIG)
#  define BF_TAG(s)	(((s)[0] << 24) | ((s)[1] << 16) | ((s)[2] << 8) | ((s)[3]))
#else
#  define BF_TAG(s)	(((s)[3] << 24) | ((s)[2] << 16) | ((s)[1] << 8) | ((s)[0]))
#endif

#define BF_IMPT	BF_TAG("IMPT")
#define BF_EXPT BF_TAG("EXPT")
#define BF_LITS BF_TAG("LITS")
#define BF_CODE BF_TAG("CODE")
#define BF_CFGP BF_TAG("CFGP")

/* the useful information contained in the header */
typedef struct {
    Unsigned32_t version;       /* binfile version; will be 0 for old format files */
    int         nImports;       /* the number of imports in the importSect */
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

static void Seek (FILE *file, off_t offset, const char *fname)
{
/*DEBUG*/Say("## Seek (-, %0#x, \"%s\")\n", (int)offset, fname);
    if (fseeko(file, offset, SEEK_SET) == -1) {
        Die ("cannot seek to \"%s@%ul\"", fname, (unsigned long)offset);
    }
}

static void ReadBinFileAt (FILE *file, void *buf, int nbytes, off_t offset, const char *fname)
{
    Seek (file, offset, fname);
    ReadBinFile (file, buf, nbytes, fname);
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

Say("# BootML: new-boot.c\n");

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
/*DEBUGSay("## ReadBinFile (-, -, %d, \"%s\")\n", nbytes, fname);*/
    if (fread(buf, nbytes, 1, file) == -1) {
        Die ("cannot read file \"%s\"", fname);
    }

} /* end of ReadBinFile */

/* ReadLEB128Unsigned:
 *
 * Read an unsigned packed integer in LEB128 format.
 */
PVT Unsigned32_t ReadLEB128Unsigned (FILE *file, const char *fname, int *nb)
{
    Unsigned32_t n = 0;
    Unsigned32_t shift = 0;
    Unsigned32_t slice;
    Byte_t       c;

    int i = 0;
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
        i++;
    } while ((c & 0x80) != 0);

    if (nb != NIL(int *)) { *nb = i; }

    return n;

} /* end of ReadLEB128Unsigned */

/* ReadPackedInt32:
 *
 * Read an integer in "packed" format.  (Small numbers only require 1 byte.)
 */
PVT Int32_t ReadPackedInt32 (FILE *file, const char *fname, int *nb)
{
    Unsigned32_t        n;
    Byte_t              c;

    n = 0;
    int i = 0;
    do {
        ReadBinFile (file, &c, sizeof(c), fname);
        n = (n << 7) | (c & 0x7f);
        i++;
    } while ((c & 0x80) != 0);

    if (nb != NIL(int *)) { *nb = i; }

/*DEBUG*/Say("#### ReadPackedInt32: %d\n", n);
    return ((Int32_t)n);

} /* end of ReadPackedInt32 */

/* ReadHeader:
 *
 * Read the header of a binfile.  This code supports multiple binfile formats.
 */
PVT void ReadHeader (FILE *file, off_t base, binfile_info_t *info, const char *fname)
{
  /* a buffer that is large enough to hold either form of header */
    Byte_t buf[256];

  /* we read the first 8 bytes to see if this is a old-style or new-style header */
    ReadBinFile (file, buf, 8, fname);
    if (memcmp(buf, "BinFile ", 8) == 0) {
      /* check the version number */
        Unsigned32_t bfVersion;
        ReadBinFile (file, &bfVersion, 4, fname);
        if (LITTLE_TO_HOST32(bfVersion) == 0x20250801) {
          /* this is the new, container-style, binfile format */
            binfile_hdr_t *p = (binfile_hdr_t *)buf;
            p->version = bfVersion;
            info->version = LITTLE_TO_HOST32(bfVersion);
            /* read the rest of the header */
            ReadBinFile (file, &(buf[12]), sizeof(binfile_hdr_t) - 12, fname);
            int nSects = LITTLE_TO_HOST32(p->numSects);
Say("# ReadHeader: new-style\n");
Say("## version = %08x\n", info->version);
Say("## nSects = %d\n", nSects);
            /* flags to track which sections have been seen */
            bool_t seenImports = FALSE;
            bool_t seenExports = FALSE;
            bool_t seenLits = FALSE;
            bool_t seenCodeOrCFG = FALSE;
            /* process the sections to get the header info */
            for (int i = 0;  i < nSects;  ++i) {
                binfile_sect_desc_t sd;
                ReadBinFile (file, &sd, sizeof(binfile_sect_desc_t), fname);
                switch (LITTLE_TO_HOST32(sd.id)) {
                case BF_IMPT:
                    if (seenImports) {
                        Die("multiple 'IMPT' sections");
                    } else {
                        seenImports = TRUE;
                        info->importSect.offset = LITTLE_TO_HOST32(sd.offsetB);
                        info->importSect.size = LITTLE_TO_HOST32(sd.szB);
                    }
                    break;
                case BF_EXPT:
                    if (seenExports) {
                        Die("multiple 'EXPT' sections");
                    } else {
                        seenExports = TRUE;
                        info->exportSect.offset = LITTLE_TO_HOST32(sd.offsetB);
                        info->exportSect.size = LITTLE_TO_HOST32(sd.szB);
                    }
                    break;
                case BF_LITS:
                    if (seenLits) {
                        Die("multiple 'LITS' sections");
                    } else {
                        seenLits = TRUE;
                        info->litsSect.offset = LITTLE_TO_HOST32(sd.offsetB);
                        info->litsSect.size = LITTLE_TO_HOST32(sd.szB);
                    }
                    break;
                case BF_CODE:
                    if (seenCodeOrCFG) {
                        Die("multiple 'CODE' / 'CFGP' sections");
                    } else {
                        seenCodeOrCFG = TRUE;
                        info->codeSect.offset = LITTLE_TO_HOST32(sd.offsetB);
                        info->codeSect.size = LITTLE_TO_HOST32(sd.szB);
                        info->isNative = TRUE;
                    }
                    break;
                case BF_CFGP:
                    if (seenCodeOrCFG) {
                        Die("multiple 'CODE' / 'CFGP' sections");
                    } else {
                        seenCodeOrCFG = TRUE;
                        info->codeSect.offset = LITTLE_TO_HOST32(sd.offsetB);
                        info->codeSect.size = LITTLE_TO_HOST32(sd.szB);
                        info->isNative = FALSE;
                    }
                    break;
                default: /* ignore other sections */
/*DEBUG*/
{Unsigned32_t id = LITTLE_TO_HOST32(sd.id); Say("# [%d] ignore ID '%c%c%c%c'\n",
i, (char)id, (char)(id >> 8), (char)(id >> 16), (char)(id >> 24));}
/*DEBUG*/
                    break;
                }
            }
            /* check that we have seen all of the necessary sections */
            if (!seenImports || !seenExports || !seenLits || !seenCodeOrCFG) {
                char msg[64] = "missing sections:";
                if (!seenImports) { strcat (msg, " IMPT"); }
                if (!seenExports) { strcat (msg, " EXPT"); }
                if (!seenLits) { strcat (msg, " LITS"); }
                if (!seenCodeOrCFG) { strcat (msg, " CODE/CFGP"); }
                Die(msg);
            }
            /* get additional info for 'IMPT' and 'CODE' sections */
            {
                int nb;
                Seek(file, base + info->importSect.offset, fname);
                info->nImports = ReadLEB128Unsigned(file, fname, &nb);
                info->importSect.offset += nb;
                info->importSect.size -= nb;
            }
            if (info->isNative) {
                int nb;
Say("## code: initial offset = %d, size = %d\n",
(int)info->codeSect.offset, (int)info->codeSect.size);
                Seek(file, base + info->codeSect.offset, fname);
                info->entry = ReadLEB128Unsigned(file, fname, &nb);
                info->codeSect.offset += nb;
                info->codeSect.size -= nb;
            }
Say("## imports: cnt = %d, offset = %d, size = %d\n",
info->nImports, (int)info->importSect.offset, (int)info->importSect.size);
Say("## exports: offset = %d, size = %d\n",
(int)info->exportSect.offset, (int)info->exportSect.size);
Say("## literals: offset = %d, size = %d\n",
(int)info->litsSect.offset, (int)info->litsSect.size);
Say("## code: offset = %d, size = %d\n",
(int)info->codeSect.offset, (int)info->codeSect.size);
        } else if (BIG_TO_HOST32(bfVersion) == BINFILE_VERSION) {
          /* this is the old (but not original) binfile format */
            new_binfile_hdr_t *p = (new_binfile_hdr_t *)buf;
            ReadBinFile (file, &(buf[12]), sizeof(new_binfile_hdr_t) - 12, fname);
            info->version = BIG_TO_HOST32(bfVersion);
            info->nImports = BIG_TO_HOST32(p->importCnt);
            info->importSect.offset = sizeof(new_binfile_hdr_t);
            info->importSect.size = BIG_TO_HOST32(p->importSzB);
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
                /* get the literals offset and size */
                ReadBinFileAt (file, &tmp, sizeof(Int32_t), codeOff, fname);
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
        info->nImports = BIG_TO_HOST32(p->importCnt);
        info->importSect.offset = sizeof(old_binfile_hdr_t);
        info->importSect.size = BIG_TO_HOST32(p->importSzB);
        info->exportSect.offset = info->importSect.offset + info->importSect.size;
        info->exportSect.size = sizeof(pers_id_t) * BIG_TO_HOST32(p->exportCnt);
        /* for the literals and code, we need to read the lengths */
        {
            Int32_t tmp;
            /* the offset (from the start of the binfile) to the literals */
            info->litsSect.offset = info->exportSect.offset
                + info->exportSect.size
                + BIG_TO_HOST32(p->cmInfoSzB)
                + BIG_TO_HOST32(p->guidSzB)
                + BIG_TO_HOST32(p->pad);
            /* seek to the start of the literals+code section in the file */
            /* get the literals size */
            ReadBinFileAt (file, &tmp, sizeof(Int32_t), base + info->litsSect.offset, fname);
            info->litsSect.size = BIG_TO_HOST32(tmp);
            info->litsSect.offset += 2 * sizeof(Int32_t); /* adjust offset */
            /* get the code offset and size */
            info->codeSect.offset = info->litsSect.offset + info->litsSect.size;
            ReadBinFileAt (file, &tmp, sizeof(Int32_t), base + info->codeSect.offset, fname);
            info->codeSect.size = BIG_TO_HOST32(tmp);
            ReadBinFile (file, &tmp, sizeof(Int32_t), fname);
            info->entry = BIG_TO_HOST32(tmp);
            info->codeSect.offset += 2 * sizeof(Int32_t); /* adjust offset */
            info->isNative = TRUE;
        }
/*DEBUG*/
Say("# ReadHeader: old-style\n");
Say("## imports: cnt = %d, offset = %d, size = %d\n",
info->nImports, (int)info->importSect.offset, (int)info->importSect.size);
Say("## exports: cnt = %d, offset = %d, size = %d\n",
BIG_TO_HOST32(p->exportCnt), (int)info->exportSect.offset, (int)info->exportSect.size);
Say("## cmInfo: size = %d\n", BIG_TO_HOST32(p->cmInfoSzB));
Say("## GUID: size = %d\n", BIG_TO_HOST32(p->guidSzB));
Say("## pad: size = %d\n", BIG_TO_HOST32(p->pad));
Say("## literals: offset = %d, size = %d\n",
(int)info->litsSect.offset, (int)info->litsSect.size);
Say("## code: offset = %d, size = %d\n",
(int)info->codeSect.offset, (int)info->codeSect.size);
/*DEBUG*/
    }

} /* end of ReadHeader */

/* OldImportSelection:
 *
 * Select out the interesting bits from the imported object.
 * This is the "old" version that uses the old packed-int encoding.
 */
PVT void OldImportSelection (
    ml_state_t *msp, FILE *file, const char *fname,
    int *importVecPos, ml_val_t tree)
{
    Int32_t cnt = ReadPackedInt32 (file, fname, NIL(int *));
Say("### OldImportSelection: tree = %p; cnt = %d\n", tree, cnt);
    if (cnt == 0) {
        ML_AllocWrite (msp, *importVecPos, tree);
        (*importVecPos)++;
    }
    else {
        while (cnt-- > 0) {
            Int32_t selector = ReadPackedInt32 (file, fname, NIL(int *));
            OldImportSelection (
                msp, file, fname, importVecPos,
                REC_SEL(tree, selector));
        }
    }

} /* end of OldImportSelection */

/* ImportSelection:
 *
 * Select out the interesting bits from the imported object.
 * This is the "new" version that uses the LEB128 packed-int encoding.
 */
PVT void ImportSelection (
    ml_state_t *msp, FILE *file, const char *fname,
    int *importVecPos, ml_val_t tree)
{
    Int32_t cnt = ReadLEB128Unsigned (file, fname, NIL(int *));
Say("### ImportSelection: tree = %p; cnt = %d\n", tree, cnt);
    if (cnt == 0) {
        ML_AllocWrite (msp, *importVecPos, tree);
        (*importVecPos)++;
    }
    else {
        while (cnt-- > 0) {
            Int32_t selector = ReadLEB128Unsigned (file, fname, NIL(int *));
Say("#### select %p[%d]\n", tree, selector);
            ImportSelection (
                msp, file, fname, importVecPos,
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
    size_t          archiveOffset;
    char            *atptr, *colonptr;
    char            *objname = fname;

/*DEBUG*/Say("# LoadBinFile: fname = \"%s\"\n", fname);
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
        Seek (file, archiveOffset, fname);
    }

  /* get the header */
    ReadHeader (file, archiveOffset, &hdr, fname);

  /* read the import PerIDs, and create the import vector */
    {
        Seek (file, archiveOffset + hdr.importSect.offset, fname);

        int     importVecPos;

        importRecLen = hdr.nImports + 1;

        if (NeedGC (msp, REC_SZB(importRecLen))) {
            InvokeGCWithRoots (msp, 0, &BinFileList, NIL(ml_val_t *));
        }

/*DEBUG*/Say("## read %d import PIDS\n", importRecLen-1);
        ML_AllocWrite (msp, 0, MAKE_DESC(importRecLen, DTAG_record));
        for (importVecPos = 1; importVecPos < importRecLen; ) {
            pers_id_t   importPid;
            ReadBinFile (file, &importPid, sizeof(pers_id_t), fname);
/*DEBUG*/
{ char    buf[64];
  ShowPerID (buf, &importPid);
  Say("### import PID[%d]: %s\n", importVecPos, buf);
}
/*DEBUG*/
            if (hdr.version == 0x20250801) {
                ImportSelection (msp, file, fname, &importVecPos, LookupPerID(&importPid));
            } else {
                OldImportSelection (msp, file, fname, &importVecPos, LookupPerID(&importPid));
            }
        }
        ML_AllocWrite(msp, importRecLen, ML_nil); /* placeholder for literals */
        importRec = ML_Alloc(msp, importRecLen);
    }

  /* read the export PerID */
    if (hdr.exportSect.size == sizeof(pers_id_t)) {
/*DEBUG*/Say("## read export PID\n");
        ReadBinFileAt (file, &exportPerID, sizeof(pers_id_t), archiveOffset + hdr.exportSect.offset, fname);
/*DEBUG*/
{ char    buf[64];
  ShowPerID (buf, &exportPerID);
  Say("### export PID: %s\n", buf);
}
/*DEBUG*/
    }
    else if (hdr.exportSect.size != 0) {
        Die ("# size of export pids is %d (should be 0 or %d)",
            (int)hdr.exportSect.size, (int)sizeof(pers_id_t));
    }

  /* read the literals */
    if (hdr.litsSect.size > 0) {
/*DEBUG*/Say("## read literals (%d bytes)\n", (int)hdr.litsSect.size);
        Byte_t *dataObj = NEW_VEC(Byte_t, hdr.litsSect.size);
        ReadBinFileAt (file, dataObj, hdr.litsSect.size, archiveOffset + hdr.litsSect.offset, fname);
        SaveCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
        val = BuildLiterals (msp, dataObj, hdr.litsSect.size);
        FREE(dataObj);
        RestoreCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
    } else {
        val = ML_unit;
    }
/*DEBUG*/Say("### literal vec = %p\n", val);

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
/*DEBUG*/
Say("## import record = {");
for (i = 0;  i < importRecLen;  i++) { Say(" %p", PTR_MLtoC(ml_val_t, val)[i]); }
Say(" }\n");
/*DEBUG*/

  /* read the code */
    if (hdr.codeSect.size > 0) {
        Int32_t thisSzB, thisEntryPoint;
/*DEBUG*/Say("## read code @ %d\n", (int)(archiveOffset + hdr.codeSect.offset));
        Seek (file, archiveOffset + hdr.codeSect.offset, fname);
        if (hdr.isNative) {
            thisSzB = hdr.codeSect.size;
            char *buffer = MALLOC(thisSzB);
            ReadBinFile (file, buffer, thisSzB, fname);
            /* allocate a code object and initialize with the code from the binfile */
            ENABLE_CODE_WRITE
                codeObj = ML_AllocCode (msp, PTR_MLtoC(void, buffer), thisSzB);
            DISABLE_CODE_WRITE
            FlushICache (PTR_MLtoC(char, codeObj), thisSzB);
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
/*DEBUG*/Say("## >>> Run ML %p (%p)\n", PTR_MLtoC(char, codeObj) + thisEntryPoint, val);
        SaveCState (msp, &BinFileList, NIL(ml_val_t *));
        val = ApplyMLFn (msp, closure, val, TRUE);
        RestoreCState (msp, &BinFileList, NIL(ml_val_t *));
/*DEBUG*/Say("## <<< val = %p\n", val);

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
        if (memcmp((char *)perID, STR_MLtoC(id), PERID_LEN) == 0) {
            return (REC_SEL(p, 1));
        }
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
