/*! \file standalone.c
 *
 * This is the main routine for linking with stand-alone applications.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ml-base.h"
#include "ml-options.h"
#include "ml-limits.h"
#include "ml-globals.h"

#ifdef COLLECT_STATS
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "stats-data.h"
#endif

FILE		*DebugF = NULL;

/* Runtime globals */
bool_t		SilentLoad = TRUE;
bool_t          DumpObjectStrings = FALSE;
bool_t		GCMessages = FALSE;
bool_t		UnlimitedHeap = FALSE;
char		**RawArgs;
char		**CmdLineArgs;	/* does not include the command name (argv[0]) */
char		*MLCmdName;	/* the command name used to invoke the runtime */

PVT void ParseOptions (int argc, char **argv, heap_params_t **heapParams);

int main (int argc, char **argv)
{
    heap_params_t	*heapParams;

    DebugF = stderr;

    /* process the command-line options */
    ParseOptions (argc, argv, &heapParams);

    InitTimers ();
    RecordGlobals ();
    InitCFunList ();

    /* start ML from the linked heap image */
    LoadML (NIL(char *), heapParams);

    Exit (0);

} /* end of main. */


/* ParseOptions:
 *
 * Parse the command-line options.
 */
PVT void ParseOptions (int argc, char **argv, heap_params_t **heapParams)
{
    char	option[MAX_OPT_LEN], *optionArg, **nextArg;
    bool_t	errFlg = FALSE;

  /* first scan for any heap/GC parameters */
    if ((*heapParams = ParseHeapParams(argv)) == NIL(heap_params_t *)) {
	errFlg = TRUE;
    }

    RawArgs = argv;
    CmdLineArgs = NEW_VEC(char *, argc);
    MLCmdName = *argv++;
    nextArg = CmdLineArgs;
    while (--argc > 0) {
	char	*arg = *argv++;

#define MATCH(opt)	(strcmp(opt, option) == 0)
#define CHECK(opt)	{						\
	if (optionArg[0] == '\0') {					\
	    errFlg = TRUE;						\
	    Error("missing argument for \"%s\" option\n", opt);		\
	    continue;							\
	}								\
    } /* CHECK */

	if (isRuntimeOption(arg, option, &optionArg)) {
	    if (MATCH("quiet")) {
		SilentLoad = TRUE;
	    }
	    else if (MATCH("verbose")) {
		SilentLoad = FALSE;
	    }
	    else if (MATCH("objects")) {
	        DumpObjectStrings = TRUE;
	    }
	    else if (MATCH("gcmessages")) {
		GCMessages = TRUE;
	    }
	    else if (MATCH("debug")) {
		CHECK("debug");
		if ((DebugF = fopen(optionArg, "w")) == NULL) {
		    DebugF = stderr; /* restore the file pointer */
		    errFlg = TRUE;
		    Error("unable to open debug output file \"%s\"\n", *(argv[-1]));
		    continue;
		}
	    }
#ifdef COLLECT_STATS
	    else if (MATCH("stats")) {
		CHECK("stats");
		StatsFD = open (optionArg, O_WRONLY|O_TRUNC|O_CREAT, 0666);
		if (StatsFD == -1) {
		    errFlg = TRUE;
		    Error("unable to open statistics file \"%s\"\n", *(argv[-1]));
		    continue;
		}
	    }
#endif
	}
	else {
	    *nextArg++ = arg;
	}
    } /* end while */

    *nextArg = NIL(char *);

    if (errFlg) {
	Exit (1);
    }

} /* end of ParseOptions */


/* Exit:
 * Exit from the ML system.
 */
void Exit (int code)
{
#if COUNT_REG_MASKS
    DumpMasks();
#endif
#ifdef COLLECT_STATS
    if (StatsFD >= 0) {
	STATS_FLUSH_BUF();
	close (StatsFD);
    }
#endif

    exit (code);

} /* end of Exit */
