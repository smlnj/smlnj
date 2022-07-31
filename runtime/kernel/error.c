/* error.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * Run-time system error messages.
 */

#include <stdio.h>
#include <stdarg.h>
#include "ml-base.h"

extern FILE	*DebugF;

/* Say:
 * Print a message to the standard output.
 */
void Say (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    vfprintf (stdout, fmt, ap);
    va_end(ap);
    fflush (stdout);

} /* end of Say */

/* SayDebug:
 * Print a message to the debug output stream.
 */
void SayDebug (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    vfprintf (DebugF, fmt, ap);
    va_end(ap);
    fflush (DebugF);

} /* end of SayDebug */

/* Error:
 * Print an error message.
 */
void Error (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    fprintf (stderr, "%s: Error -- ", MLCmdName);
    vfprintf (stderr, fmt, ap);
    va_end(ap);

} /* end of Error */


/* Die:
 * Print an error message and then exit.
 */
void Die (const char *fmt, ...)
{
    va_list	ap;

    va_start (ap, fmt);
    fprintf (stderr, "%s: Fatal error -- ", MLCmdName);
    vfprintf (stderr, fmt, ap);
    fprintf (stderr, "\n");
    va_end(ap);

    Exit (1);

} /* end of Die */


#ifdef ASSERT_ON
/* AssertFail:
 *
 * Print an assertion failure message.
 */
void AssertFail (const char *a, const char *file, int line)
{
    fprintf (stderr, "%s: Assertion failure (%s) at \"%s:%d\"\n",
	MLCmdName, a, file, line);

    Exit (2);

} /* end of AssertFail */
#endif

