/* timers.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * OS independent timer routines; these rely on a OS dependent implementation
 * of the following function:
 *
 *	void GetCPUTime (Time_t *user_t, Time_t *sys_t);
 */

#include "ml-base.h"
#include "vproc-state.h"
#include "ml-timer.h"


/* ResetTimers:
 *
 * Clear the GC timers.
 */
void ResetTimers (vproc_state_t *vsp)
{
    vsp->vp_gcUsrTime->seconds = 0;
    vsp->vp_gcUsrTime->uSeconds = 0;
    vsp->vp_gcSysTime->seconds = 0;
    vsp->vp_gcSysTime->uSeconds = 0;

} /* end of ResetTimers. */


/* StartGCTimer:
 */
void StartGCTimer (vproc_state_t *vsp)
{
    GetCPUTime (vsp->vp_gcUsrTime0, vsp->vp_gcSysTime0);

} /* end of StartGCTimer */


/* StopGCTimer:
 *
 * Stop the garbage collection timer and update the cumulative garbage collection
 * time.  If time is not NIL, then return the time (in ms.) spent since
 * the start of the GC.
 */
void StopGCTimer (vproc_state_t *vsp, long *time)
{
    int			sec, usec;
    Time_t		ut, st;
    Time_t		*usr0 = vsp->vp_gcUsrTime0;
    Time_t		*sys0 = vsp->vp_gcSysTime0;
    Time_t		*usr = vsp->vp_gcUsrTime;
    Time_t		*sys = vsp->vp_gcSysTime;

    GetCPUTime (&ut, &st);

  /* First: process the user time */
    sec = ut.seconds - usr0->seconds;
    usec = ut.uSeconds - usr0->uSeconds;

    if (usec < 0) {
        sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
        sec++; usec -= 1000000;
    }
    if (time != NIL(long *)) {
	*time = (usec/1000 + sec*1000);
    }

    sec = usr->seconds + sec;
    usec = usr->uSeconds + usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
	sec++; usec -= 1000000;
    }
    usr->seconds = sec;
    usr->uSeconds = usec;

  /* Second: process the system time */
    sec = st.seconds - sys0->seconds;
    usec = st.uSeconds - sys0->uSeconds;

    if (usec < 0) {
        sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
        sec++; usec -= 1000000;
    }

    sec = sys->seconds + sec;
    usec = sys->uSeconds + usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
	sec++; usec -= 1000000;
    }
    sys->seconds = sec;
    sys->uSeconds = usec;

} /* end of StopGCTimer */

