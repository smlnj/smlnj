dnl check_smlnj.m4
dnl
dnl COPYRIGHT (c) 2019 The The SML/NJ Fellowship (http://smlnj.org/)
dnl
dnl @synopsis CHECK_SMLNJ(ACTION-IF-UNKNOWN)
dnl
dnl This macro attempts to figure out the location of the SML/NJ installation,
dnl as well as its version.  It will check the user's path, as well as the
dnl standard locations of /usr/local/smlnj/bin and /usr/local/bin.
dnl You can override the version of SML/NJ used by defining either the SML_CMD
dnl or the SMLNJ_CMD variable in the environment (SMLNJ_CMD is for backwards
dnl compatibility; SML_CMD is prefered).
dnl This macro sets the following shell variables when it executes successfully:
dnl
dnl	SML_CMD*		-- the absolute path to the "sml" command
dnl	SMLNJ_CMD*		-- same as $SML_CMD; for backward compatibility
dnl	SMLNJ_PATH*		-- the
dnl	SMLNJ_VERSION		-- the version as "<major>.<minor>.<patch>", where
dnl				   the ".<patch>" is optional.
dnl	SMLNJ_MAJOR_VERSION	-- major version number
dnl	SMLNJ_MINOR_VERSION	-- minor version number
dnl	SMLNJ_PATCH_VERSION	-- patch number (empty if there is no patch number)
dnl	SMLNJ_ARCH*		-- the host archectecture
dnl	SMLNJ_OPSYS*		-- the host operating system
dnl	SMLNJ_HEAP_SUFFIX*	-- the heap suffix
dnl
dnl * This macro also does an AC_SUBST for the variables marked with "*"
dnl
dnl @author John Reppy <jhr@cs.uchicago.edu>
dnl
AC_DEFUN([CHECK_SMLNJ], [
dnl
dnl first we check for the existence of SML/NJ
dnl
  if test z$SML_CMD != z ; then
    SMLNJ_CMD=$SML_CMD
  elif test z$SMLNJ_CMD != z ; then
    SML_CMD=$SMLNJ_CMD
  else
    AC_PATH_PROGS(SML_CMD, sml, none, [$PATH:/usr/local/smlnj/bin:/usr/local/bin])
    SMLNJ_CMD=$SML_CMD
  fi
dnl
dnl
  if test $SML_CMD = none; then
    $1
  else
dnl
dnl SML/NJ is installed, so determine its location
dnl
    SMLNJ_PATH=`dirname $SML_CMD`
dnl
dnl Determine the version numbers
dnl
    AC_MSG_CHECKING([version of SML/NJ])
    ac_check_smlnj_version=`$SML_CMD @SMLversion`
    if test $? -eq 0 ; then
dnl
dnl normalize the ac_check_smlnj_version variable
dnl
      case $ac_check_smlnj_version in
	sml*) ac_check_smlnj_version=`echo $ac_check_smlnj_version | sed -e 's/sml //'` ;;
	*) AC_MSG_ERROR([bogus SML/NJ version ($ac_check_smlnj_version) reported]);;
      esac
      SMLNJ_VERSION=$ac_check_smlnj_version
      case $ac_check_smlnj_version in
	110)
dnl
dnl Versions 110.0.x report "sml 110" for the @SMLversion flag, so we need to
dnl do some more work.
dnl
	  banner=`echo "" | $SML_CMD | head -1`
	  [ac_check_smlnj_version=`echo $banner \
	    | sed -e 's/.*Version \([0-9.]*\).*/\1/'`]
	  SMLNJ_VERSION=$ac_check_smlnj_version
	  ;;
	*.*.*) ;;
	*.*) ac_check_smlnj_version="$ac_check_smlnj_version".0 ;;
	*) ac_check_smlnj_version="$ac_check_smlnj_version".0.0 ;;
      esac
      [SMLNJ_MAJOR_VERSION=`echo $ac_check_smlnj_version \
	| sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\1/'`]
      [SMLNJ_MINOR_VERSION=`echo $ac_check_smlnj_version \
	| sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\2/'`]
      [SMLNJ_PATCH_VERSION=`echo $ac_check_smlnj_version \
	| sed -e 's/\([0-9]*\).\([0-9]*\).\([0-9]*\)/\3/'`]
      AC_MSG_RESULT([$SMLNJ_VERSION])
      AC_SUBST(SML_CMD)
      AC_SUBST(SMLNJ_CMD)
      AC_SUBST(SMLNJ_PATH)
    else
      $1
    fi
dnl
dnl Determine the heap suffix; we assume that this has the form <arch>-<opsys>
dnl
    AC_MSG_CHECKING([heap suffix of SML/NJ])
    ac_check_smlnj_suffix=`$SML_CMD @SMLsuffix`
    if test $? -eq 0 ; then
      SMLNJ_HEAP_SUFFIX=$ac_check_smlnj_suffix
      [SMLNJ_ARCH=`echo $ac_check_smlnj_suffix \
	| sed -e 's/\([a-z0-9A-Z]*\)-\([a-z0-9A-Z]*\)/\1/'`]
      [SMLNJ_OPSYS=`echo $ac_check_smlnj_suffix \
	| sed -e 's/\([a-z0-9A-Z]*\)-\([a-z0-9A-Z]*\)/\2/'`]
      AC_MSG_RESULT([$SMLNJ_HEAP_SUFFIX])
      AC_SUBST(SMLNJ_HEAP_SUFFIX)
      AC_SUBST(SMLNJ_ARCH)
      AC_SUBST(SMLNJ_OPSYS)
    else
      $1
    fi
  fi
])dnl
