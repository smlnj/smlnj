dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
dnl All rights reserved.
dnl
dnl This the configuration script for autoconf version 2.60+.

AC_INIT([smlnj-lib-doc],[1.0],[],[],[http://smlnj.org])

AC_PREREQ([2.71])
AC_COPYRIGHT([[COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)]])
AC_CONFIG_SRCDIR(src/JSON/json-lib.adoc)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

sinclude(config/check_smlnj.m4)

dnl
dnl get host information
dnl
AC_CANONICAL_HOST

dnl check for standard programs
dnl
AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl
dnl directory paths
dnl
case "$host_os" in
  mingw32)
    ROOT_DIR=`pwd -W`
  ;;
  *)
    ROOT_DIR=`pwd`
  ;;
esac

SRC_DIR=$ROOT_DIR/src
MK_DIR=$ROOT_DIR/mk
SRC_CONFIG_DIR=$SRC_DIR/config
SRC_STYLES_DIR=$SRC_DIR/styles
TOOL_BIN_DIR=$ROOT_DIR/tools/bin

AC_SUBST(ROOT_DIR)
AC_SUBST(SRC_DIR)
AC_SUBST(MK_DIR)
AC_SUBST(SRC_CONFIG_DIR)
AC_SUBST(SRC_STYLES_DIR)
AC_SUBST(TOOL_BIN_DIR)

dnl
dnl ******************** get SML/NJ root directory ********************
dnl
AC_ARG_WITH([smlnj-root],
  AS_HELP_STRING([--with-smlnj-root=DIR], [specify the SML/NJ root directory path]),
  [SMLNJ_ROOT=$withval],
  [SMLNJ_ROOT=none])
if test x"$SMLNJ_ROOT" = xnone ; then
  if test -d $ROOT_DIR/../../base/compiler ; then
    SMLNJ_ROOT=$ROOT_DIR/../..
  else
    AC_MSG_ERROR([please specify the SML/NJ root directory path])
  fi
fi
AC_SUBST(SMLNJ_ROOT)

dnl
dnl ******************** get the output directory ********************
dnl
AC_ARG_WITH([output-dir],
  AS_HELP_STRING([--with-output-dir=DIR], [specify the output directory path]),
  [OUT_DIR=$withval],
  [OUT_DIR=none])
if test x"$OUT_DIR" = xnone ; then
  OUT_DIR=$ROOT_DIR/doc/html
fi
AC_SUBST(OUT_DIR)

dnl
dnl ******************** get SML/NJ release number and date ********************
dnl
SMLNJ_DATE=`date "+%B %d, %Y"`

AC_ARG_WITH([version],
  AS_HELP_STRING([--with-version=VERS], [specify the SML/NJ release version]),
  [SMLNJ_VERSION=$withval],
  [SMLNJ_VERSION=none])
if test x"$SMLNJ_VERSION" = xnone ; then
  if test -r $SMLNJ_ROOT/config/version ; then
    SMLNJ_VERSION=`cat $SMLNJ_ROOT/config/version`
    AC_MSG_NOTICE(["SML/NJ release version: $SMLNJ_VERSION"])
  else
    SMLNJ_VERSION="working"
    AC_MSG_NOTICE(["SML/NJ working version"])
  fi
fi
AC_SUBST(SMLNJ_VERSION)

AC_ARG_WITH([date],
  AS_HELP_STRING([--with-date=DATE], [specify the SML/NJ release date]),
  [SMLNJ_DATE=$withval],
  [SMLNJ_DATE=none])
if test x"$SMLNJ_DATE" = xnone ; then
  if test x"$SMLNJ_VERSION" = xworking ; then
    #
    # for working versions, we just use the current date
    #
    SMLNJ_DATE=`date "+%B %d, %Y"`
  else
    if test -r $SMLNJ_ROOT/config/releasedate ; then
      SMLNJ_DATE=`cat $SMLNJ_ROOT/config/releasedate`
    else
      SMLNJ_DATE=`date "+%B %d, %Y"`
    fi
  fi
fi
AC_MSG_NOTICE(["SML/NJ release date: $SMLNJ_DATE"])
AC_SUBST(SMLNJ_DATE)

dnl
dnl ******************** get SML/NJ info (for generation tools) ********************
dnl

dnl check that we have a compatible version of SML/NJ; we need at least 110.98 because
dnl of a bug in earlier versions of the JSON library.
dnl
CHECK_SMLNJ([AC_MSG_ERROR([unable to find SML/NJ installation; please add it to your PATH or set SML_CMD])])
if test $SMLNJ_MAJOR_VERSION -lt 110 \
  -o \( $SMLNJ_MAJOR_VERSION -eq 110 -a $SMLNJ_MINOR_VERSION -lt 98 \)
then
  AC_MSG_ERROR([installation requires SML/NJ version 110.97+ (available from smlnj.org)])
fi

dnl
dnl look for ml-build
dnl
tmpPATH="$SMLNJ_PATH:$PATH"
AC_PATH_PROG(ML_BUILD, ml-build, none, $tmpPATH)
if test $ML_BUILD = none ; then
  AC_MSG_ERROR([ml-build not found])
fi
AC_SUBST(ML_BUILD)

dnl
dnl ******************** check for asciidoctor ********************
dnl
AC_PATH_PROGS([ASCIIDOCTOR], [asciidoctor], [none])

if test x"$ASCIIDOCTOR" = xnone ; then
  AC_MSG_ERROR([documentation generation requires asciidoctor])
fi

AC_SUBST(ASCIIDOCTOR)

dnl
dnl ******************** check for latexmk ********************
dnl
AC_PATH_PROGS([LATEXMK], [latexmk], [none])

if test x"$LATEXMK" = xnone ; then
  AC_MSG_ERROR([documentation generation requires latexmk (from texlive)])
fi

AC_SUBST(LATEXMK)

dnl
dnl ******************** write output ********************
dnl
AC_CONFIG_FILES(
dnl ***** Makefiles *****
  Makefile
  src/Makefile
  tools/Makefile
  tools/gen-pages/Makefile
  tools/make-index/Makefile
dnl ***** SML files *****
  tools/gen-pages/config.sml:tools/gen-pages/config_sml.in
)

dnl
dnl shell scripts
dnl
AC_CONFIG_FILES([
  bin/install-sml-wrapper.sh:config/install-sml-wrapper_sh.in
], [chmod +x bin/install-sml-wrapper.sh])

AC_OUTPUT
