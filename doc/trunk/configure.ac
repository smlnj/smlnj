dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
dnl All rights reserved.
dnl
dnl This the configuration script for autoconf version 2.60+.

AC_INIT([smlnj-doc],[1.0],[],[],[http://smlnj.org])

AC_PREREQ(2.70)
AC_COPYRIGHT([[COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)]])
AC_CONFIG_SRCDIR(src/man/smlnj.7.adoc)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

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
OUT_DIR=$ROOT_DIR/doc
WEB_OUT_DIR=$ROOT_DIR/htdocs
MK_DIR=$ROOT_DIR/mk
SRC_CONFIG_DIR=$SRC_DIR/config
SRC_STYLES_DIR=$SRC_DIR/styles

AC_SUBST(ROOT_DIR)
AC_SUBST(OUT_DIR)
AC_SUBST(WEB_OUT_DIR)
AC_SUBST(SRC_DIR)
AC_SUBST(MK_DIR)
AC_SUBST(SRC_CONFIG_DIR)
AC_SUBST(SRC_STYLES_DIR)

dnl
dnl ******************** get SML/NJ release number and date ********************
dnl
SMLNJ_DATE=`date "+%B %d, %Y"`

AC_ARG_WITH([version],
  AS_HELP_STRING([--with-version], ["specify the SML/NJ release version"]),
  [SMLNJ_VERSION=$withval],
  [SMLNJ_VERSION=none])
if test x"$SMLNJ_VERSION" = xnone ; then
  if test -r ../config/version ; then
    SMLNJ_VERSION=`cat ../config/version`
    AC_MSG_NOTICE(["SML/NJ release version: $SMLNJ_VERSION"])
  else
    SMLNJ_VERSION="working"
    AC_MSG_NOTICE(["SML/NJ working version"])
  fi
fi
AC_SUBST(SMLNJ_VERSION)

AC_ARG_WITH([date],
  AS_HELP_STRING([--with-date], ["specify the SML/NJ release date"]),
  [SMLNJ_DATE=$withval],
  [SMLNJ_DATE=none])
if test x"$SMLNJ_DATE" = xnone ; then
  if test x"$SMLNJ_VERSION" = xworking ; then
    #
    # for working versions, we just use the current date
    #
    SMLNJ_DATE=`date "+%B %d, %Y"`
  else
    if test -r ../config/releasedate ; then
      SMLNJ_DATE=`cat ../config/releasedate`
    else
      SMLNJ_DATE=`date "+%B %d, %Y"`
    fi
  fi
fi
AC_MSG_NOTICE(["SML/NJ release date: $SMLNJ_DATE"])
AC_SUBST(SMLNJ_DATE)

dnl
dnl ******************** check for asciidoctor ********************
dnl
AC_PATH_PROGS([ASCIIDOCTOR], [asciidoctor], [none])

dnl
dnl error check for availability of tools
dnl
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
  mk/doc-rules.gmk:config/doc-rules_gmk.in
  Makefile
  src/Makefile
  src/changelog/Makefile
  src/extensions/Makefile
  src/install-notes/Makefile
  src/man/Makefile
  src/manuals/Makefile
  src/release-notes/Makefile
  src/smlnj-guide/Makefile
)

AC_OUTPUT
