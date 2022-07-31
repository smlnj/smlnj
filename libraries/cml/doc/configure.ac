dnl Process this file with autoconf (2.52+) to produce a configure script.
dnl
dnl COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
dnl All rights reserved.
dnl

AC_INIT(CMLManual,1.1)

AC_PREREQ(2.52)
AC_COPYRIGHT([COPYRIGHT (c) 2002 John Reppy] (http://www.cs.uchicago.edu/~jhr))
AC_CONFIG_SRCDIR(ML-Doc/cml.mldoc)
AC_CONFIG_AUX_DIR(config)

BASIS_DIR=`pwd`
AC_SUBST(BASIS_DIR)

dnl
dnl find location of ML-Doc tools
dnl
AC_ARG_WITH(mldoc,
  [  --with-mldoc=PATH      specify location of ML-Doc tools],
  MLDOC_DIR=$with_mldoc)
if test x$MLDOC_DIR = xyes -o x$MLDOC_DIR = xno ; then
  AC_MSG_ERROR([--with-mldoc option must specify directory argument])
elif test x$MLDOC_DIR != x ; then
dnl
dnl verify that $MLDOC_DIR is an absolute path
dnl
  case $MLDOC_DIR in
    /*) ;;
    *) AC_MSG_ERROR([--with-mldoc argument must be absolute]) ;;
  esac
fi
AC_SUBST(MLDOC_DIR)

dnl
dnl Get location of SML Basis Library Master.info
dnl file.
dnl
AC_ARG_WITH(basis-info,
  [  --with-basis-info=PATH specify location of SML Basis master info file],
  BASIS_INFO_PATH=$with_basis_info)
if test x$BASIS_INFO_PATH = xyes -o x$BASIS_INFO_PATH = xno ; then
  AC_MSG_ERROR([--with-basis-info option must specify directory argument])
elif test x$BASIS_INFO_PATH != x ; then
dnl
dnl verify the existance of the file
dnl
  case $BASIS_INFO_PATH in
    /*) ;;
    *) AC_MSG_ERROR([--with-basis-info argument must be absolute]) ;;
  esac
  if test ! -r $BASIS_INFO_PATH ; then
    AC_MSG_ERROR([info file "$BASIS_INFO_PATH" not found])
  fi
else
  AC_MSG_ERROR([must specify location of SML Basis Library info file])
fi
AC_SUBST(BASIS_INFO_PATH)

dnl
dnl Generate makefiles
dnl
$MLDOC_DIR/bin/mk-mldoc-makefile < input

AC_CONFIG_FILES(
  CATALOG:config/CATALOG.in \
  Config.cfg:config/Config_cfg.in \
  Hardcopy/run-latex:config/run-latex.in \
)

AC_CONFIG_COMMANDS_PRE([if test ! -d Info ; then mkdir Info; fi])
AC_CONFIG_COMMANDS_PRE([if test ! -d HTML ; then mkdir HTML; fi])
AC_CONFIG_COMMANDS_PRE([if test ! -d Hardcopy ; then mkdir Hardcopy; fi])
AC_CONFIG_COMMANDS_PRE([if test ! -d Proof ; then mkdir Proof; fi])

AC_OUTPUT
