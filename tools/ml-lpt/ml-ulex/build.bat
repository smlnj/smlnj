@ECHO off
%COMSPEC% /C "..\..\bin\ml-build.bat -D NO_ML_ANTLR -D NO_ML_LEX -D NO_ML_YACC sources.cm Main.main ml-ulex"
