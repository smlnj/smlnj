#!/bin/sh
#
# usage scripts/prepare-lib.sh <dir>
#

function usage {
  echo "usage: scripts/prepare-lib.sh <dir>"
  exit 1
}

if [ ! -x scripts/prepare-lib.sh ] ; then
  usage
fi

if [ $# -ne 1 ] ; then
  usage
fi

d=$1 ; shift

if [ ! -f $d/MODULES ] ; then
  echo "missing $d/MODULES"
  exit 1
fi

lib=$(basename ../../$d/*-lib.cm .cm)
lib_adoc=$d/$lib.adoc
## copy the header
sed -e "s/@DIR@/$d/" -e "s/@LIBRARY@/$lib/" Templates/lib-head.adoc > $lib_adoc
## add module entries
while read -r line ; do
  name=$(echo $line | sed -e 's/structure //' -e 's/signature //' -e 's/functor //')
  case $line in
    signature*)
      template=Templates/sig.adoc
      kw="[.kw]#signature#"
      stem="sig-$name"
      ;;
    structure*)
      template=Templates/str.adoc
      kw="[.kw]#structure#"
      stem="str-$name"
      ;;
    functor*)
      template=Templates/fun.adoc
      kw="[.kw]#functor#"
      stem="fun-$name"
      ;;
  esac
  mod_adoc="$d/$stem.adoc"
  link="xref:$stem.adoc"
  ## add the module to the library file
  echo $link"[\`"$kw $name"\`]::" >> $lib_adoc
  echo "  something" >> $lib_adoc
  echo "" >> $lib_adoc
  ## create the placeholder for the module
  sed -e "s/@DIR@/$d/" -e "s/@LIBRARY@/$lib/" -e "s/@NAME@/$name/" $template > $mod_adoc
done < $d/MODULES
## copy the middle part
sed -e "s/@DIR@/$d/" -e "s/@LIBRARY@/$lib/" Templates/lib-mid.adoc >> $lib_adoc
## add the module entries for the PDF version
while read -r line ; do
  name=$(echo $line | sed -e 's/structure //' -e 's/signature //' -e 's/functor //')
  case $line in
    signature*)
      echo "include::sig-$name.adoc[]" >> $lib_adoc
      echo "" >> $lib_adoc
      ;;
    structure*)
      echo "include::str-$name.adoc[]" >> $lib_adoc
      echo "" >> $lib_adoc
      ;;
    functor*)
      echo "include::fun-$name.adoc[]" >> $lib_adoc
      echo "" >> $lib_adoc
      ;;
  esac
done < $d/MODULES
## copy the footer
sed -e "s/@DIR@/$d/" -e "s/@LIBRARY@/$lib/" Templates/lib-foot.adoc >> $lib_adoc


exit 0
