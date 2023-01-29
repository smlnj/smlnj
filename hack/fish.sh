fish() {
  cd "$1"
  ORIG_CM_DIR_ARC=unknown
  for i in * .[a-zA-Z0-9]* ; do
    if [ -d $i ] ; then
      echo  $i
      break
    fi
  done
}
