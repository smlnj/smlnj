extern int getopt ();

main (int argc, int argv){
  int i;

  /*
    while((i = getopt (argc, argv, "c:a:f:F:") != -1)){}
   */

  while ((i = getopt(argc, argv, "c:a:f:F:")) != -1)
  {
    continue;
  }
  

  return 0;

}
