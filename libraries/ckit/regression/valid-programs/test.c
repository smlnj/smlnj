void printf();

main()
{

  signed long i;

  for(i=-10; i<11;  i++) {
    printf("%lx  ", ((signed long)i) * 0x3);
    printf("%lx\n", ((unsigned long)i) * 0x3);
  }

}

