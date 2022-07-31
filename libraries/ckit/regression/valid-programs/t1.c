extern int printf();

main ()
{
  printf ("this is the end, my only friend the end");
  goto label_1;

label_1:
    printf ("this is the end, my only friend the end");
    goto label_2;
    goto label_1;

label_2: 
  return 0;
}

