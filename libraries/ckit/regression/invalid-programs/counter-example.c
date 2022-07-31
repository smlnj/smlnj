extern int foo (int);

main ()
{
  int i;
  printf ("foo");
  
  /* bad */
  default: printf ("bar\n");
  /* worse */
  case 3: printf ("baz\n");

  switch (i) 
    default:
      if (foo (i))
	case 2: case 3: case 5: case 7:
	  foo (i);
      else 
        case 4: case 6: case 8: case 9: case 10:
          printf ("1");
}

