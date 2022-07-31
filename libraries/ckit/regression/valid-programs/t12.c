extern printf ();

int main ()
{ 
  int i;

  for (i=0; i<10; i++) {
    switch (i) {
      case 0:  continue ;
      default: break;
    }

    if (i == 4) break;
    printf ("i = %d\n",i);
  }

  return 0;
}




