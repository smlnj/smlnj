extern printf ();

int main ()
{ 
  int i = 0;

  f1:
  {
    if (i++ < 4) goto f2; else goto f3;
    {
      f2:  printf ("i = %d\n",i);
      goto f1;
    }

    f3:
    printf ("i'= %d\n",i);
  }

}




