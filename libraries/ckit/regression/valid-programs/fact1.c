void printf();

main (){
  int i,a;

  for (i=1,a=1; i<=6;i++)
    {
      a = a * i;
    }

  printf ("fact 6 = %d\n",a);

  i = 1;
  a = 1;

  while (i<=7)
    {
      a = a * i++;
    }

  printf ("fact 7 = %d\n",a);


  i = 1;
  a = 1;

  do
    {
      a = a * i++;
    } while (i<=8);

  printf ("fact 8 = %d\n",a);
}
      
