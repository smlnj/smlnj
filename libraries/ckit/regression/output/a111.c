
extern int printf ();
extern int i;
int foo ()
{
  
  return i;
}
int i=10;
int main ()
{
  int call_p98;
  call_p98 = foo ();
  printf ("foo = %d\n",call_p98);
  printf ("i = %d\n",i);
  return i;
}
