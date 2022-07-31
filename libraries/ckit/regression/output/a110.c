
extern int printf ();
extern int i;
int foo ()
{
  
  return i;
}
int i=10;
int main ()
{
  int call_p92;
  call_p92 = foo ();
  printf ("foo = %d\n",call_p92);
  printf ("i = %d\n",i);
  return i;
}
