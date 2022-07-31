
int x;
long y;
extern unsigned long foo (int);
extern int main ();
unsigned long foo (int x_p1487)
{
  unsigned long i_p1488;
  i_p1488 = ((unsigned long) x_p1487);
  return x_p1487;
}
int main ()
{
  int i_p1490;
  switch (i_p1490)
    {
      
      
    case 2: 
      foo (i_p1490);
      goto switchBrk_p1491;
      
    case 3: 
      foo (i_p1490);
      goto switchBrk_p1491;
      
    case 5: 
      foo (i_p1490);
      goto switchBrk_p1491;
      
    case 7: 
      foo (i_p1490);
      goto switchBrk_p1491;
      
    default: 
      foo (i_p1490*2);
    }
  
switchBrk_p1491: 
  ;
}
