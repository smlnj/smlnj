
int main ()
{
  long i_p1555;
  long post_p1561;
  i_p1555 = ((long) (-10));
  goto forStart_p1558;
  
forTop_p1557: 
  ;
  printf ("%lx  ",((long) i_p1555)*3);
  printf ("%lx\n",((unsigned long) i_p1555)*3);
  post_p1561 = i_p1555;
  i_p1555 = (i_p1555+((long) 1));
  
forStart_p1558: 
  ;
  if (i_p1555<11) 
    goto forTop_p1557;
}
