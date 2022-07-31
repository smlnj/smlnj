
extern int narn (int,int,int);
int narn (int x_p1546,int y_p1547,int z_p1548)
{
  
  return 1;
}
void main ()
{
  register int j_p1550;
  static int k_p1551;
  int k_p1552;
  int call_p1553;
  call_p1553 = narn (k_p1552,j_p1550,j_p1550);
  k_p1552 = call_p1553;
  j_p1550 = k_p1552;
  return ;
}
