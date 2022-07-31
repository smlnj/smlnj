
int (**x[4]) (int,int,int);
int (*(*y)[10]) (int,int,int);
extern int nar (int,int,int);
int nar (int x_p1510,int y_p1511,int z_p1512)
{
  
  return 3;
}
typedef int bar_t253;
static int g;
static int narn (int x_p1516,int y_p1517,int z_p1518)
{
  
  return (x_p1516+y_p1517)+z_p1518;
}
void main ()
{
  int (**x1_p1520) (int,int,int);
  int (*x2) (int,int,int);
  int (**y1_p1522) (int,int,int);
  int (*y2) (int,int,int);
  int i_p1524;
  int j_p1525;
  int call_p1526;
  int call_p1527;
  *x[1] = nar;
  x1_p1520 = x[1];
  x2 = (*x1_p1520);
  y1_p1522 = ((int (**) (int,int,int)) (*y));
  y2 = y1_p1522[0];
  call_p1526 = x2 (4,5,6);
  i_p1524 = call_p1526;
  call_p1527 = y2 (4,5,6);
  j_p1525 = call_p1527;
  return ;
}
