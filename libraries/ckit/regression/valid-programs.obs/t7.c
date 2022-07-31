int (**x[4]) (int,int,int);

int (*(*y)[10]) (int,int,int);

extern int (nar) (int x,int y,int z);

int nar (int x,int y,int z) {
  return 3;
}

typedef int bar;

static int g;

static int narn (int x,int y,int z) {
  return (x+y+z);
}

void main ()
{ int (**x1) (int,int,int);
  int (*x2) (int,int,int);

  int (**y1) (int,int,int);
  int (*y2) (int,int,int);

  int i,j;

  *x[1] = &nar;
  x1 = x[1];
  x2 = *x1;

  
  y1 = *y;
  y2 = y1[0];


  i = (*x2) (4,5,6);
  j = (*y2)(4,5,6);
  return;
}




