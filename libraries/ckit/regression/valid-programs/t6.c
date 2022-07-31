typedef struct {int x; int y;} point;

int (*x)[10];

int *(y[42]);

int (*(z[55]))[66][77];

int (*a) (int (*)(int),int,int);


void myfunc (int x, int y){
  return;
}

void (*fp) (int,int) = myfunc;

void main ()
{
  point p;
  int *l;

  return;
}


