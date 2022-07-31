void printf();

int main () {
  int j;
  int i; 

  j = (i = 3) ? 4 : 5;

  printf ("i=%d, j=%d\n",i,j);

  return (j?0:j-j);
}

