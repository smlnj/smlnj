main (){

  int x = 1;
  const int y[3] = {2,1,3};
  volatile int j = ++x;

  return (x-j);
}
