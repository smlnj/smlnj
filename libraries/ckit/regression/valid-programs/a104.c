int a[4] /*= {1,2,3,4}*/;

int f (int x, int y, int z){
  return x + y + z;
}

int main () {
  int s = 3;
  return (f (a[2],(a[3] = 5,a[0]),a[1]));
}

