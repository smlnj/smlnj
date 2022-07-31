typedef int *windowTy[1];

int f (w)
 windowTy w;
{
 return 1;
}

void main(){
  windowTy w;
  f (w);
}
