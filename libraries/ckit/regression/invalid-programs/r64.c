
  struct foo_t{
    int x;
    int y;
  };
 
  void f(int x0, int y0){
    struct foo_t myfoo = {x0,y0};
  }

  void main(){
    f(0,0);
  }

