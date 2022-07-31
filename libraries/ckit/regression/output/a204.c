
struct foo_t29 {
  int a[3];
  int b;
};
struct foo_t29 w[2]={{{1,0,0},0},{{2,0,0},0}};
struct foo_t29 z[9]={{{2,3,4},0},{{5,0,0},0},{{1,42,44},13},{{1,2,2},2},{{3,0,0},0},{{4,0,0},0},{{0,0,0},0},{{0,0,0},0},{{0,0,0},0}};
int main ()
{
  int i_p261;
  int j_p262;
  int post_p268;
  int post_p273;
  i_p261 = 0;
  goto forStart_p265;
  
forTop_p264: 
  ;
  printf ("a=");
  j_p262 = 0;
  goto forStart_p270;
  
forTop_p269: 
  ;
  printf ("%10d ",z[i_p261].a[j_p262]);
  post_p273 = j_p262;
  j_p262 = (j_p262+1);
  
forStart_p270: 
  ;
  if (j_p262<3) 
    goto forTop_p269;
  printf ("b=%d\n",z[i_p261].b);
  post_p268 = i_p261;
  i_p261 = (i_p261+1);
  
forStart_p265: 
  ;
  if (i_p261<(144/16)) 
    goto forTop_p264;
}
