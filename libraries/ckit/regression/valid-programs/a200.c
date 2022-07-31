struct foo {int x; float y;};
union bar  {int x; float y;};

main (){

  static char *s0 = "this string";
  static char s1[19] = "Not null terminated";
  static char s2[16] = "Null terminated";
  static char s3[16] = "N";

  char *s4 = "this string";
  char s5[19] = "Not null terminated";
  char s6[16] = "Null terminated";

  /* something weird happens here
  char s7[] = "Also null terminated";
   */

  /* should also work
  static int a1[] = {0,1,2,3,4};
  */ 

  static int a1[5] = {0,1,2,3,4};
  /* This is incorrect */
  static int a2[5];
  int a3[5] = {0,1,2,3,4};
  int a4[5] = {0,1,2};

  static struct foo st1 = {1,2.0};
  static struct foo st2;

  struct foo st3 = {3,4.0};
  
  static union bar u1;       /* 1st component should be statically set to 0 */
  static union bar u2 = {3}; /* 1st component should be statically set to 3 */
  union bar u3;              /* no initialization */
  union bar u4 = {4};        /* 1st component should be dynamically set to 4 */

  static struct foo ast1[3] = {{1,1.0},{2,2.0},{3,3.0}};
  static struct foo ast2[3] = {1,1.0,2,2.0,3,3.0,};
  static struct foo ast3[3] = {1,1.0,2,2.0,{3,3.0,}};
  static struct foo ast4[3] = {1,1.0,2,2.0,{3,}};
  static struct foo ast5[3] = {1,1.0,2,{3,}};

  char z[][3] = {"abc","def"};


  return 0;
}
