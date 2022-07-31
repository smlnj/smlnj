struct foo {int m;};

int i;

struct foo j;

int *f (){

  return &i;
}

struct foo *g (){

  return &j;
}

main () {
  struct foo *p,pp;

  *f() += 1;

   p->m += 1;

   (p++)->m += 1;

   (++p)->m += 1;

   (*p).m +=1;

   pp.m +=1;

   (*g()).m +=1;
}
