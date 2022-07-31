enum t {
 g1, g2
};


struct t {
  enum t *x;
};

struct s {
  enum t *x;
};

main () {

 return(1);
}
