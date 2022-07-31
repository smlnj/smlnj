struct t {
  struct s *x;
};

struct s {
  struct t *x;
};

struct s {
  struct t *x;
};

main () {
 struct t;
 struct s {
   struct t *y;
 };
 struct t {
   struct s *y;
 };
 return(1);
}
