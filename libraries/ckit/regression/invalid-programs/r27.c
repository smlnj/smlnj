struct t;
struct t *x;
main () {

 struct s {
   struct t *y;
 };

 struct t {
  struct s *y;
 };
 x -> y;
 return(1);
}




