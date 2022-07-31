
struct t_t41 {
  struct s_t42 *x;
};
struct s_t42 {
  struct t_t41 *x;
};
int main ()
{
  struct s_t44 {
    struct t_t43 *y;
  };
  struct t_t43 {
    struct s_t44 *y;
  };
  return 1;
}
