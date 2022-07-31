/* enum tree_code {a1, a2, a3}; */

struct tree_common
{
  enum tree_code code : 8;
};

struct tree_int_cst
{
  char common[sizeof (struct tree_common)];
};
