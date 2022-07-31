
struct foo_t61 {
  int m;
};
int i;
struct foo_t61 j;
int * f ()
{
  
  return &i;
}
struct foo_t61 * g ()
{
  
  return &j;
}
int main ()
{
  struct foo_t61 *p_p469;
  struct foo_t61 pp_p470;
  int *call_p471;
  struct foo_t61 *post_p472;
  struct foo_t61 *pref_p473;
  struct foo_t61 *call_p474;
  call_p471 = f ();
  *call_p471 = ((*call_p471)+1);
  (*p_p469).m = (((*p_p469).m)+1);
  post_p472 = p_p469;
  p_p469 = ((struct foo_t61 *) (((int) p_p469)+4));
  (*post_p472).m = (((*post_p472).m)+1);
  p_p469 = ((struct foo_t61 *) (((int) p_p469)+4));
  pref_p473 = p_p469;
  (*pref_p473).m = (((*pref_p473).m)+1);
  (*p_p469).m = (((*p_p469).m)+1);
  pp_p470.m = ((pp_p470.m)+1);
  call_p474 = g ();
  (*call_p474).m = (((*call_p474).m)+1);
}
