
struct t248 {
  int count[3];
};
struct t248 *p;
int main ()
{
  int i_p1427;
  int post_p1428;
  i_p1427 = 0;
  post_p1428 = ((*p).count)[i_p1427];
  (*p).count[i_p1427] = (((*p).count)[i_p1427]+1);
  post_p1428;
  (*p).count[i_p1427] = (((*p).count)[i_p1427]+1);
}
