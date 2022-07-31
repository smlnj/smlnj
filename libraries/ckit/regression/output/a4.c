
int * f ()
{
  int i_p429;
  i_p429 = 1;
  return &i_p429;
}
int main ()
{
  void *j_p431;
  int *call_p432;
  call_p432 = f ();
  j_p431 = ((void *) call_p432);
}
