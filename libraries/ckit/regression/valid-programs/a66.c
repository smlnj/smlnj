void main() {

  char a, b, c, d, e;

  a?b:c?d:e;     /* Line 5. */
  (a?b:c)?d:e;   /* Line 6. */
  a?b:(c?d:e);   /* Line 7. */
}
