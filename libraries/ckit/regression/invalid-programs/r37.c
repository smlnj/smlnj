main () {
 char *p;
 const char *q;
 char *const r;

 q = p; /* ok */
 p=q;   /* bad */
 r = q; /* bad */
}
