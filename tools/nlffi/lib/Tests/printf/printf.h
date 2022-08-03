extern int printf (const char *, ...);
typedef int (*printf_int_int) (const char *, int, int);
typedef int (*printf_int_double) (const char *, int, double);
typedef int (*printf_int_string_pointer) (const char *, int, char*, void *);
