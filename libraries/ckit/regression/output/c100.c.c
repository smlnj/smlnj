
typedef int __int32_t_t67;
typedef unsigned int __uint32_t_t68;
typedef long long __int64_t_t69;
typedef unsigned long long __uint64_t_t70;
typedef __int32_t_t67 __psint_t_t71;
typedef __uint32_t_t68 __psunsigned_t_t72;
typedef __int32_t_t67 __scint_t_t73;
typedef __uint32_t_t68 __scunsigned_t_t74;
typedef unsigned int size_t_t75;
typedef long fpos_t_t76;
typedef __int64_t_t69 off64_t_t77;
typedef __int64_t_t69 fpos64_t_t78;
typedef char *va_list_t79;
struct __file_s_t80 {
  int _cnt;
  char *_ptr;
  char *_base;
  char _flag;
  char _file;
};
typedef struct __file_s_t80 FILE_t81;
extern FILE_t81 __iob[100];
extern FILE_t81 *_lastbuf;
extern char *_bufendtab[];
extern char _sibuf[];
extern char _sobuf[];
extern int remove (char *);
extern int rename (char *,char *);
extern FILE_t81 * tmpfile (void);
extern char * tmpnam (char *);
extern int fclose (FILE_t81 *);
extern int fflush (FILE_t81 *);
extern FILE_t81 * fopen (char *,char *);
extern FILE_t81 * freopen (char *,char *,FILE_t81 *);
extern void setbuf (FILE_t81 *,char *);
extern int setvbuf (FILE_t81 *,char *,int,size_t_t75);
extern int fprintf (FILE_t81 *,char *,...);
extern int fscanf (FILE_t81 *,char *,...);
extern int printf (char *,...);
extern int scanf (char *,...);
extern int sprintf (char *,char *,...);
extern int sscanf (char *,char *,...);
extern int vfprintf (FILE_t81 *,char *,char *);
extern int vprintf (char *,char *);
extern int vsprintf (char *,char *,char *);
extern int fgetc (FILE_t81 *);
extern char * fgets (char *,int,FILE_t81 *);
extern int fputc (int,FILE_t81 *);
extern int fputs (char *,FILE_t81 *);
extern int getc (FILE_t81 *);
extern int getchar (void);
extern char * gets (char *);
extern int putc (int,FILE_t81 *);
extern int putchar (int);
extern int puts (char *);
extern int ungetc (int,FILE_t81 *);
extern size_t_t75 fread (void *,size_t_t75,size_t_t75,FILE_t81 *);
extern size_t_t75 fwrite (void *,size_t_t75,size_t_t75,FILE_t81 *);
extern int fgetpos (FILE_t81 *,fpos_t_t76 *);
extern int fseek (FILE_t81 *,long,int);
extern int fsetpos (FILE_t81 *,fpos_t_t76 *);
extern long ftell (FILE_t81 *);
extern void rewind (FILE_t81 *);
extern void clearerr (FILE_t81 *);
extern int feof (FILE_t81 *);
extern int ferror (FILE_t81 *);
extern void perror (char *);
extern int __filbuf (FILE_t81 *);
extern int __flsbuf (int,FILE_t81 *);
extern FILE_t81 * fdopen (int,char *);
extern int fileno (FILE_t81 *);
extern void flockfile (FILE_t81 *);
extern int ftrylockfile (FILE_t81 *);
extern void funlockfile (FILE_t81 *);
extern int getc_unlocked (FILE_t81 *);
extern int putc_unlocked (int,FILE_t81 *);
extern int getchar_unlocked (void);
extern int putchar_unlocked (int);
extern FILE_t81 * popen (char *,char *);
extern int pclose (FILE_t81 *);
extern int getopt (int,char **,char *);
extern char *optarg;
extern int opterr;
extern int optind;
extern int optopt;
extern int getsubopt (char **,char **,char **);
extern void getoptreset (void);
extern char * ctermid (char *);
extern char * cuserid (char *);
extern char * tempnam (char *,char *);
extern int getw (FILE_t81 *);
extern int putw (int,FILE_t81 *);
extern char * mktemp (char *);
extern int mkstemp (char *);
extern int setbuffer (FILE_t81 *,char *,int);
extern int setlinebuf (FILE_t81 *);
extern int system (char *);
extern int fgetpos64 (FILE_t81 *,fpos64_t_t78 *);
extern FILE_t81 * fopen64 (char *,char *);
extern FILE_t81 * freopen64 (char *,char *,FILE_t81 *);
extern int fseek64 (FILE_t81 *,off64_t_t77,int);
extern int fseeko64 (FILE_t81 *,off64_t_t77,int);
extern int fseeko (FILE_t81 *,__int64_t_t69,int);
extern int fsetpos64 (FILE_t81 *,fpos64_t_t78 *);
extern off64_t_t77 ftell64 (FILE_t81 *);
extern __int64_t_t69 ftello (FILE_t81 *);
extern off64_t_t77 ftello64 (FILE_t81 *);
extern FILE_t81 * tmpfile64 (void);
extern int __semputc (int,FILE_t81 *);
extern int __semgetc (FILE_t81 *);
extern int __us_rsthread_stdio;
extern char * ctermid_r (char *);
int main ()
{
  int c_p600;
  int i_p601;
  int quesCol_p605;
  int pref_p609;
  int quesCol_p610;
  i_p601 = 0;
  goto whileCont_p603;
  
whileTop_p602: 
  ;
  i_p601 = (i_p601+1);
  pref_p609 = i_p601;
  if (pref_p609>100) 
    goto whileBrk_p604;
  if (__us_rsthread_stdio) 
    quesCol_p610 = __semputc (c_p600,(FILE_t81 *) (&__iob[1]));
  else
    {
      int quesCol_p611;
      int pref_p612;
      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
      pref_p612 = ((*(&__iob[1]))._cnt);
      if (pref_p612<0) 
        quesCol_p611 = __flsbuf (c_p600,(FILE_t81 *) (&__iob[1]));
      else
        {
          char *post_p613;
          post_p613 = ((*(&__iob[1]))._ptr);
          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
          *post_p613 = ((char) c_p600);
          quesCol_p611 = ((int) (*post_p613));
        }
      quesCol_p610 = quesCol_p611;
    }
  quesCol_p610;
  
whileCont_p603: 
  ;
  if (__us_rsthread_stdio) 
    quesCol_p605 = __semgetc ((FILE_t81 *) (&__iob[0]));
  else
    {
      int quesCol_p606;
      int pref_p607;
      (*(&__iob[0]))._cnt = (((*(&__iob[0]))._cnt)-1);
      pref_p607 = ((*(&__iob[0]))._cnt);
      if (pref_p607<0) 
        quesCol_p606 = __filbuf ((FILE_t81 *) (&__iob[0]));
      else
        {
          char *post_p608;
          post_p608 = ((*(&__iob[0]))._ptr);
          (*(&__iob[0]))._ptr = ((char *) (((int) ((*(&__iob[0]))._ptr))+1));
          quesCol_p606 = ((int) (*post_p608));
        }
      quesCol_p605 = quesCol_p606;
    }
  c_p600 = quesCol_p605;
  if (c_p600!=(-1)) 
    goto whileTop_p602;
  
whileBrk_p604: 
  ;
}
