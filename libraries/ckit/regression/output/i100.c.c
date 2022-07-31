
typedef int __int32_t_t82;
typedef unsigned int __uint32_t_t83;
typedef long long __int64_t_t84;
typedef unsigned long long __uint64_t_t85;
typedef __int32_t_t82 __psint_t_t86;
typedef __uint32_t_t83 __psunsigned_t_t87;
typedef __int32_t_t82 __scint_t_t88;
typedef __uint32_t_t83 __scunsigned_t_t89;
typedef unsigned int size_t_t90;
typedef long fpos_t_t91;
typedef __int64_t_t84 off64_t_t92;
typedef __int64_t_t84 fpos64_t_t93;
typedef char *va_list_t94;
struct __file_s_t95 {
  int _cnt;
  char *_ptr;
  char *_base;
  char _flag;
  char _file;
};
typedef struct __file_s_t95 FILE_t96;
extern FILE_t96 __iob[100];
extern FILE_t96 *_lastbuf;
extern char *_bufendtab[];
extern char _sibuf[];
extern char _sobuf[];
extern int remove (char *);
extern int rename (char *,char *);
extern FILE_t96 * tmpfile (void);
extern char * tmpnam (char *);
extern int fclose (FILE_t96 *);
extern int fflush (FILE_t96 *);
extern FILE_t96 * fopen (char *,char *);
extern FILE_t96 * freopen (char *,char *,FILE_t96 *);
extern void setbuf (FILE_t96 *,char *);
extern int setvbuf (FILE_t96 *,char *,int,size_t_t90);
extern int fprintf (FILE_t96 *,char *,...);
extern int fscanf (FILE_t96 *,char *,...);
extern int printf (char *,...);
extern int scanf (char *,...);
extern int sprintf (char *,char *,...);
extern int sscanf (char *,char *,...);
extern int vfprintf (FILE_t96 *,char *,char *);
extern int vprintf (char *,char *);
extern int vsprintf (char *,char *,char *);
extern int fgetc (FILE_t96 *);
extern char * fgets (char *,int,FILE_t96 *);
extern int fputc (int,FILE_t96 *);
extern int fputs (char *,FILE_t96 *);
extern int getc (FILE_t96 *);
extern int getchar (void);
extern char * gets (char *);
extern int putc (int,FILE_t96 *);
extern int putchar (int);
extern int puts (char *);
extern int ungetc (int,FILE_t96 *);
extern size_t_t90 fread (void *,size_t_t90,size_t_t90,FILE_t96 *);
extern size_t_t90 fwrite (void *,size_t_t90,size_t_t90,FILE_t96 *);
extern int fgetpos (FILE_t96 *,fpos_t_t91 *);
extern int fseek (FILE_t96 *,long,int);
extern int fsetpos (FILE_t96 *,fpos_t_t91 *);
extern long ftell (FILE_t96 *);
extern void rewind (FILE_t96 *);
extern void clearerr (FILE_t96 *);
extern int feof (FILE_t96 *);
extern int ferror (FILE_t96 *);
extern void perror (char *);
extern int __filbuf (FILE_t96 *);
extern int __flsbuf (int,FILE_t96 *);
extern FILE_t96 * fdopen (int,char *);
extern int fileno (FILE_t96 *);
extern void flockfile (FILE_t96 *);
extern int ftrylockfile (FILE_t96 *);
extern void funlockfile (FILE_t96 *);
extern int getc_unlocked (FILE_t96 *);
extern int putc_unlocked (int,FILE_t96 *);
extern int getchar_unlocked (void);
extern int putchar_unlocked (int);
extern FILE_t96 * popen (char *,char *);
extern int pclose (FILE_t96 *);
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
extern int getw (FILE_t96 *);
extern int putw (int,FILE_t96 *);
extern char * mktemp (char *);
extern int mkstemp (char *);
extern int setbuffer (FILE_t96 *,char *,int);
extern int setlinebuf (FILE_t96 *);
extern int system (char *);
extern int fgetpos64 (FILE_t96 *,fpos64_t_t93 *);
extern FILE_t96 * fopen64 (char *,char *);
extern FILE_t96 * freopen64 (char *,char *,FILE_t96 *);
extern int fseek64 (FILE_t96 *,off64_t_t92,int);
extern int fseeko64 (FILE_t96 *,off64_t_t92,int);
extern int fseeko (FILE_t96 *,__int64_t_t84,int);
extern int fsetpos64 (FILE_t96 *,fpos64_t_t93 *);
extern off64_t_t92 ftell64 (FILE_t96 *);
extern __int64_t_t84 ftello (FILE_t96 *);
extern off64_t_t92 ftello64 (FILE_t96 *);
extern FILE_t96 * tmpfile64 (void);
extern int __semputc (int,FILE_t96 *);
extern int __semgetc (FILE_t96 *);
extern int __us_rsthread_stdio;
extern char * ctermid_r (char *);
int main ()
{
  int c_p743;
  int i_p744;
  int quesCol_p748;
  int pref_p752;
  int quesCol_p753;
  i_p744 = 0;
  goto whileCont_p746;
  
whileTop_p745: 
  ;
  i_p744 = (i_p744+1);
  pref_p752 = i_p744;
  if (pref_p752>100) 
    {
      
      printf ((char *) "\n");
      i_p744 = 0;
    }
  if (c_p743==10) 
    i_p744 = 0;
  if (__us_rsthread_stdio) 
    quesCol_p753 = __semputc (c_p743,(FILE_t96 *) (&__iob[1]));
  else
    {
      int quesCol_p754;
      int pref_p755;
      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
      pref_p755 = ((*(&__iob[1]))._cnt);
      if (pref_p755<0) 
        quesCol_p754 = __flsbuf (c_p743,(FILE_t96 *) (&__iob[1]));
      else
        {
          char *post_p756;
          post_p756 = ((*(&__iob[1]))._ptr);
          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
          *post_p756 = ((char) c_p743);
          quesCol_p754 = ((int) (*post_p756));
        }
      quesCol_p753 = quesCol_p754;
    }
  quesCol_p753;
  
whileCont_p746: 
  ;
  if (__us_rsthread_stdio) 
    quesCol_p748 = __semgetc ((FILE_t96 *) (&__iob[0]));
  else
    {
      int quesCol_p749;
      int pref_p750;
      (*(&__iob[0]))._cnt = (((*(&__iob[0]))._cnt)-1);
      pref_p750 = ((*(&__iob[0]))._cnt);
      if (pref_p750<0) 
        quesCol_p749 = __filbuf ((FILE_t96 *) (&__iob[0]));
      else
        {
          char *post_p751;
          post_p751 = ((*(&__iob[0]))._ptr);
          (*(&__iob[0]))._ptr = ((char *) (((int) ((*(&__iob[0]))._ptr))+1));
          quesCol_p749 = ((int) (*post_p751));
        }
      quesCol_p748 = quesCol_p749;
    }
  c_p743 = quesCol_p748;
  if (c_p743!=(-1)) 
    goto whileTop_p745;
}
