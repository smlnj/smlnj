
typedef int __int32_t_t284;
typedef unsigned int __uint32_t_t285;
typedef long long __int64_t_t286;
typedef unsigned long long __uint64_t_t287;
typedef __int32_t_t284 __psint_t_t288;
typedef __uint32_t_t285 __psunsigned_t_t289;
typedef __int32_t_t284 __scint_t_t290;
typedef __uint32_t_t285 __scunsigned_t_t291;
typedef unsigned int size_t_t292;
typedef long fpos_t_t293;
typedef __int64_t_t286 off64_t_t294;
typedef __int64_t_t286 fpos64_t_t295;
typedef char *va_list_t296;
struct __file_s_t297 {
  int _cnt;
  char *_ptr;
  char *_base;
  char _flag;
  char _file;
};
typedef struct __file_s_t297 FILE_t298;
extern FILE_t298 __iob[100];
extern FILE_t298 *_lastbuf;
extern char *_bufendtab[];
extern char _sibuf[];
extern char _sobuf[];
extern int remove (char *);
extern int rename (char *,char *);
extern FILE_t298 * tmpfile (void);
extern char * tmpnam (char *);
extern int fclose (FILE_t298 *);
extern int fflush (FILE_t298 *);
extern FILE_t298 * fopen (char *,char *);
extern FILE_t298 * freopen (char *,char *,FILE_t298 *);
extern void setbuf (FILE_t298 *,char *);
extern int setvbuf (FILE_t298 *,char *,int,size_t_t292);
extern int fprintf (FILE_t298 *,char *,...);
extern int fscanf (FILE_t298 *,char *,...);
extern int printf (char *,...);
extern int scanf (char *,...);
extern int sprintf (char *,char *,...);
extern int sscanf (char *,char *,...);
extern int vfprintf (FILE_t298 *,char *,char *);
extern int vprintf (char *,char *);
extern int vsprintf (char *,char *,char *);
extern int fgetc (FILE_t298 *);
extern char * fgets (char *,int,FILE_t298 *);
extern int fputc (int,FILE_t298 *);
extern int fputs (char *,FILE_t298 *);
extern int getc (FILE_t298 *);
extern int getchar (void);
extern char * gets (char *);
extern int putc (int,FILE_t298 *);
extern int putchar (int);
extern int puts (char *);
extern int ungetc (int,FILE_t298 *);
extern size_t_t292 fread (void *,size_t_t292,size_t_t292,FILE_t298 *);
extern size_t_t292 fwrite (void *,size_t_t292,size_t_t292,FILE_t298 *);
extern int fgetpos (FILE_t298 *,fpos_t_t293 *);
extern int fseek (FILE_t298 *,long,int);
extern int fsetpos (FILE_t298 *,fpos_t_t293 *);
extern long ftell (FILE_t298 *);
extern void rewind (FILE_t298 *);
extern void clearerr (FILE_t298 *);
extern int feof (FILE_t298 *);
extern int ferror (FILE_t298 *);
extern void perror (char *);
extern int __filbuf (FILE_t298 *);
extern int __flsbuf (int,FILE_t298 *);
extern FILE_t298 * fdopen (int,char *);
extern int fileno (FILE_t298 *);
extern void flockfile (FILE_t298 *);
extern int ftrylockfile (FILE_t298 *);
extern void funlockfile (FILE_t298 *);
extern int getc_unlocked (FILE_t298 *);
extern int putc_unlocked (int,FILE_t298 *);
extern int getchar_unlocked (void);
extern int putchar_unlocked (int);
extern FILE_t298 * popen (char *,char *);
extern int pclose (FILE_t298 *);
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
extern int getw (FILE_t298 *);
extern int putw (int,FILE_t298 *);
extern char * mktemp (char *);
extern int mkstemp (char *);
extern int setbuffer (FILE_t298 *,char *,int);
extern int setlinebuf (FILE_t298 *);
extern int system (char *);
extern int fgetpos64 (FILE_t298 *,fpos64_t_t295 *);
extern FILE_t298 * fopen64 (char *,char *);
extern FILE_t298 * freopen64 (char *,char *,FILE_t298 *);
extern int fseek64 (FILE_t298 *,off64_t_t294,int);
extern int fseeko64 (FILE_t298 *,off64_t_t294,int);
extern int fseeko (FILE_t298 *,__int64_t_t286,int);
extern int fsetpos64 (FILE_t298 *,fpos64_t_t295 *);
extern off64_t_t294 ftell64 (FILE_t298 *);
extern __int64_t_t286 ftello (FILE_t298 *);
extern off64_t_t294 ftello64 (FILE_t298 *);
extern FILE_t298 * tmpfile64 (void);
extern int __semputc (int,FILE_t298 *);
extern int __semgetc (FILE_t298 *);
extern int __us_rsthread_stdio;
extern char * ctermid_r (char *);
int main ()
{
  int c_p1771;
  int i_p1772;
  char a_p1773[1000];
  int quesCol_p1777;
  int post_p1781;
  i_p1772 = 0;
  goto whileCont_p1775;
  
whileTop_p1774: 
  ;
  if (((c_p1771>=97)&&(c_p1771<=122))||((c_p1771>=65)&&(c_p1771<=90))) 
    {
      
      post_p1781 = i_p1772;
      i_p1772 = (i_p1772+1);
      a_p1773[post_p1781] = ((char) c_p1771);
    }
  else
    if (((c_p1771==10)||(c_p1771==32))||(c_p1771==9)) 
      {
        
        if (i_p1772>1) 
          {
            
            a_p1773[i_p1772] = ((char) 0);
            printf ((char *) "%s\n",(char *) a_p1773);
          }
        i_p1772 = 0;
      }
    else
      {
        
        i_p1772 = 0;
      }
  if (i_p1772>999) 
    {
      
      i_p1772 = 0;
    }
  
whileCont_p1775: 
  ;
  if (__us_rsthread_stdio) 
    quesCol_p1777 = __semgetc ((FILE_t298 *) (&__iob[0]));
  else
    {
      int quesCol_p1778;
      int pref_p1779;
      (*(&__iob[0]))._cnt = (((*(&__iob[0]))._cnt)-1);
      pref_p1779 = ((*(&__iob[0]))._cnt);
      if (pref_p1779<0) 
        quesCol_p1778 = __filbuf ((FILE_t298 *) (&__iob[0]));
      else
        {
          char *post_p1780;
          post_p1780 = ((*(&__iob[0]))._ptr);
          (*(&__iob[0]))._ptr = ((char *) (((int) ((*(&__iob[0]))._ptr))+1));
          quesCol_p1778 = ((int) (*post_p1780));
        }
      quesCol_p1777 = quesCol_p1778;
    }
  c_p1771 = quesCol_p1777;
  if (c_p1771!=(-1)) 
    goto whileTop_p1774;
}
