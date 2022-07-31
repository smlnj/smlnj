
typedef int __int32_t_t101;
typedef unsigned int __uint32_t_t102;
typedef long long __int64_t_t103;
typedef unsigned long long __uint64_t_t104;
typedef __int32_t_t101 __psint_t_t105;
typedef __uint32_t_t102 __psunsigned_t_t106;
typedef __int32_t_t101 __scint_t_t107;
typedef __uint32_t_t102 __scunsigned_t_t108;
typedef unsigned int size_t_t109;
typedef long fpos_t_t110;
typedef __int64_t_t103 off64_t_t111;
typedef __int64_t_t103 fpos64_t_t112;
typedef char *va_list_t113;
struct __file_s_t114 {
  int _cnt;
  char *_ptr;
  char *_base;
  char _flag;
  char _file;
};
typedef struct __file_s_t114 FILE_t115;
extern FILE_t115 __iob[100];
extern FILE_t115 *_lastbuf;
extern char *_bufendtab[];
extern char _sibuf[];
extern char _sobuf[];
extern int remove (char *);
extern int rename (char *,char *);
extern FILE_t115 * tmpfile (void);
extern char * tmpnam (char *);
extern int fclose (FILE_t115 *);
extern int fflush (FILE_t115 *);
extern FILE_t115 * fopen (char *,char *);
extern FILE_t115 * freopen (char *,char *,FILE_t115 *);
extern void setbuf (FILE_t115 *,char *);
extern int setvbuf (FILE_t115 *,char *,int,size_t_t109);
extern int fprintf (FILE_t115 *,char *,...);
extern int fscanf (FILE_t115 *,char *,...);
extern int printf (char *,...);
extern int scanf (char *,...);
extern int sprintf (char *,char *,...);
extern int sscanf (char *,char *,...);
extern int vfprintf (FILE_t115 *,char *,char *);
extern int vprintf (char *,char *);
extern int vsprintf (char *,char *,char *);
extern int fgetc (FILE_t115 *);
extern char * fgets (char *,int,FILE_t115 *);
extern int fputc (int,FILE_t115 *);
extern int fputs (char *,FILE_t115 *);
extern int getc (FILE_t115 *);
extern int getchar (void);
extern char * gets (char *);
extern int putc (int,FILE_t115 *);
extern int putchar (int);
extern int puts (char *);
extern int ungetc (int,FILE_t115 *);
extern size_t_t109 fread (void *,size_t_t109,size_t_t109,FILE_t115 *);
extern size_t_t109 fwrite (void *,size_t_t109,size_t_t109,FILE_t115 *);
extern int fgetpos (FILE_t115 *,fpos_t_t110 *);
extern int fseek (FILE_t115 *,long,int);
extern int fsetpos (FILE_t115 *,fpos_t_t110 *);
extern long ftell (FILE_t115 *);
extern void rewind (FILE_t115 *);
extern void clearerr (FILE_t115 *);
extern int feof (FILE_t115 *);
extern int ferror (FILE_t115 *);
extern void perror (char *);
extern int __filbuf (FILE_t115 *);
extern int __flsbuf (int,FILE_t115 *);
extern FILE_t115 * fdopen (int,char *);
extern int fileno (FILE_t115 *);
extern void flockfile (FILE_t115 *);
extern int ftrylockfile (FILE_t115 *);
extern void funlockfile (FILE_t115 *);
extern int getc_unlocked (FILE_t115 *);
extern int putc_unlocked (int,FILE_t115 *);
extern int getchar_unlocked (void);
extern int putchar_unlocked (int);
extern FILE_t115 * popen (char *,char *);
extern int pclose (FILE_t115 *);
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
extern int getw (FILE_t115 *);
extern int putw (int,FILE_t115 *);
extern char * mktemp (char *);
extern int mkstemp (char *);
extern int setbuffer (FILE_t115 *,char *,int);
extern int setlinebuf (FILE_t115 *);
extern int system (char *);
extern int fgetpos64 (FILE_t115 *,fpos64_t_t112 *);
extern FILE_t115 * fopen64 (char *,char *);
extern FILE_t115 * freopen64 (char *,char *,FILE_t115 *);
extern int fseek64 (FILE_t115 *,off64_t_t111,int);
extern int fseeko64 (FILE_t115 *,off64_t_t111,int);
extern int fseeko (FILE_t115 *,__int64_t_t103,int);
extern int fsetpos64 (FILE_t115 *,fpos64_t_t112 *);
extern off64_t_t111 ftell64 (FILE_t115 *);
extern __int64_t_t103 ftello (FILE_t115 *);
extern off64_t_t111 ftello64 (FILE_t115 *);
extern FILE_t115 * tmpfile64 (void);
extern int __semputc (int,FILE_t115 *);
extern int __semgetc (FILE_t115 *);
extern int __us_rsthread_stdio;
extern char * ctermid_r (char *);
int is_prime (int);
int main ()
{
  int i_p876;
  int post_p884;
  i_p876 = 2;
  goto forStart_p881;
  
forTop_p880: 
  ;
  if (is_prime (i_p876)) 
    printf ((char *) "%d\n",i_p876);
  post_p884 = i_p876;
  i_p876 = (i_p876+1);
  
forStart_p881: 
  ;
  if (1) 
    goto forTop_p880;
}
int is_prime (int i_p878)
{
  int j_p879;
  int post_p889;
  j_p879 = 2;
  goto forStart_p886;
  
forTop_p885: 
  ;
  if ((i_p878%j_p879)==0) 
    return 0;
  post_p889 = j_p879;
  j_p879 = (j_p879+1);
  
forStart_p886: 
  ;
  if ((j_p879*j_p879)<=i_p878) 
    goto forTop_p885;
  return 1;
}
