 









 

















 





































 
















 
 

 
 
 

 






 

















 






























 








 
















 
 
 
 

 
 
 

 












 
 
 


 







 
 
 
 

 







 
 

 



 
 
 
 
 

 









 
 

 















 
 

 
 

 








 





 

















 















 






 









 






		 


 









 



































 













 





typedef int __int32_t;
typedef unsigned  __uint32_t;



 



































	 
typedef long long __int64_t;
typedef unsigned long long  __uint64_t;






typedef __int32_t __psint_t;
typedef __uint32_t __psunsigned_t;




 





 
typedef __int32_t __scint_t;
typedef __uint32_t __scunsigned_t;













typedef unsigned int 	size_t;





typedef long		fpos_t;




 
typedef	__int64_t	off64_t;	 




 
typedef	__int64_t	fpos64_t;





 


typedef char *va_list;



























































typedef struct	 

 







__file_s

{

	int		_cnt;	 


	unsigned char	*_ptr;	 
	unsigned char	*_base;	 

	unsigned char	_flag;	 
	unsigned char	_file;	 

} FILE;

extern FILE		__iob[100 ];	
extern FILE		*_lastbuf;
extern unsigned char 	*_bufendtab[];
extern unsigned char	 _sibuf[], _sobuf[];

extern int	remove(const char *);
extern int	rename(const char *, const char *);
extern FILE	*tmpfile(void);
extern char	*tmpnam(char *);
extern int	fclose(FILE *);
extern int	fflush(FILE *);
extern FILE	*fopen(const char *, const char *);
extern FILE	*freopen(const char *, const char *, FILE *);
extern void	setbuf(FILE *, char *);
extern int	setvbuf(FILE *, char *, int, size_t);
 
extern int	fprintf(FILE *, const char *, ...);
 
extern int	fscanf(FILE *, const char *, ...);
 
extern int	printf(const char *, ...);
 
extern int	scanf(const char *, ...);
 
extern int	sprintf(char *, const char *, ...);
 
extern int	sscanf(const char *, const char *, ...);
extern int	vfprintf(FILE *, const char *,   char *);
extern int	vprintf(const char *,   char *);
extern int	vsprintf(char *, const char *,   char *);



extern int	fgetc(FILE *);
extern char	*fgets(char *, int, FILE *); 
extern int	fputc(int, FILE *);
extern int	fputs(const char *, FILE *);
extern int	getc(FILE *);
extern int	getchar(void);
extern char	*gets(char *);
extern int	putc(int, FILE *);
extern int	putchar(int);
extern int	puts(const char *);
extern int	ungetc(int, FILE *);
extern size_t	fread(void *, size_t, size_t, FILE *);
extern size_t	fwrite(const void *, size_t, size_t, FILE *);
extern int	fgetpos(FILE *, fpos_t *);
extern int	fseek(FILE *, long, int);
extern int	fsetpos(FILE *, const fpos_t *);
extern long	ftell(FILE *);
extern void	rewind(FILE *);
extern void	clearerr(FILE *);
extern int	feof(FILE *);
extern int	ferror(FILE *);
extern void	perror(const char *);

extern int	__filbuf(FILE *);	
extern int	__flsbuf(int, FILE *);	


	 
extern FILE    *fdopen(int, const char *);
extern int	fileno(FILE *);



	 
extern void	flockfile(FILE *);
extern int	ftrylockfile(FILE *);
extern void	funlockfile(FILE *);
extern int	getc_unlocked(FILE *);
extern int	putc_unlocked(int, FILE *);
extern int	getchar_unlocked(void);
extern int	putchar_unlocked(int);



extern FILE	*popen(const char *, const char *);
extern int	pclose(FILE *);


	 





 




















extern int	getopt(int, char *const *, const char *);

extern char	*optarg;
extern int	opterr;
extern int	optind;
extern int	optopt;




 





extern int	getsubopt(char **, char *const *, char **);
extern void	getoptreset(void);





extern char	*ctermid(char *);
extern char	*cuserid(char *);
extern char	*tempnam(const char *, const char *);
extern int	getw(FILE *);
extern int	putw(int, FILE *);




	 
extern char	*mktemp(char *);
extern int	mkstemp(char *);
extern int	setbuffer(FILE *, char *, int);
extern int	setlinebuf(FILE *);
extern int	system(const char *);



	 
extern int	fgetpos64(FILE *, fpos64_t *);
extern FILE	*fopen64(const char *, const char *);
extern FILE	*freopen64(const char *, const char *, FILE *);
extern int	fseek64(FILE *, off64_t, int);
extern int	fseeko64(FILE *, off64_t, int);
 
extern int	fseeko(FILE *, __int64_t, int);
extern int	fsetpos64(FILE *, const fpos64_t *);
extern off64_t	ftell64(FILE *);
 
extern __int64_t ftello(FILE *);
extern off64_t	ftello64(FILE *);
extern FILE	*tmpfile64(void);



extern int	__semputc(int, FILE *);
extern int	__semgetc(FILE *);
extern int	__us_rsthread_stdio;



extern char	*ctermid_r(char *);





 










 





 




 

 

 
 







 


 


















 




 



 















 
 

 
 
 







 












 
typedef unsigned char   uchar_t;
typedef unsigned short  ushort_t;
typedef unsigned int    uint_t;
typedef unsigned long   ulong_t;

 
typedef	char *		addr_t;		 
typedef	char *		caddr_t;	 

typedef	long		daddr_t;	 

typedef	long		pgno_t;		 
typedef	__uint32_t	pfn_t;		 
typedef	short		cnt_t;		 
typedef unsigned long	basictime_t;
typedef __int64_t	micro_t;
 



typedef __int32_t	pgcnt_t;	 

typedef enum { B_FALSE, B_TRUE } boolean_t;


 








typedef long			id_t;	 
					 
					 
					 
					 



 


typedef ulong_t	major_t;	 
typedef ulong_t	minor_t;	 



 










typedef	ushort_t o_mode_t;		 
typedef short	o_dev_t;		 
typedef	ushort_t o_uid_t;		 
typedef	o_uid_t	o_gid_t;		 
typedef	short	o_nlink_t;		 
typedef short	o_pid_t;		 
typedef __uint32_t o_ino_t;		 


typedef	unsigned long	mode_t;		 
typedef	unsigned long	dev_t;		 
typedef	long		uid_t;
typedef	long		gid_t;
typedef unsigned long	nlink_t;	 
typedef long		pid_t;		 



typedef dev_t	vertex_hdl_t;		 


typedef	unsigned long	ino_t;		 

typedef __uint64_t	ino64_t;	 


typedef long		off_t;		 




typedef __scint_t	__scoff_t;	 

typedef __scoff_t	scoff_t;	 



	 
typedef	__int64_t	blkcnt64_t;
typedef	__uint64_t	fsblkcnt64_t;
typedef	__uint64_t	fsfilcnt64_t;



typedef	long		blkcnt_t;	 
typedef	ulong_t		fsblkcnt_t;	 
typedef	ulong_t		fsfilcnt_t;	 


typedef	long		swblk_t;
typedef	unsigned long	paddr_t;	 
typedef	unsigned long	iopaddr_t;	 
typedef	int		key_t;		 
typedef	unsigned char	use_t;		 
typedef	long		sysid_t;
typedef	short		index_t;

typedef signed short	nasid_t;         
typedef signed short	cnodeid_t;	 
typedef	signed char  	partid_t;	 
typedef signed short 	moduleid_t;	 

typedef unsigned int 	lock_t;		 
typedef	signed short	cpuid_t;	 
typedef	unsigned char	pri_t;		 
typedef __uint64_t	accum_t;	 
typedef __int64_t	prid_t;		 
typedef __int64_t	ash_t;		 
typedef int		cell_t;	 






typedef int    ssize_t;







typedef	long		time_t;		 







typedef	long		clock_t;  







typedef long wchar_t;






typedef int             clockid_t;



typedef int		timer_t;



 




typedef	unsigned int	useconds_t;


 



typedef __scunsigned_t	bitnum_t;	 
typedef __scunsigned_t	bitlen_t;	 


typedef int processorid_t;		 
typedef int toid_t;			 
typedef	long *qaddr_t;		       
typedef __uint32_t inst_t;		 

 


typedef unsigned machreg_t;




 


typedef __uint32_t fpreg_t;




 







typedef signed char             int8_t;
typedef unsigned char           uint8_t;
typedef signed short            int16_t;
typedef unsigned short          uint16_t;
typedef signed int              int32_t;
typedef unsigned int            uint32_t;
typedef __int64_t 		int64_t;
typedef __uint64_t		uint64_t;
typedef __int64_t 		intmax_t;
typedef __uint64_t		uintmax_t;
typedef signed long int         intptr_t;
typedef unsigned long int       uintptr_t;


 



typedef	unsigned char	u_int8_t;
typedef	unsigned short	u_int16_t;
typedef	__uint32_t	u_int32_t;


 













 



typedef	long	hostid_t;

 








 



 





















 




typedef	struct { int r[1]; } *	physadr;
typedef	unsigned char	unchar;
typedef	unsigned char	u_char;
typedef	unsigned short	ushort;
typedef	unsigned short	u_short;
typedef	unsigned int	uint;
typedef	unsigned int	u_int;
typedef	unsigned long	ulong;
typedef	unsigned long	u_long;
typedef	struct	_quad { long val[2]; } quad;



 















 
 
 

 
 
 
 








 






 










typedef	long	fd_mask_t;
typedef	unsigned long	ufd_mask_t;






typedef	struct fd_set {
	fd_mask_t	fds_bits[(((1024)+(( (int)(sizeof(fd_mask_t) * 8))-1))/( (int)(sizeof(fd_mask_t) * 8))) ];
} fd_set;









 
















 
 
 

 
 
 
 

 










 


extern void *memcpy(void *, const void *, size_t);
extern void *memmove(void *, const void *, size_t);
extern char *strcpy(char *, const char *);
extern char *strncpy(char *, const char *, size_t);
extern char *strcat(char *, const char *);
extern char *strncat(char *, const char *, size_t);
extern void *memccpy(void *, const void *, int, size_t);
extern int memcmp(const void *, const void *, size_t);
extern int strcmp(const char *, const char *);
extern int strcoll(const char *, const char *);
extern int strncmp(const char *, const char *, size_t);
extern size_t strxfrm(char *, const char *, size_t);
extern void *memchr(const void *, int, size_t);
extern char *strchr(const char *, int);
extern size_t strcspn(const char *, const char *);
extern char *strpbrk(const char *, const char *);
extern char *strrchr(const char *, int);
extern size_t strspn(const char *, const char *);
extern char *strstr(const char *, const char *);
extern char *strtok(char *, const char *);
extern void *memset(void *, int, size_t);
extern char *strerror(int);
extern size_t strlen(const char *);


extern int ffs(int);
 
extern int strcasecmp(const char *, const char *);
extern int strncasecmp(const char *, const char *, size_t);



extern char *strdup(const char *);



extern char *strtok_r(char *, const char *, char **);



 















 













typedef	long	fd_mask;













typedef struct {                 
        __uint32_t sigbits[2];
} k_sigset_t;









 




extern int	bcmp(const void *, const void *, size_t);
extern void	bcopy(const void *, void *, size_t);
extern void	bzero(void *, size_t);
extern char	*index(const char *, int);
extern char	*rindex(const char *, int);









 
















 
 

 
 
 

 
















extern int isalnum(int);        
extern int isalpha(int);        
extern int iscntrl(int);        
extern int isdigit(int);        
extern int isgraph(int);        
extern int islower(int);        
extern int isprint(int);        
extern int ispunct(int);        
extern int isspace(int);        
extern int isupper(int);        
extern int isxdigit(int);       
extern int tolower(int);
extern int toupper(int);

 
extern int isascii(int);        
extern int toascii(int);        

extern int _tolower(int);
extern int _toupper(int);


extern unsigned char	__ctype[];






























 
















 
 

 
 
 

 



 














 



 





 






















typedef	struct {
	 int	quot;
	 int	rem;
	} div_t;

typedef struct {
	 long	quot;
	 long	rem;
	} ldiv_t;


typedef struct {
	 long long	quot;
	 long long	rem;
	} lldiv_t;










extern unsigned char 	__ctype[];	



 
extern double atof(const char *);
extern int atoi(const char *);
extern long int atol(const char *);
extern double strtod(const char *, char **);
extern long int strtol(const char *, char **, int);
extern unsigned long int strtoul(const char *, char **, int);
extern int rand(void);
extern void srand(unsigned int);
extern void *calloc(size_t, size_t);
extern void free(void *);
extern void *malloc(size_t);
extern void *realloc(void *, size_t);

extern void abort(void);
extern int atexit(void (*)(void));
extern void exit(int);

extern char *getenv(const char *);
extern int system(const char *);
extern void *bsearch(const void *, const void *, size_t, size_t,
	int (*)(const void *, const void *));
extern void qsort(void *, size_t, size_t,
	int (*)(const void *, const void *));


extern int abs(int);



extern div_t div(int, int);
extern long int labs(long);

extern ldiv_t ldiv(long, long);
extern int mbtowc(wchar_t *, const char *, size_t);
extern int mblen(const char *, size_t);
extern int wctomb(char *, wchar_t);
extern size_t mbstowcs(wchar_t *, const char *, size_t);
extern size_t wcstombs(char *, const wchar_t *, size_t);


	 
extern int putenv(const char *);
extern double	drand48(void);
extern double	erand48(unsigned short [3]);
extern long	lrand48(void);
extern long	nrand48(unsigned short [3]);
extern long	mrand48(void);
extern long	jrand48(unsigned short [3]);
extern void	srand48(long);
extern void	lcong48(unsigned short int [7]);
extern void     setkey(const char *);
extern unsigned short * seed48(unsigned short int [3]);



	 
extern long a64l(const char *);
extern char *ecvt(double, int, int *, int *);
extern char *fcvt(double, int, int *, int *);
extern char *gcvt(double, int, char *);
extern int getsubopt(char **, char * const *, char **);
extern int grantpt(int);
extern char *initstate(unsigned int, char *, size_t);
extern char *l64a(long);
extern char *mktemp(char *);
extern int mkstemp(char *);
extern char *ptsname(int);
extern long random(void);
extern char *realpath(const char *, char *);
extern char *setstate(const char *);
extern void srandom(unsigned);
extern int ttyslot(void);
extern int unlockpt(int);
extern void *valloc(size_t);




extern int rand_r(unsigned int *);



	 

extern int atcheckpoint(void (*)(void));
extern int atrestart(void (*)(void));
extern int getpw(int, char *);
extern void l3tol(long *, const char *, int);
extern void ltol3(char *, const long *, int);
extern void *memalign(size_t, size_t);


	 




extern int dup2(int, int);
extern char *getcwd(char *, size_t);
extern char *getlogin(void);
extern char *getpass(const char *);
extern int isatty(int);
extern void swab(const void *, void *, ssize_t);
extern char *ttyname(int);
extern long long int atoll(const char *);
extern long long int strtoll(const char *, char **, int);
extern unsigned long long int strtoull(const char *, char **, int);
extern long long llabs(long long);

extern lldiv_t lldiv(long long, long long);



extern char *ecvt_r(double, int, int *, int *, char *);
extern char *fcvt_r(double, int, int *, int *, char *);











void dviparse(FILE *);   
void psparse();

void main(argc, argv)
int argc; 
char *argv[];
{
  int i,                   
      known_flag,          
      dvi_file = 0 ;    
  FILE *file, *source;     

  source = (&__iob[0]) ;                                     
  for(i=1; i<argc; i++)                            
    {
      known_flag = 0 ;
      if (strcmp(argv[i],"-dvi") == 0)     
	{ 
	  dvi_file = 1 ; 
	  known_flag = 1 ;
	}
      if (strcmp(argv[i],"-") == 0)     
	{ 
	  source = (&__iob[0]) ; 
	  known_flag = 1 ; 
	}
      if (!known_flag)                         
	{
	  if ((file=fopen(argv[i],"r")) != 0L  )
	    source=file;
	  else 
	    {
	      fprintf((&__iob[2]) ,"ps2txt: error opening file %s\n",argv[i]);
	      fprintf((&__iob[2]) ,"usage:  ps2txt [-dvi] [-] [input_file.ps]\n");
	      exit(1);
	    }
	}
    }

dviparse(source);   
    
}

void dviparse(source)
FILE *source;
{
  int ch,                  
      prev_ch = '\n',      
      next_to_prev_ch = '\n',      
      in_paren = 0 ,    
      b_flag = 0 ,      
      b_space = 1 ,      
      c,                   
      word_over_line = 0 ;  
  char junk[80];           

  while ((ch = fgetc(source)) != (-1) )
    {
      if (ch == '\n') ch = fgetc(source);       
      if (in_paren)               
	switch(ch)
	  {
	  case ')'  : in_paren--; b_flag=1; break;  
	  case '\n' : (__us_rsthread_stdio ? __semputc((' '), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((' ')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((' ')))))  ; ; break;               
	  case '-' : if((c = fgetc(source)) == ')') {   
	               word_over_line=1 ;
		     }
	             else {
		       (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; ;
		     }
	             ungetc(c, source);
	             break;
	    
	    (__us_rsthread_stdio ? __semputc((' '), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((' ')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((' ')))))  ; ; break;               
	  case '\\' : 
	    switch(ch=fgetc(source))
	      {
	      case '(' :
	      case ')' : (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; ; break;                          
	      case 't' : (__us_rsthread_stdio ? __semputc(('\t'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\t')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\t')))))  ; ; break;                    
	      case '\n' : break;
	      case 'n' :   break;              
	      case '\\': (__us_rsthread_stdio ? __semputc(('"'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('"')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('"')))))  ; ; break;                     
	      case '0' : switch(ch=fgetc(source))
		{
		case '1': switch(ch=fgetc(source))
		  {
		  case '3' : fputs("ff",(&__iob[1]) ); break;          
		  case '4' : fputs("fi",(&__iob[1]) ); break;
		  case '5' : fputs("fl",(&__iob[1]) ); break;
		  case '6' : fputs("ffi",(&__iob[1]) ); break;
		  case '7' : fputs("ffl",(&__iob[1]) ); break;
		  default: fputs("\\01",(&__iob[1]) ); (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; ;    
		  } break;                                        
		default: fputs("\\0",(&__iob[1]) ); (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; ;       
		} break;
	      case '1' : case '2' : case '3' : case '4' :
	      case '5' : case '6' : case '7' : (__us_rsthread_stdio ? __semputc(('\\'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\\')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\\')))))  ; ;    
	      default: (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; ;
	      } break;                                
	  default: (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; ;
	  }
      else                                                  
	switch(ch)
	  {
	  case '%'  : fgets(junk, 80, source); break;    
	  case '\n' : break;                 
	  case '-'  : if (b_flag) 
	    {
	      b_flag = 0;                    
	      b_space = 0;     
                               
                               
	    } break;
	  case '('  : in_paren++;                     
	    if(!word_over_line) {
	      if(!((__ctype + 1)[next_to_prev_ch] & (01  | 02 )) ) {
		switch(prev_ch)   
		  {
		  case 'l' : case 'm' : case 'n' : case 'o' :  
		  case 'q' : case 'r' : case 's' : case 't' : 
		    break;
		  case 'y' : (__us_rsthread_stdio ? __semputc(('\n'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\n')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\n')))))  ; ; break;                
		  case 'b' : if (b_space) (__us_rsthread_stdio ? __semputc((' '), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((' ')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((' ')))))  ; ; break;  
		  case 'a' : case 'c' : case 'd' : case 'e' : 
		  case 'f' : case 'g' : case 'h' : case 'i' : 
		  case 'j' : case 'k' : case 'x' : (__us_rsthread_stdio ? __semputc((' '), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((' ')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((' ')))))  ; ; break;
		  default: break;
		  } 
	      }
	      else {
		(__us_rsthread_stdio ? __semputc((' '), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((' ')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((' ')))))  ; ;
	      }
	    }
	    b_space = 1;               
	    word_over_line = 0 ;
	    break;
	  default: b_flag = 0; break;             
	  }
      next_to_prev_ch=prev_ch;   
      prev_ch=ch;   

    }
}

void psparse(source)      
FILE *source;             
{
char *str;
char junk[80];
int ch, para=0, last=0;
while ((ch=fgetc(source)) != (-1) )
  {
    switch (ch)
      {
      case '%'  : if (para==0) fgets(junk, 80, source);
      else (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ;
      case '\n' : if (last==1) { puts(""); last=0; } break;
      case '('  : if (para++>0) (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; break;
      case ')'  : if (para-->1) (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; 
      else (__us_rsthread_stdio ? __semputc((' '), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((' ')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((' ')))))  ;
	last=1; break;
	
      case '\\' : if (para>0)
	switch(ch=fgetc(source))
	  {
	  case '(' :
	  case ')' :  (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; break;
	  case 't' :  (__us_rsthread_stdio ? __semputc(('\t'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\t')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\t')))))  ; break;
	  case 'n' :  (__us_rsthread_stdio ? __semputc(('\n'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\n')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\n')))))  ; break;
	  case '\\':  (__us_rsthread_stdio ? __semputc(('\\'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\\')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\\')))))  ; break;
	  case '0' :  case '1' : case '2' : case '3' :
	  case '4' :  case '5' : case '6' : case '7' :
	    (__us_rsthread_stdio ? __semputc(('\\'), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf((('\\')), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)(('\\')))))  ;
	  default:  (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ; break;
	  }
	break;
      default:	if (para>0) (__us_rsthread_stdio ? __semputc((ch), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((ch)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((ch)))))  ;
      }
  }
}
