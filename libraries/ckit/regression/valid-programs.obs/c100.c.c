



 
















 
 

 
 
 

 






 

















 






























 








 
















 
 
 
 

 
 
 

 












 
 
 


 







 
 
 
 

 







 
 

 



 
 
 
 
 

 









 
 

 















 
 

 
 

 








 





 

















 















 






 









 






		 


 









 



































 













 





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





 










 





 




 

 

 
 







 


 
















 

main()
{
	int c, i;

	i = 0;
	while((c = (__us_rsthread_stdio ? __semgetc((&__iob[0])) : 			(--((&__iob[0]))->_cnt < 0 ? __filbuf((&__iob[0])) : (int)*((&__iob[0]))->_ptr++))  ) != (-1) ) {
		if (++i > 100) break;
		(__us_rsthread_stdio ? __semputc((c), (&__iob[1])) : 			(--( (&__iob[1]))->_cnt < 0 ? __flsbuf(((c)), ( (&__iob[1]))) 					  : (int)(*( (&__iob[1]))->_ptr++ = (unsigned char)((c)))))  ;
		}
}
