
typedef int __int32_t_t116;
typedef unsigned int __uint32_t_t117;
typedef long long __int64_t_t118;
typedef unsigned long long __uint64_t_t119;
typedef __int32_t_t116 __psint_t_t120;
typedef __uint32_t_t117 __psunsigned_t_t121;
typedef __int32_t_t116 __scint_t_t122;
typedef __uint32_t_t117 __scunsigned_t_t123;
typedef unsigned int size_t_t124;
typedef long fpos_t_t125;
typedef __int64_t_t118 off64_t_t126;
typedef __int64_t_t118 fpos64_t_t127;
typedef char *va_list_t128;
struct __file_s_t129 {
  int _cnt;
  char *_ptr;
  char *_base;
  char _flag;
  char _file;
};
typedef struct __file_s_t129 FILE_t130;
extern FILE_t130 __iob[100];
extern FILE_t130 *_lastbuf;
extern char *_bufendtab[];
extern char _sibuf[];
extern char _sobuf[];
extern int remove (char *);
extern int rename (char *,char *);
extern FILE_t130 * tmpfile (void);
extern char * tmpnam (char *);
extern int fclose (FILE_t130 *);
extern int fflush (FILE_t130 *);
extern FILE_t130 * fopen (char *,char *);
extern FILE_t130 * freopen (char *,char *,FILE_t130 *);
extern void setbuf (FILE_t130 *,char *);
extern int setvbuf (FILE_t130 *,char *,int,size_t_t124);
extern int fprintf (FILE_t130 *,char *,...);
extern int fscanf (FILE_t130 *,char *,...);
extern int printf (char *,...);
extern int scanf (char *,...);
extern int sprintf (char *,char *,...);
extern int sscanf (char *,char *,...);
extern int vfprintf (FILE_t130 *,char *,char *);
extern int vprintf (char *,char *);
extern int vsprintf (char *,char *,char *);
extern int fgetc (FILE_t130 *);
extern char * fgets (char *,int,FILE_t130 *);
extern int fputc (int,FILE_t130 *);
extern int fputs (char *,FILE_t130 *);
extern int getc (FILE_t130 *);
extern int getchar (void);
extern char * gets (char *);
extern int putc (int,FILE_t130 *);
extern int putchar (int);
extern int puts (char *);
extern int ungetc (int,FILE_t130 *);
extern size_t_t124 fread (void *,size_t_t124,size_t_t124,FILE_t130 *);
extern size_t_t124 fwrite (void *,size_t_t124,size_t_t124,FILE_t130 *);
extern int fgetpos (FILE_t130 *,fpos_t_t125 *);
extern int fseek (FILE_t130 *,long,int);
extern int fsetpos (FILE_t130 *,fpos_t_t125 *);
extern long ftell (FILE_t130 *);
extern void rewind (FILE_t130 *);
extern void clearerr (FILE_t130 *);
extern int feof (FILE_t130 *);
extern int ferror (FILE_t130 *);
extern void perror (char *);
extern int __filbuf (FILE_t130 *);
extern int __flsbuf (int,FILE_t130 *);
extern FILE_t130 * fdopen (int,char *);
extern int fileno (FILE_t130 *);
extern void flockfile (FILE_t130 *);
extern int ftrylockfile (FILE_t130 *);
extern void funlockfile (FILE_t130 *);
extern int getc_unlocked (FILE_t130 *);
extern int putc_unlocked (int,FILE_t130 *);
extern int getchar_unlocked (void);
extern int putchar_unlocked (int);
extern FILE_t130 * popen (char *,char *);
extern int pclose (FILE_t130 *);
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
extern int getw (FILE_t130 *);
extern int putw (int,FILE_t130 *);
extern char * mktemp (char *);
extern int mkstemp (char *);
extern int setbuffer (FILE_t130 *,char *,int);
extern int setlinebuf (FILE_t130 *);
extern int system (char *);
extern int fgetpos64 (FILE_t130 *,fpos64_t_t127 *);
extern FILE_t130 * fopen64 (char *,char *);
extern FILE_t130 * freopen64 (char *,char *,FILE_t130 *);
extern int fseek64 (FILE_t130 *,off64_t_t126,int);
extern int fseeko64 (FILE_t130 *,off64_t_t126,int);
extern int fseeko (FILE_t130 *,__int64_t_t118,int);
extern int fsetpos64 (FILE_t130 *,fpos64_t_t127 *);
extern off64_t_t126 ftell64 (FILE_t130 *);
extern __int64_t_t118 ftello (FILE_t130 *);
extern off64_t_t126 ftello64 (FILE_t130 *);
extern FILE_t130 * tmpfile64 (void);
extern int __semputc (int,FILE_t130 *);
extern int __semgetc (FILE_t130 *);
extern int __us_rsthread_stdio;
extern char * ctermid_r (char *);
typedef char uchar_t_t131;
typedef unsigned short ushort_t_t132;
typedef unsigned int uint_t_t133;
typedef unsigned long ulong_t_t134;
typedef char *addr_t_t135;
typedef char *caddr_t_t136;
typedef long daddr_t_t137;
typedef long pgno_t_t138;
typedef __uint32_t_t117 pfn_t_t139;
typedef short cnt_t_t140;
typedef unsigned long basictime_t_t141;
typedef __int64_t_t118 micro_t_t142;
typedef __int32_t_t116 pgcnt_t_t143;
enum t144 {
  B_FALSE=0,
  B_TRUE=0
};
typedef enum t144 boolean_t_t145;
typedef long id_t_t146;
typedef ulong_t_t134 major_t_t147;
typedef ulong_t_t134 minor_t_t148;
typedef ushort_t_t132 o_mode_t_t149;
typedef short o_dev_t_t150;
typedef ushort_t_t132 o_uid_t_t151;
typedef o_uid_t_t151 o_gid_t_t152;
typedef short o_nlink_t_t153;
typedef short o_pid_t_t154;
typedef __uint32_t_t117 o_ino_t_t155;
typedef unsigned long mode_t_t156;
typedef unsigned long dev_t_t157;
typedef long uid_t_t158;
typedef long gid_t_t159;
typedef unsigned long nlink_t_t160;
typedef long pid_t_t161;
typedef dev_t_t157 vertex_hdl_t_t162;
typedef unsigned long ino_t_t163;
typedef __uint64_t_t119 ino64_t_t164;
typedef long off_t_t165;
typedef __scint_t_t122 __scoff_t_t166;
typedef __scoff_t_t166 scoff_t_t167;
typedef __int64_t_t118 blkcnt64_t_t168;
typedef __uint64_t_t119 fsblkcnt64_t_t169;
typedef __uint64_t_t119 fsfilcnt64_t_t170;
typedef long blkcnt_t_t171;
typedef ulong_t_t134 fsblkcnt_t_t172;
typedef ulong_t_t134 fsfilcnt_t_t173;
typedef long swblk_t_t174;
typedef unsigned long paddr_t_t175;
typedef unsigned long iopaddr_t_t176;
typedef int key_t_t177;
typedef char use_t_t178;
typedef long sysid_t_t179;
typedef short index_t_t180;
typedef short nasid_t_t181;
typedef short cnodeid_t_t182;
typedef signed char partid_t_t183;
typedef short moduleid_t_t184;
typedef unsigned int lock_t_t185;
typedef short cpuid_t_t186;
typedef char pri_t_t187;
typedef __uint64_t_t119 accum_t_t188;
typedef __int64_t_t118 prid_t_t189;
typedef __int64_t_t118 ash_t_t190;
typedef int cell_t_t191;
typedef int ssize_t_t192;
typedef long time_t_t193;
typedef long clock_t_t194;
typedef long wchar_t_t195;
typedef int clockid_t_t196;
typedef int timer_t_t197;
typedef unsigned int useconds_t_t198;
typedef __scunsigned_t_t123 bitnum_t_t199;
typedef __scunsigned_t_t123 bitlen_t_t200;
typedef int processorid_t_t201;
typedef int toid_t_t202;
typedef long *qaddr_t_t203;
typedef __uint32_t_t117 inst_t_t204;
typedef unsigned int machreg_t_t205;
typedef __uint32_t_t117 fpreg_t_t206;
typedef signed char int8_t_t207;
typedef char uint8_t_t208;
typedef short int16_t_t209;
typedef unsigned short uint16_t_t210;
typedef int int32_t_t211;
typedef unsigned int uint32_t_t212;
typedef __int64_t_t118 int64_t_t213;
typedef __uint64_t_t119 uint64_t_t214;
typedef __int64_t_t118 intmax_t_t215;
typedef __uint64_t_t119 uintmax_t_t216;
typedef long intptr_t_t217;
typedef unsigned long uintptr_t_t218;
typedef char u_int8_t_t219;
typedef unsigned short u_int16_t_t220;
typedef __uint32_t_t117 u_int32_t_t221;
typedef long hostid_t_t222;
struct t223 {
  int r[1];
};
typedef struct t223 *physadr_t224;
typedef char unchar_t225;
typedef char u_char_t226;
typedef unsigned short ushort_t227;
typedef unsigned short u_short_t228;
typedef unsigned int uint_t229;
typedef unsigned int u_int_t230;
typedef unsigned long ulong_t231;
typedef unsigned long u_long_t232;
struct _quad_t233 {
  long val[2];
};
typedef struct _quad_t233 quad_t234;
typedef long fd_mask_t_t235;
typedef unsigned long ufd_mask_t_t236;
struct fd_set_t237 {
  fd_mask_t_t235 fds_bits[32];
};
typedef struct fd_set_t237 fd_set_t238;
extern void * memcpy (void *,void *,size_t_t124);
extern void * memmove (void *,void *,size_t_t124);
extern char * strcpy (char *,char *);
extern char * strncpy (char *,char *,size_t_t124);
extern char * strcat (char *,char *);
extern char * strncat (char *,char *,size_t_t124);
extern void * memccpy (void *,void *,int,size_t_t124);
extern int memcmp (void *,void *,size_t_t124);
extern int strcmp (char *,char *);
extern int strcoll (char *,char *);
extern int strncmp (char *,char *,size_t_t124);
extern size_t_t124 strxfrm (char *,char *,size_t_t124);
extern void * memchr (void *,int,size_t_t124);
extern char * strchr (char *,int);
extern size_t_t124 strcspn (char *,char *);
extern char * strpbrk (char *,char *);
extern char * strrchr (char *,int);
extern size_t_t124 strspn (char *,char *);
extern char * strstr (char *,char *);
extern char * strtok (char *,char *);
extern void * memset (void *,int,size_t_t124);
extern char * strerror (int);
extern size_t_t124 strlen (char *);
extern int ffs (int);
extern int strcasecmp (char *,char *);
extern int strncasecmp (char *,char *,size_t_t124);
extern char * strdup (char *);
extern char * strtok_r (char *,char *,char **);
typedef long fd_mask_t239;
struct t240 {
  __uint32_t_t117 sigbits[2];
};
typedef struct t240 k_sigset_t_t241;
extern int bcmp (void *,void *,size_t_t124);
extern void bcopy (void *,void *,size_t_t124);
extern void bzero (void *,size_t_t124);
extern char * index (char *,int);
extern char * rindex (char *,int);
extern int isalnum (int);
extern int isalpha (int);
extern int iscntrl (int);
extern int isdigit (int);
extern int isgraph (int);
extern int islower (int);
extern int isprint (int);
extern int ispunct (int);
extern int isspace (int);
extern int isupper (int);
extern int isxdigit (int);
extern int tolower (int);
extern int toupper (int);
extern int isascii (int);
extern int toascii (int);
extern int _tolower (int);
extern int _toupper (int);
extern char __ctype[];
struct t242 {
  int quot;
  int rem;
};
typedef struct t242 div_t_t243;
struct t244 {
  long quot;
  long rem;
};
typedef struct t244 ldiv_t_t245;
struct t246 {
  long long quot;
  long long rem;
};
typedef struct t246 lldiv_t_t247;
extern char __ctype[];
extern double atof (char *);
extern int atoi (char *);
extern long atol (char *);
extern double strtod (char *,char **);
extern long strtol (char *,char **,int);
extern unsigned long strtoul (char *,char **,int);
extern int rand (void);
extern void srand (unsigned int);
extern void * calloc (size_t_t124,size_t_t124);
extern void free (void *);
extern void * malloc (size_t_t124);
extern void * realloc (void *,size_t_t124);
extern void abort (void);
extern int atexit (void (*) (void));
extern void exit (int);
extern char * getenv (char *);
extern int system (char *);
extern void * bsearch (void *,void *,size_t_t124,size_t_t124,int (*) (void *,void *));
extern void qsort (void *,size_t_t124,size_t_t124,int (*) (void *,void *));
extern int abs (int);
extern div_t_t243 div (int,int);
extern long labs (long);
extern ldiv_t_t245 ldiv (long,long);
extern int mbtowc (wchar_t_t195 *,char *,size_t_t124);
extern int mblen (char *,size_t_t124);
extern int wctomb (char *,wchar_t_t195);
extern size_t_t124 mbstowcs (wchar_t_t195 *,char *,size_t_t124);
extern size_t_t124 wcstombs (char *,wchar_t_t195 *,size_t_t124);
extern int putenv (char *);
extern double drand48 (void);
extern double erand48 (unsigned short *);
extern long lrand48 (void);
extern long nrand48 (unsigned short *);
extern long mrand48 (void);
extern long jrand48 (unsigned short *);
extern void srand48 (long);
extern void lcong48 (unsigned short *);
extern void setkey (char *);
extern unsigned short * seed48 (unsigned short *);
extern long a64l (char *);
extern char * ecvt (double,int,int *,int *);
extern char * fcvt (double,int,int *,int *);
extern char * gcvt (double,int,char *);
extern int getsubopt (char **,char **,char **);
extern int grantpt (int);
extern char * initstate (unsigned int,char *,size_t_t124);
extern char * l64a (long);
extern char * mktemp (char *);
extern int mkstemp (char *);
extern char * ptsname (int);
extern long random (void);
extern char * realpath (char *,char *);
extern char * setstate (char *);
extern void srandom (unsigned int);
extern int ttyslot (void);
extern int unlockpt (int);
extern void * valloc (size_t_t124);
extern int rand_r (unsigned int *);
extern int atcheckpoint (void (*) (void));
extern int atrestart (void (*) (void));
extern int getpw (int,char *);
extern void l3tol (long *,char *,int);
extern void ltol3 (char *,long *,int);
extern void * memalign (size_t_t124,size_t_t124);
extern int dup2 (int,int);
extern char * getcwd (char *,size_t_t124);
extern char * getlogin (void);
extern char * getpass (char *);
extern int isatty (int);
extern void swab (void *,void *,ssize_t_t192);
extern char * ttyname (int);
extern long long atoll (char *);
extern long long strtoll (char *,char **,int);
extern unsigned long long strtoull (char *,char **,int);
extern long long llabs (long long);
extern lldiv_t_t247 lldiv (long long,long long);
extern char * ecvt_r (double,int,int *,int *,char *);
extern char * fcvt_r (double,int,int *,int *,char *);
void dviparse (FILE_t130 *);
void psparse ();
void main (int argc_p1257,char *argv_p1258[])
{
  int i_p1259;
  int known_flag_p1260;
  int dvi_file_p1261;
  FILE_t130 *file_p1262;
  FILE_t130 *source_p1263;
  int post_p1286;
  int call_p1287;
  int call_p1288;
  FILE_t130 *call_p1289;
  dvi_file_p1261 = 0;
  source_p1263 = ((FILE_t130 *) (&__iob[0]));
  i_p1259 = 1;
  goto forStart_p1283;
  
forTop_p1282: 
  ;
  known_flag_p1260 = 0;
  call_p1287 = strcmp ((char *) argv_p1258[i_p1259],(char *) "-dvi");
  if (call_p1287==0) 
    {
      
      dvi_file_p1261 = 1;
      known_flag_p1260 = 1;
    }
  call_p1288 = strcmp ((char *) argv_p1258[i_p1259],(char *) "-");
  if (call_p1288==0) 
    {
      
      source_p1263 = ((FILE_t130 *) (&__iob[0]));
      known_flag_p1260 = 1;
    }
  if (!known_flag_p1260) 
    {
      
      call_p1289 = fopen ((char *) argv_p1258[i_p1259],(char *) "r");
      file_p1262 = call_p1289;
      if (file_p1262!=0) 
        source_p1263 = file_p1262;
      else
        {
          
          fprintf ((FILE_t130 *) (&__iob[2]),(char *) "ps2txt: error opening file %s\n",argv_p1258[i_p1259]);
          fprintf ((FILE_t130 *) (&__iob[2]),(char *) "usage:  ps2txt [-dvi] [-] [input_file.ps]\n");
          exit (1);
        }
    }
  post_p1286 = i_p1259;
  i_p1259 = (i_p1259+1);
  
forStart_p1283: 
  ;
  if (i_p1259<argc_p1257) 
    goto forTop_p1282;
  dviparse (source_p1263);
}
void dviparse (FILE_t130 *source_p1265)
{
  int ch_p1266;
  int prev_ch_p1267;
  int next_to_prev_ch_p1268;
  int in_paren_p1269;
  int b_flag_p1270;
  int b_space_p1271;
  int c_p1272;
  int word_over_line_p1273;
  char junk_p1274[80];
  int call_p1293;
  int call_p1294;
  int post_p1296;
  int quesCol_p1297;
  int call_p1301;
  int quesCol_p1302;
  int quesCol_p1306;
  int call_p1310;
  int quesCol_p1312;
  int quesCol_p1316;
  int quesCol_p1320;
  int call_p1324;
  int call_p1326;
  int quesCol_p1328;
  int quesCol_p1332;
  int quesCol_p1336;
  int quesCol_p1340;
  int quesCol_p1344;
  int post_p1349;
  int quesCol_p1351;
  int quesCol_p1355;
  int quesCol_p1359;
  int quesCol_p1363;
  prev_ch_p1267 = 10;
  next_to_prev_ch_p1268 = 10;
  in_paren_p1269 = 0;
  b_flag_p1270 = 0;
  b_space_p1271 = 1;
  word_over_line_p1273 = 0;
  goto whileCont_p1291;
  
whileTop_p1290: 
  ;
  if (ch_p1266==10) 
    {
      
      call_p1294 = fgetc (source_p1265);
      ch_p1266 = call_p1294;
    }
  if (in_paren_p1269) 
    {
      
      switch (ch_p1266)
        {
          
          
        case 41: 
          {
            
            post_p1296 = in_paren_p1269;
            in_paren_p1269 = (in_paren_p1269-1);
            post_p1296;
          }
          b_flag_p1270 = 1;
          goto switchBrk_p1295;
          
        case 10: 
          {
            
            if (__us_rsthread_stdio) 
              quesCol_p1297 = __semputc (32,(FILE_t130 *) (&__iob[1]));
            else
              {
                int quesCol_p1298;
                int pref_p1299;
                (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                pref_p1299 = ((*(&__iob[1]))._cnt);
                if (pref_p1299<0) 
                  quesCol_p1298 = __flsbuf (32,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    char *post_p1300;
                    post_p1300 = ((*(&__iob[1]))._ptr);
                    (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                    *post_p1300 = ((char) 32);
                    quesCol_p1298 = ((int) (*post_p1300));
                  }
                quesCol_p1297 = quesCol_p1298;
              }
            quesCol_p1297;
          }
          goto switchBrk_p1295;
          
        case 45: 
          {
            
            call_p1301 = fgetc (source_p1265);
            c_p1272 = call_p1301;
            if (c_p1272==41) 
              {
                
                word_over_line_p1273 = 1;
              }
            else
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1302 = __semputc (ch_p1266,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1303;
                    int pref_p1304;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1304 = ((*(&__iob[1]))._cnt);
                    if (pref_p1304<0) 
                      quesCol_p1303 = __flsbuf (ch_p1266,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1305;
                        post_p1305 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1305 = ((char) ch_p1266);
                        quesCol_p1303 = ((int) (*post_p1305));
                      }
                    quesCol_p1302 = quesCol_p1303;
                  }
                quesCol_p1302;
              }
          }
          ungetc (c_p1272,source_p1265);
          goto switchBrk_p1295;
          if (__us_rsthread_stdio) 
            quesCol_p1306 = __semputc (32,(FILE_t130 *) (&__iob[1]));
          else
            {
              int quesCol_p1307;
              int pref_p1308;
              (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
              pref_p1308 = ((*(&__iob[1]))._cnt);
              if (pref_p1308<0) 
                quesCol_p1307 = __flsbuf (32,(FILE_t130 *) (&__iob[1]));
              else
                {
                  char *post_p1309;
                  post_p1309 = ((*(&__iob[1]))._ptr);
                  (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                  *post_p1309 = ((char) 32);
                  quesCol_p1307 = ((int) (*post_p1309));
                }
              quesCol_p1306 = quesCol_p1307;
            }
          quesCol_p1306;
          goto switchBrk_p1295;
          
        case 92: 
          {
            
            call_p1310 = fgetc (source_p1265);
            switch (ch_p1266 = call_p1310)
              {
                
                
              case 40: 
                
              case 41: 
                {
                  
                  if (__us_rsthread_stdio) 
                    quesCol_p1312 = __semputc (ch_p1266,(FILE_t130 *) (&__iob[1]));
                  else
                    {
                      int quesCol_p1313;
                      int pref_p1314;
                      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                      pref_p1314 = ((*(&__iob[1]))._cnt);
                      if (pref_p1314<0) 
                        quesCol_p1313 = __flsbuf (ch_p1266,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          char *post_p1315;
                          post_p1315 = ((*(&__iob[1]))._ptr);
                          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                          *post_p1315 = ((char) ch_p1266);
                          quesCol_p1313 = ((int) (*post_p1315));
                        }
                      quesCol_p1312 = quesCol_p1313;
                    }
                  quesCol_p1312;
                }
                goto switchBrk_p1311;
                
              case 116: 
                {
                  
                  if (__us_rsthread_stdio) 
                    quesCol_p1316 = __semputc (9,(FILE_t130 *) (&__iob[1]));
                  else
                    {
                      int quesCol_p1317;
                      int pref_p1318;
                      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                      pref_p1318 = ((*(&__iob[1]))._cnt);
                      if (pref_p1318<0) 
                        quesCol_p1317 = __flsbuf (9,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          char *post_p1319;
                          post_p1319 = ((*(&__iob[1]))._ptr);
                          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                          *post_p1319 = ((char) 9);
                          quesCol_p1317 = ((int) (*post_p1319));
                        }
                      quesCol_p1316 = quesCol_p1317;
                    }
                  quesCol_p1316;
                }
                goto switchBrk_p1311;
                
              case 10: 
                goto switchBrk_p1311;
                
              case 110: 
                goto switchBrk_p1311;
                
              case 92: 
                {
                  
                  if (__us_rsthread_stdio) 
                    quesCol_p1320 = __semputc (34,(FILE_t130 *) (&__iob[1]));
                  else
                    {
                      int quesCol_p1321;
                      int pref_p1322;
                      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                      pref_p1322 = ((*(&__iob[1]))._cnt);
                      if (pref_p1322<0) 
                        quesCol_p1321 = __flsbuf (34,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          char *post_p1323;
                          post_p1323 = ((*(&__iob[1]))._ptr);
                          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                          *post_p1323 = ((char) 34);
                          quesCol_p1321 = ((int) (*post_p1323));
                        }
                      quesCol_p1320 = quesCol_p1321;
                    }
                  quesCol_p1320;
                }
                goto switchBrk_p1311;
                
              case 48: 
                {
                  
                  call_p1324 = fgetc (source_p1265);
                  switch (ch_p1266 = call_p1324)
                    {
                      
                      
                    case 49: 
                      {
                        
                        call_p1326 = fgetc (source_p1265);
                        switch (ch_p1266 = call_p1326)
                          {
                            
                            
                          case 51: 
                            fputs ((char *) "ff",(FILE_t130 *) (&__iob[1]));
                            goto switchBrk_p1327;
                            
                          case 52: 
                            fputs ((char *) "fi",(FILE_t130 *) (&__iob[1]));
                            goto switchBrk_p1327;
                            
                          case 53: 
                            fputs ((char *) "fl",(FILE_t130 *) (&__iob[1]));
                            goto switchBrk_p1327;
                            
                          case 54: 
                            fputs ((char *) "ffi",(FILE_t130 *) (&__iob[1]));
                            goto switchBrk_p1327;
                            
                          case 55: 
                            fputs ((char *) "ffl",(FILE_t130 *) (&__iob[1]));
                            goto switchBrk_p1327;
                            
                          default: 
                            fputs ((char *) "\\01",(FILE_t130 *) (&__iob[1]));
                            if (__us_rsthread_stdio) 
                              quesCol_p1328 = __semputc (ch_p1266,(FILE_t130 *) (&__iob[1]));
                            else
                              {
                                int quesCol_p1329;
                                int pref_p1330;
                                (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                                pref_p1330 = ((*(&__iob[1]))._cnt);
                                if (pref_p1330<0) 
                                  quesCol_p1329 = __flsbuf (ch_p1266,(FILE_t130 *) (&__iob[1]));
                                else
                                  {
                                    char *post_p1331;
                                    post_p1331 = ((*(&__iob[1]))._ptr);
                                    (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                                    *post_p1331 = ((char) ch_p1266);
                                    quesCol_p1329 = ((int) (*post_p1331));
                                  }
                                quesCol_p1328 = quesCol_p1329;
                              }
                            quesCol_p1328;
                          }
                        
                      switchBrk_p1327: 
                        ;
                      }
                      goto switchBrk_p1325;
                      
                    default: 
                      fputs ((char *) "\\0",(FILE_t130 *) (&__iob[1]));
                      if (__us_rsthread_stdio) 
                        quesCol_p1332 = __semputc (ch_p1266,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          int quesCol_p1333;
                          int pref_p1334;
                          (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                          pref_p1334 = ((*(&__iob[1]))._cnt);
                          if (pref_p1334<0) 
                            quesCol_p1333 = __flsbuf (ch_p1266,(FILE_t130 *) (&__iob[1]));
                          else
                            {
                              char *post_p1335;
                              post_p1335 = ((*(&__iob[1]))._ptr);
                              (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                              *post_p1335 = ((char) ch_p1266);
                              quesCol_p1333 = ((int) (*post_p1335));
                            }
                          quesCol_p1332 = quesCol_p1333;
                        }
                      quesCol_p1332;
                    }
                  
                switchBrk_p1325: 
                  ;
                }
                goto switchBrk_p1311;
                
              case 49: 
                
              case 50: 
                
              case 51: 
                
              case 52: 
                
              case 53: 
                
              case 54: 
                
              case 55: 
                {
                  
                  if (__us_rsthread_stdio) 
                    quesCol_p1336 = __semputc (92,(FILE_t130 *) (&__iob[1]));
                  else
                    {
                      int quesCol_p1337;
                      int pref_p1338;
                      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                      pref_p1338 = ((*(&__iob[1]))._cnt);
                      if (pref_p1338<0) 
                        quesCol_p1337 = __flsbuf (92,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          char *post_p1339;
                          post_p1339 = ((*(&__iob[1]))._ptr);
                          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                          *post_p1339 = ((char) 92);
                          quesCol_p1337 = ((int) (*post_p1339));
                        }
                      quesCol_p1336 = quesCol_p1337;
                    }
                  quesCol_p1336;
                }
                
              default: 
                {
                  
                  if (__us_rsthread_stdio) 
                    quesCol_p1340 = __semputc (ch_p1266,(FILE_t130 *) (&__iob[1]));
                  else
                    {
                      int quesCol_p1341;
                      int pref_p1342;
                      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                      pref_p1342 = ((*(&__iob[1]))._cnt);
                      if (pref_p1342<0) 
                        quesCol_p1341 = __flsbuf (ch_p1266,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          char *post_p1343;
                          post_p1343 = ((*(&__iob[1]))._ptr);
                          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                          *post_p1343 = ((char) ch_p1266);
                          quesCol_p1341 = ((int) (*post_p1343));
                        }
                      quesCol_p1340 = quesCol_p1341;
                    }
                  quesCol_p1340;
                }
              }
            
          switchBrk_p1311: 
            ;
          }
          goto switchBrk_p1295;
          
        default: 
          {
            
            if (__us_rsthread_stdio) 
              quesCol_p1344 = __semputc (ch_p1266,(FILE_t130 *) (&__iob[1]));
            else
              {
                int quesCol_p1345;
                int pref_p1346;
                (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                pref_p1346 = ((*(&__iob[1]))._cnt);
                if (pref_p1346<0) 
                  quesCol_p1345 = __flsbuf (ch_p1266,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    char *post_p1347;
                    post_p1347 = ((*(&__iob[1]))._ptr);
                    (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                    *post_p1347 = ((char) ch_p1266);
                    quesCol_p1345 = ((int) (*post_p1347));
                  }
                quesCol_p1344 = quesCol_p1345;
              }
            quesCol_p1344;
          }
        }
      
    switchBrk_p1295: 
      ;
    }
  else
    {
      
      switch (ch_p1266)
        {
          
          
        case 37: 
          fgets ((char *) junk_p1274,80,source_p1265);
          goto switchBrk_p1348;
          
        case 10: 
          goto switchBrk_p1348;
          
        case 45: 
          if (b_flag_p1270) 
            {
              
              b_flag_p1270 = 0;
              b_space_p1271 = 0;
            }
          goto switchBrk_p1348;
          
        case 40: 
          {
            
            post_p1349 = in_paren_p1269;
            in_paren_p1269 = (in_paren_p1269+1);
            post_p1349;
          }
          if (!word_over_line_p1273) 
            {
              
              if (!((__ctype+(1*1))[next_to_prev_ch_p1268]&(1|2))) 
                {
                  
                  switch (prev_ch_p1267)
                    {
                      
                      
                    case 108: 
                      
                    case 109: 
                      
                    case 110: 
                      
                    case 111: 
                      
                    case 113: 
                      
                    case 114: 
                      
                    case 115: 
                      
                    case 116: 
                      goto switchBrk_p1350;
                      
                    case 121: 
                      {
                        
                        if (__us_rsthread_stdio) 
                          quesCol_p1351 = __semputc (10,(FILE_t130 *) (&__iob[1]));
                        else
                          {
                            int quesCol_p1352;
                            int pref_p1353;
                            (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                            pref_p1353 = ((*(&__iob[1]))._cnt);
                            if (pref_p1353<0) 
                              quesCol_p1352 = __flsbuf (10,(FILE_t130 *) (&__iob[1]));
                            else
                              {
                                char *post_p1354;
                                post_p1354 = ((*(&__iob[1]))._ptr);
                                (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                                *post_p1354 = ((char) 10);
                                quesCol_p1352 = ((int) (*post_p1354));
                              }
                            quesCol_p1351 = quesCol_p1352;
                          }
                        quesCol_p1351;
                      }
                      goto switchBrk_p1350;
                      
                    case 98: 
                      if (b_space_p1271) 
                        {
                          
                          if (__us_rsthread_stdio) 
                            quesCol_p1355 = __semputc (32,(FILE_t130 *) (&__iob[1]));
                          else
                            {
                              int quesCol_p1356;
                              int pref_p1357;
                              (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                              pref_p1357 = ((*(&__iob[1]))._cnt);
                              if (pref_p1357<0) 
                                quesCol_p1356 = __flsbuf (32,(FILE_t130 *) (&__iob[1]));
                              else
                                {
                                  char *post_p1358;
                                  post_p1358 = ((*(&__iob[1]))._ptr);
                                  (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                                  *post_p1358 = ((char) 32);
                                  quesCol_p1356 = ((int) (*post_p1358));
                                }
                              quesCol_p1355 = quesCol_p1356;
                            }
                          quesCol_p1355;
                        }
                      goto switchBrk_p1350;
                      
                    case 97: 
                      
                    case 99: 
                      
                    case 100: 
                      
                    case 101: 
                      
                    case 102: 
                      
                    case 103: 
                      
                    case 104: 
                      
                    case 105: 
                      
                    case 106: 
                      
                    case 107: 
                      
                    case 120: 
                      {
                        
                        if (__us_rsthread_stdio) 
                          quesCol_p1359 = __semputc (32,(FILE_t130 *) (&__iob[1]));
                        else
                          {
                            int quesCol_p1360;
                            int pref_p1361;
                            (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                            pref_p1361 = ((*(&__iob[1]))._cnt);
                            if (pref_p1361<0) 
                              quesCol_p1360 = __flsbuf (32,(FILE_t130 *) (&__iob[1]));
                            else
                              {
                                char *post_p1362;
                                post_p1362 = ((*(&__iob[1]))._ptr);
                                (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                                *post_p1362 = ((char) 32);
                                quesCol_p1360 = ((int) (*post_p1362));
                              }
                            quesCol_p1359 = quesCol_p1360;
                          }
                        quesCol_p1359;
                      }
                      goto switchBrk_p1350;
                      
                    default: 
                      goto switchBrk_p1350;
                    }
                  
                switchBrk_p1350: 
                  ;
                }
              else
                {
                  
                  if (__us_rsthread_stdio) 
                    quesCol_p1363 = __semputc (32,(FILE_t130 *) (&__iob[1]));
                  else
                    {
                      int quesCol_p1364;
                      int pref_p1365;
                      (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                      pref_p1365 = ((*(&__iob[1]))._cnt);
                      if (pref_p1365<0) 
                        quesCol_p1364 = __flsbuf (32,(FILE_t130 *) (&__iob[1]));
                      else
                        {
                          char *post_p1366;
                          post_p1366 = ((*(&__iob[1]))._ptr);
                          (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                          *post_p1366 = ((char) 32);
                          quesCol_p1364 = ((int) (*post_p1366));
                        }
                      quesCol_p1363 = quesCol_p1364;
                    }
                  quesCol_p1363;
                }
            }
          b_space_p1271 = 1;
          word_over_line_p1273 = 0;
          goto switchBrk_p1348;
          
        default: 
          b_flag_p1270 = 0;
          goto switchBrk_p1348;
        }
      
    switchBrk_p1348: 
      ;
    }
  next_to_prev_ch_p1268 = prev_ch_p1267;
  prev_ch_p1267 = ch_p1266;
  
whileCont_p1291: 
  ;
  call_p1293 = fgetc (source_p1265);
  ch_p1266 = call_p1293;
  if (ch_p1266!=(-1)) 
    goto whileTop_p1290;
}
void psparse (FILE_t130 *source_p1276)
{
  char *str_p1277;
  char junk_p1278[80];
  int ch_p1279;
  int para_p1280;
  int last_p1281;
  int call_p1370;
  int quesCol_p1372;
  int post_p1376;
  int quesCol_p1377;
  int post_p1381;
  int quesCol_p1382;
  int quesCol_p1386;
  int call_p1390;
  int quesCol_p1392;
  int quesCol_p1396;
  int quesCol_p1400;
  int quesCol_p1404;
  int quesCol_p1408;
  int quesCol_p1412;
  int quesCol_p1416;
  para_p1280 = 0;
  last_p1281 = 0;
  goto whileCont_p1368;
  
whileTop_p1367: 
  ;
  switch (ch_p1279)
    {
      
      
    case 37: 
      if (para_p1280==0) 
        fgets ((char *) junk_p1278,80,source_p1276);
      else
        {
          
          if (__us_rsthread_stdio) 
            quesCol_p1372 = __semputc (ch_p1279,(FILE_t130 *) (&__iob[1]));
          else
            {
              int quesCol_p1373;
              int pref_p1374;
              (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
              pref_p1374 = ((*(&__iob[1]))._cnt);
              if (pref_p1374<0) 
                quesCol_p1373 = __flsbuf (ch_p1279,(FILE_t130 *) (&__iob[1]));
              else
                {
                  char *post_p1375;
                  post_p1375 = ((*(&__iob[1]))._ptr);
                  (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                  *post_p1375 = ((char) ch_p1279);
                  quesCol_p1373 = ((int) (*post_p1375));
                }
              quesCol_p1372 = quesCol_p1373;
            }
          quesCol_p1372;
        }
      
    case 10: 
      if (last_p1281==1) 
        {
          
          puts ((char *) "");
          last_p1281 = 0;
        }
      goto switchBrk_p1371;
      
    case 40: 
      {
        
        post_p1376 = para_p1280;
        para_p1280 = (para_p1280+1);
        if (post_p1376>0) 
          {
            
            if (__us_rsthread_stdio) 
              quesCol_p1377 = __semputc (ch_p1279,(FILE_t130 *) (&__iob[1]));
            else
              {
                int quesCol_p1378;
                int pref_p1379;
                (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                pref_p1379 = ((*(&__iob[1]))._cnt);
                if (pref_p1379<0) 
                  quesCol_p1378 = __flsbuf (ch_p1279,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    char *post_p1380;
                    post_p1380 = ((*(&__iob[1]))._ptr);
                    (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                    *post_p1380 = ((char) ch_p1279);
                    quesCol_p1378 = ((int) (*post_p1380));
                  }
                quesCol_p1377 = quesCol_p1378;
              }
            quesCol_p1377;
          }
      }
      goto switchBrk_p1371;
      
    case 41: 
      {
        
        post_p1381 = para_p1280;
        para_p1280 = (para_p1280-1);
        if (post_p1381>1) 
          {
            
            if (__us_rsthread_stdio) 
              quesCol_p1382 = __semputc (ch_p1279,(FILE_t130 *) (&__iob[1]));
            else
              {
                int quesCol_p1383;
                int pref_p1384;
                (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                pref_p1384 = ((*(&__iob[1]))._cnt);
                if (pref_p1384<0) 
                  quesCol_p1383 = __flsbuf (ch_p1279,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    char *post_p1385;
                    post_p1385 = ((*(&__iob[1]))._ptr);
                    (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                    *post_p1385 = ((char) ch_p1279);
                    quesCol_p1383 = ((int) (*post_p1385));
                  }
                quesCol_p1382 = quesCol_p1383;
              }
            quesCol_p1382;
          }
        else
          {
            
            if (__us_rsthread_stdio) 
              quesCol_p1386 = __semputc (32,(FILE_t130 *) (&__iob[1]));
            else
              {
                int quesCol_p1387;
                int pref_p1388;
                (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                pref_p1388 = ((*(&__iob[1]))._cnt);
                if (pref_p1388<0) 
                  quesCol_p1387 = __flsbuf (32,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    char *post_p1389;
                    post_p1389 = ((*(&__iob[1]))._ptr);
                    (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                    *post_p1389 = ((char) 32);
                    quesCol_p1387 = ((int) (*post_p1389));
                  }
                quesCol_p1386 = quesCol_p1387;
              }
            quesCol_p1386;
          }
      }
      last_p1281 = 1;
      goto switchBrk_p1371;
      
    case 92: 
      if (para_p1280>0) 
        {
          
          call_p1390 = fgetc (source_p1276);
          switch (ch_p1279 = call_p1390)
            {
              
              
            case 40: 
              
            case 41: 
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1392 = __semputc (ch_p1279,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1393;
                    int pref_p1394;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1394 = ((*(&__iob[1]))._cnt);
                    if (pref_p1394<0) 
                      quesCol_p1393 = __flsbuf (ch_p1279,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1395;
                        post_p1395 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1395 = ((char) ch_p1279);
                        quesCol_p1393 = ((int) (*post_p1395));
                      }
                    quesCol_p1392 = quesCol_p1393;
                  }
                quesCol_p1392;
              }
              goto switchBrk_p1391;
              
            case 116: 
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1396 = __semputc (9,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1397;
                    int pref_p1398;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1398 = ((*(&__iob[1]))._cnt);
                    if (pref_p1398<0) 
                      quesCol_p1397 = __flsbuf (9,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1399;
                        post_p1399 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1399 = ((char) 9);
                        quesCol_p1397 = ((int) (*post_p1399));
                      }
                    quesCol_p1396 = quesCol_p1397;
                  }
                quesCol_p1396;
              }
              goto switchBrk_p1391;
              
            case 110: 
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1400 = __semputc (10,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1401;
                    int pref_p1402;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1402 = ((*(&__iob[1]))._cnt);
                    if (pref_p1402<0) 
                      quesCol_p1401 = __flsbuf (10,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1403;
                        post_p1403 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1403 = ((char) 10);
                        quesCol_p1401 = ((int) (*post_p1403));
                      }
                    quesCol_p1400 = quesCol_p1401;
                  }
                quesCol_p1400;
              }
              goto switchBrk_p1391;
              
            case 92: 
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1404 = __semputc (92,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1405;
                    int pref_p1406;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1406 = ((*(&__iob[1]))._cnt);
                    if (pref_p1406<0) 
                      quesCol_p1405 = __flsbuf (92,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1407;
                        post_p1407 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1407 = ((char) 92);
                        quesCol_p1405 = ((int) (*post_p1407));
                      }
                    quesCol_p1404 = quesCol_p1405;
                  }
                quesCol_p1404;
              }
              goto switchBrk_p1391;
              
            case 48: 
              
            case 49: 
              
            case 50: 
              
            case 51: 
              
            case 52: 
              
            case 53: 
              
            case 54: 
              
            case 55: 
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1408 = __semputc (92,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1409;
                    int pref_p1410;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1410 = ((*(&__iob[1]))._cnt);
                    if (pref_p1410<0) 
                      quesCol_p1409 = __flsbuf (92,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1411;
                        post_p1411 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1411 = ((char) 92);
                        quesCol_p1409 = ((int) (*post_p1411));
                      }
                    quesCol_p1408 = quesCol_p1409;
                  }
                quesCol_p1408;
              }
              
            default: 
              {
                
                if (__us_rsthread_stdio) 
                  quesCol_p1412 = __semputc (ch_p1279,(FILE_t130 *) (&__iob[1]));
                else
                  {
                    int quesCol_p1413;
                    int pref_p1414;
                    (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
                    pref_p1414 = ((*(&__iob[1]))._cnt);
                    if (pref_p1414<0) 
                      quesCol_p1413 = __flsbuf (ch_p1279,(FILE_t130 *) (&__iob[1]));
                    else
                      {
                        char *post_p1415;
                        post_p1415 = ((*(&__iob[1]))._ptr);
                        (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                        *post_p1415 = ((char) ch_p1279);
                        quesCol_p1413 = ((int) (*post_p1415));
                      }
                    quesCol_p1412 = quesCol_p1413;
                  }
                quesCol_p1412;
              }
              goto switchBrk_p1391;
            }
          
        switchBrk_p1391: 
          ;
        }
      goto switchBrk_p1371;
      
    default: 
      if (para_p1280>0) 
        {
          
          if (__us_rsthread_stdio) 
            quesCol_p1416 = __semputc (ch_p1279,(FILE_t130 *) (&__iob[1]));
          else
            {
              int quesCol_p1417;
              int pref_p1418;
              (*(&__iob[1]))._cnt = (((*(&__iob[1]))._cnt)-1);
              pref_p1418 = ((*(&__iob[1]))._cnt);
              if (pref_p1418<0) 
                quesCol_p1417 = __flsbuf (ch_p1279,(FILE_t130 *) (&__iob[1]));
              else
                {
                  char *post_p1419;
                  post_p1419 = ((*(&__iob[1]))._ptr);
                  (*(&__iob[1]))._ptr = ((char *) (((int) ((*(&__iob[1]))._ptr))+1));
                  *post_p1419 = ((char) ch_p1279);
                  quesCol_p1417 = ((int) (*post_p1419));
                }
              quesCol_p1416 = quesCol_p1417;
            }
          quesCol_p1416;
        }
    }
  
switchBrk_p1371: 
  ;
  
whileCont_p1368: 
  ;
  call_p1370 = fgetc (source_p1276);
  ch_p1279 = call_p1370;
  if (ch_p1279!=(-1)) 
    goto whileTop_p1367;
}
