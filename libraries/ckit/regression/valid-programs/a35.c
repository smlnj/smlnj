typedef unsigned short USHORT;
typedef unsigned char UCHAR;

typedef struct {
	USHORT	size;		
	UCHAR	type;		
	UCHAR	class;		
	long	retran :8;	
	long	to_esid :24;	
	long	fill :8;	
	long	from_esid :24;	
} MGIHDR;

main () {
  int i[sizeof(MGIHDR)];
}
