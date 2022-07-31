typedef unsigned long ULONG;

typedef struct cpMSCID {
	ULONG    sid: 16,
		 swno: 8,
		 fill: 8;
} CP_MSCID_TYPE;

struct A {
   CP_MSCID_TYPE t;
};

int b[sizeof(struct A)];

main () {
  int i;
}
