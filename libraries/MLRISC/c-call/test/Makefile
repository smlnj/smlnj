ML_BUILD_FLAGS =	-Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm

build-sparc:
#no TDP for sparc
	ml-build sources.cm SparcTest.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g -c mlrisc.s
	gcc -g  -c glue.c
	gcc -g  -c sanity.c
	gcc -g  mlrisc.o glue.o sanity.o -o sanity
	gcc -g  -c main.c
	gcc -g  mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff main.out sanity.out

build-x86-64:
	ml-build $(ML_BUILD_FLAGS) sources.cm X86_64Test.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g -m64 -c mlrisc.s
	gcc -g  -m64 -c glue.c
	gcc -g  -m64 -c sanity.c
	gcc -g  -m64 mlrisc.o glue.o sanity.o -o sanity
	gcc -g  -m64 -c main.c
	gcc -g  -m64 mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

X86_FLAGS=-m32 -march=i686 -g

build-x86:
	ml-build $(ML_BUILD_FLAGS) sources.cm X86Test.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc $(X86_FLAGS)  -c mlrisc.s
	gcc $(X86_FLAGS)   -c glue.c
	gcc $(X86_FLAGS)   -c sanity.c
	gcc $(X86_FLAGS) -S glue.c
	gcc $(X86_FLAGS)   mlrisc.o glue.o sanity.o -o sanity
	gcc $(X86_FLAGS)   -c main.c
	gcc $(X86_FLAGS)   mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

clean:
	rm -rf *.s *.c *.o main sanity *~ .cm *.out test-main*