# Build a shared object for the interpreter.

X86_C_FLAGS=-m32 -g3 -march=i686 -fPIC

build-x86-linux: vararg-interp-x86-linux.s util.c
	gcc -shared $(X86_C_FLAGS) util.c vararg-interp-x86-linux.s -o vararg

test-x86-linux: vararg-interp-x86-linux.s util.c test.c
	gcc -shared $(X86_C_FLAGS) util.c test.c -o vararg

build-x86-darwin: vararg-interp-x86-darwin.s util.c
	gcc $(X86_C_FLAGS) -c vararg-interp-x86-darwin.s
	gcc $(X86_C_FLAGS) -c util.c
	gcc -dynamiclib $(X86_C_FLAGS) util.o vararg-interp-x86-darwin.o -o vararg

test-x86-darwin: vararg-interp-x86-darwin.s util.c test.c
	gcc $(X86_C_FLAGS) -c test.c	
	gcc $(X86_C_FLAGS) -c util.c
	gcc -dynamiclib $(X86_C_FLAGS) util.o test.o -o vararg

interp-x86-darwin: vararg-interp-x86-darwin.s util.c test-interp.c
	gcc $(X86_C_FLAGS) -c test-interp.c	
	gcc $(X86_C_FLAGS) -c util.c
	gcc $(X86_C_FLAGS) -c vararg-interp-x86-darwin.s
	gcc $(X86_C_FLAGS) util.o test-interp.o vararg-interp-x86-darwin.o -o interp

AMD64_C_FLAGS=-m32 -g3

build-amd64-linux: vararg-interp-amd64.s util.c
	gcc -shared $(AMD64_C_FLAGS) util.c vararg-interp-amd64.s -o vararg

build-amd64-darwin: vararg-interp-amd64.s util.c
	gcc -dynlib $(AMD64_C_FLAGS) util.c vararg-interp-amd64.s -o vararg
