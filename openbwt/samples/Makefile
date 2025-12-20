CC						= gcc
CFLAGS				= -O3 -fomit-frame-pointer
CPPFLAGS			= -Wall -DNDEBUG -I..
all: bwt unbwt
bwt: ../openbwt.o bwt.o
unbwt: ../openbwt.o unbwt.o
clean:
	$(RM) bwt unbwt bwt.exe unbwt.exe bwt.o unbwt.o ../openbwt.o
bwt.o unbwt.o ../openbwt.o: ../openbwt.h Makefile
