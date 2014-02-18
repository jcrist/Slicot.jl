####################################################################
#  Alternate routines makefile, for use with Julia                 #
#  Makefile for creating/updating the alternate Library object file# 
#  for SLICOT on Unix platforms.                                   #
#  SLICOT, Release 5.0                  ./slicot/src_alt/makefile  #
####################################################################
#								   #
#  This makefile compiles and creates xerbla.a, which contains     #
#  an alternate implementation of the routine xerbla.a for use     #
#  with Julia. This version doesn't print to stdout/stderr, and    #
#  doesn't stop the program. The responsibility for handling       #
#  errors falls on julia. Always check upon return if INFO < 0,    #
#  and act accordingly.                                            #
#								   #
#  To compile, enter:						   #
#  	make							   #
#  						                   #
#  This results in xerbla.a being created in this directory.       #
#								   #
#  The command:                                                    #
#  	make clean						   #
#								   #
#  removes xerbla.o and xerbla.a from the directory                #

include ../make.inc

DSLSRC = xerbla.o

all: double

double: $(DSLSRC)
	$(ARCH) $(ARCHFLAGS) -o xerbla.a $(DSLSRC)

$(DSLSRC): $(FRC)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o
	rm -f *.a

.f.o: 
	$(FORTRAN) -c $<
