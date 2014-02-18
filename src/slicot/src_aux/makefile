####################################################################
#  Modified Auxiliary routines makefile, for use with Julia        #
#  Makefile for creating/updating the auxiliary Library object file# 
#  for SLICOT on Unix platforms.                                   #
#  SLICOT, Release 5.0                  ./slicot/src_aux/makefile  #
#  Vasile Sima, ICI Bucharest                                      #
#  Jan. 24, 2005.                                                  #
#  Revised  Jan. 9, 2009, Sep. 26, 2009, Jan. 25, 2010.,	   #
#  	    Feb 14, 2014					   #
####################################################################
#
#  This is the makefile to create/update an auxiliary library 
#  for SLICOT. It contains a modified LAPACK routine dhgeqz 
#  (modification made by Dai), which converged also on some special
#  examples for which the distributed LAPACK routine didn't converge.
#  Moreover, three errors have been removed in dlagv2, dhgeqz and dtgsy2. 
#  On some platforms, the old BLAS routine dcabs1 might also be needed.
#  Examples are some Linux platforms, but also Sun platforms.
#  To compile it, comment the statement defining DSLSRC and uncomment
#  the statements below referring dcabs1.
#  The SLICOT Library routines are written for double precision only.
#
#  The command
#       make
#  without any arguments creates or updates a library called
#       lpkaux.so
#  in the next higher directory level.
#
#  To remove the object files after the library is created, enter
#       make clean
#
#  On some systems, you can force the source files to be recompiled by
#  entering (for example)
#       make double FRC=FRC
#
#
#######################################################################
 
include ../make.inc

DSLSRC = \
    dlagv2.o dhgeqz.o dtgsy2.o

#DSLSRC = \
#    dcabs1.o dhgeqz.o dtgsy2.o

all: double

double: $(DSLSRC)
	$(LOAD) $(LOADFLAGS) -o $(LPKAUXLIB) $(DSLSRC)

$(DSLSRC): $(FRC)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o

.f.o: 
	$(FORTRAN) $(OPTS) -c $<
