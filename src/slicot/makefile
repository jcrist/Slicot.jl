####################################################################
#  Modified SLICOT main makefile, for use with Julia               #
#  Top level makefile for building the libslicot.so library file,  #
#  and the libaux.so library file. 				   #
#  SLICOT, Release 5.0                          ./slicot/makefile  #
#  October 31, 1996                                                #
#  Revised December 7, 1999; February 14, 2005.; February 16, 2014 #
####################################################################
#
#  This makefile creates/updates the SLICOT Library object file, and
#  the auxiliary library. To perform all these actions, enter 
#       make
#
#  To remove the object files for SLICOT routines and auxiliary
#  routines, enter
#       make clean
#
####################################################################

include make.inc

all:
	( cd src_alt; $(MAKE) )
	( cd src; $(MAKE) )
	( cd src_aux; $(MAKE) )

clean:
	( cd src_alt; $(MAKE) clean )
	( cd src; $(MAKE) clean )
	( cd src_aux; $(MAKE) clean )
