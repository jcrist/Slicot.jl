####################################################################
#  Modified SLICOT make include file, for use with Julia           #
#  SLICOT, Release 5.0                          ./slicot/make.inc  #
#  October 31, 1996                                                #
#  Revised December 7, 1999; February 14, 2005.; February 16, 2014 #
####################################################################
#
#  The machine (platform) identifier to append to the library names
#
#  PLAT = _linux
#  
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
FORTRAN  = f77
OPTS     = -O4 -u -fPIC
NOOPT    = -u
LOADER   = f77
LOADOPTS = $(ALT_XERBLALIB) $(BLASLIB) $(LAPACKLIB)
#
#  The loader, archiver and the flag(s) to use when building the library
#
LOAD     = ld
LOADFLAGS= -shared
ARCH     = ar
ARCHFLAGS= r
# ARCHFLAGS= cr
# RANLIB   = ranlib
#
#  The location of the libraries to which you will link.  (The 
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
BLASLIB     = /usr/lib/libblas.so.3 
LAPACKLIB    = /usr/lib/liblapack.so.3
SLICOTLIB    = ../libslicot.so
LPKAUXLIB    = ../lpkaux.so
#  For Slicot.jl, a modified version of XERBLA.f is provided in src_alt.
#  It is recommended to compile using this version, as it doesn't stop
#  the program on error, allowing julia to handle all errors. If this is
#  not desired, comment out this line:
ALT_XERBLALIB = ../src_alt/xerbla.a

