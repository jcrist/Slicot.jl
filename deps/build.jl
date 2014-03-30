using BinDeps
@BinDeps.setup

#Define dependencies. Because blas and lapack are required for julia,
#only the slicot source needs to be downloaded.
libblas = library_dependency(Base.libblas_name)
liblapack = library_dependency(Base.liblapack_name)
libslicot = library_dependency("libslicot", depends = [libblas, liblapack])

# Define location of source for slicot
const slicot_version = "5.0"
slicot_unpacked = "slicot-5.0+20101122"
download_url = "http://ftp.de.debian.org/debian/pool/main/s/slicot/slicot_5.0+20101122.orig.tar.gz"
provides(Sources, URI(download_url), libslicot, unpacked_dir=slicot_unpacked)

# Define Directories for Build
depsdir = BinDeps.depsdir(libslicot)
usr = joinpath(depsdir, "usr")
lib = joinpath(usr, "lib")
altfildir = joinpath(depsdir, "altfiles")
builddir = joinpath(depsdir, "src", slicot_unpacked)
libslicotpath = joinpath(lib, "$(libslicot.name).so")

# List possible fortran compilers and the corresponding options
f_compilers = ["gfortran"   "-fPIC -O4";
               "f77"        "-O4 -u -fPIC"]

# TODO: Add these compilers:
#"g77", "xlf", "frt", "pgf77", "cf77", 
#"fort77", "fl32", "af77", "xlf90", "f90", "pgf90", "pghpf", 
#"epcf90", "g95", "xlf95", "f95", "fort", "ifort", "ifc", "efc", 
#"pgfortran", "pgf95", "lf95", "ftn", "nagfor"

# Determine system fortran compiler 
F77 = false
OPTS = false
for (i, f) in enumerate(f_compilers[:,1])
    if success(`which $f`)
        F77 = f
        OPTS = f_compilers[i,2]
        break
    end
end
if F77==false
    error("No Fortran Compiler Found")
end

#Define a template for make.inc.in:
make_inc =
""" 
F77      = $F77
OPTS     = $OPTS
LOAD     = ld
LOADFLAGS= -shared
LOADOPTS = \$(ALT_XERBLALIB) \$(BLASLIB) \$(LAPACKLIB)
ARCH     = ar
ARCHFLAGS= r

BLASLIB     = /usr/lib/$(libblas.name).so 
LAPACKLIB    = /usr/lib/$(liblapack.name).so
SLICOTLIB    = $(libslicotpath) 
ALT_XERBLALIB = ../src_alt/xerbla.a
"""
make_inc_path = joinpath(altfildir, "make.inc")
#Write the make.inc file
make_inc_fil = open(make_inc_path, "w+")
write(make_inc_fil, make_inc)
close(make_inc_fil)

# Define Build Process
provides(BuildProcess,
        (@build_steps begin
                GetSources(libslicot)
                CreateDirectory(usr)
                CreateDirectory(lib)
                #Build the library:
                FileRule(libslicotpath,@build_steps begin
                     ChangeDirectory(builddir)
                    `cp -r $(joinpath(altfildir, "src_alt")) $builddir`
                    `cp $(joinpath(altfildir, "make.inc")) $(joinpath(builddir, "make.inc"))`
                    `cp $(joinpath(altfildir, "top_makefile")) $(joinpath(builddir, "makefile"))`
                    `cp $(joinpath(altfildir, "src_makefile")) $(joinpath(builddir, "src", "makefile"))`
                    `make`
                    `make clean`
                end)
            end), libslicot, os=:unix)
@BinDeps.install
