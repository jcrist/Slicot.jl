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

#Define a template for make.inc.in:
make_inc_in =
""" 
F77      = @F77@
OPTS     = -fPIC -O4
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
make_inc_in_path = joinpath(altfildir, "make.inc.in")
#Write the make.inc template
make_inc_in_fil = open(make_inc_in_path, "w+")
write(make_inc_in_fil, make_inc_in)
close(make_inc_in_fil)

# Define Build Process
provides(BuildProcess,
        (@build_steps begin
                GetSources(libslicot)
                CreateDirectory(usr)
                CreateDirectory(lib)
                #Build the library:
                FileRule(libslicotpath,@build_steps begin
                     ChangeDirectory(builddir)
                    `$(joinpath(altfildir, "configure"))`
                    `cp -r $(joinpath(altfildir, "src_alt")) $builddir`
                    `cp $(joinpath(altfildir, "top_makefile")) $(joinpath(builddir, "makefile"))`
                    `cp $(joinpath(altfildir, "src_makefile")) $(joinpath(builddir, "src", "makefile"))`
                    `make`
                    `make clean`
                end)
            end), libslicot)
@BinDeps.install
