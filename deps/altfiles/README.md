This directory contains alternate files used to replace files in SLICOT
before building for use with Julia.

Contents:

- configure
- make.inc.in
- makefiles
- `src_alt\xerbla.f`

The configure and makefiles are used to build into a shared library, rather
than a static library like the base configuration does. The alternative
xerbla file allows julia to handle the errors instead of crashing,
as the standard xerbla would have it do.
