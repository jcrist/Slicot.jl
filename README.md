[SLICOT](http://slicot.org/) wrapper for JuliaLang.

[![Build Status](https://travis-ci.org/jcrist/Slicot.jl.svg?branch=master)](https://travis-ci.org/jcrist/Slicot.jl)

Status
------
Unfinished, untested, extremely new. 

* All Subroutines have been wrapped in the "RAW" API, with a few exceptions
    - No true functions have been wrapped (around 12 routines)
    - No subroutines that require a callback have been wrapped
* So far only 1 routine has been wrapped in the "Simple" API.

However, more are on the way. 

For more up to date information, check "status.md"

Installation
------------
```
Pkg.clone("https://github.com/jcrist/Slicot.jl.git")
Pkg.build("Slicot")
```

*Note: The build process is extremely new, and right now only works on unix/linux
based systems, and for a small number of fortran compilers. If you find it doesn't
work for you, please file an issue, or send a pull request to improve it.*

Use
---
There are two submodules: "simple" and "raw". 

Simple makes assumptions about your use case, but allows for a much 
shorter call signature. Inputs are validated inside Julia (allows for 
helpful error messages). Output only arrays are created in the subroutine,
and array dimensions are interpreted from the input arrays (no need for 
explicit passing).

Raw makes no assumptions and leaves all calls up to you. The call signature
for the "Raw" api is *almost* exactly the same as the original FORTRAN. The
only difference is, "INFO" is not passed into the function. This is to
ensure that the error handling mechanism is always valid. The remaining 
inputs are not sanitized in julia, and will result in a call to xerbla.f
in the case of an error. An alternate version of xerbla.f has been included 
that doesn't exit upon error, allowing these errors to be handled more
gracefully. **If using the raw bindings, always check if INFO < 0 after
each call**. In addition to this, a "SlicotError" will be thrown as well.

Note that the raw functions only check for INFO < 0. For some subroutines,
other types of errors may occur. As each routine is added to the simple
submodule, the corresponding raw routine will be updated. For now, all 
routines were just wrapped programmatically by a **very** dumb script.
Improvements are clearly needed.
