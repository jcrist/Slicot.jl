## Wrapper for SLICOT library ##

module Slicot

import Base.USE_BLAS64

if USE_BLAS64
    typealias BlasInt Int64
else
    typealias BlasInt Int32
end

type SlicotException <:Exception
    info::BlasInt
    msg::ASCIIString
end

## SIMPLIFIED ROUTINES ##
#
# These are wrapped with "sugar", using defaults for many
# inputs, and automatically creating outputs of required
# size. For 99% of cases, you want to use these.
#
# These are exported, and may be called with "using", or with
# explicit imports
include("simple.jl")

## RAW ROUTINES ##
#
# These are just the FORTRAN call strings minus the "INFO" param. 
# For most cases, you shouldn't need to have any need to 
# touch these. If your case requires more finese, please file 
# an issue, as the simple.jl routines interface may need to be
# changed to allow for your use case. Assumptions may have been
# made in the design of of the simple api that were unwarranted. 
#
# Possible reasons to still use the raw strings:
#
#---> 1.) Reduce overhead. No checks occur in julia. All data
#         is immediately sent to the fortran routine.
#---> 2.) Reduce memory use. The simplified api allocates extra
#         arrays on the fly. By using the raw api, you can reuse
#         arrays for more than one operations (as you would in
#         the original fortran.
#---> 3.) Quick translate of legacy code. The call signature is
#         exactly the same.
#
# A "SlicotError" is thrown if INFO < 0. This may be
# handled same as any other error.
#
# To use these, you must import them explicitly. The raw module
# doesn't export anything, because namespaces are awesome.
include("raw.jl")

end     #Module
