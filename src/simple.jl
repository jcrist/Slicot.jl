## Simplified wrapper for SLICOT routines ##

module Simple

import Slicot: SlicotException, BlasInt

export tb04ad

#TODO: Slice return matrices before returning on tb04ad

function tb04ad(rowcol::Char, n::Integer, m::Integer, p::Integer, 
    A::Array{FloatingPoint, 2}, B::Array{FloatingPoint, 2}, 
    C::Array{FloatingPoint, 2}, D::Array{FloatingPoint, 2}, 
    tol1=0.0, tol2=0.0, ldwork::Integer=-1)
    ## Calculate Transfer Matrix of State Space Representation (A,B,C,D) ##
    ## INPUTS:
    #---> rowcol = 'R' for row/den, 'C' for col/den
    #--->      n = Order of state space representation
    #--->      m = Number of system inputs
    #--->      p = Number of system outputs
    #--->    A-D = System matrices A-D
    #--->   tol1 = Tolerance used in determining i-th row of T(s)
    #--->   tol2 = Tolerance used to separate out controllable subsystem
    #---> ldwork = Length of array DWORK. Larger ldwork improves performance
    ## OUTPUTS:
    #--->      A = A[1:NR, 1:NR] contains upper block Hessenberg matrix A
    #--->      B = B[1:NR, 1:m] contains the transformed input/state matrix B
    #--->      C = C[1:p, 1:NR] contains the transformed state/output matrix C
    #--->     NR = Order of the transformed state-space representation
    #--->  INDEX = Degrees of the denominator polynomials
    #---> DCOEFF = DCOEFF[1:porm, 1:kdcoef] contains coefficients of den poly
    #---> UCOEFF = UCOEFF[1:porm, 1:porp] contains coefficients of num poly
    #--->   INFO = Exit status. If < 0, -INFOth argument had bad value

    #Determine required params for mode
    if rowcol == 'R'
        mp, pm = m, p
        porm, porp = p, m
        bcol = m                    #Number of cols B must have
        ldc_min = max(1,m)          #Min ldc
        ldd_min = max(1,p)          #Min ldd
    elseif rowcol == 'C'
        mp, pm = p, m
        porm, porp = m, p
        bcol = max(m, p)            #Number of cols B must have
        ldc_min = max(1,m, p)       #Min ldc
        ldd_min = ldc_min           #Min ldd
    else
        #Invalid rowcol mode. 
        error("rowcol must be either 'R' or 'C'")
    end

    #Params that don't change with mode
    lda_min = max(1,n)              #Min lda
    ldb_min = lda_min               #Min ldb
    lda = size(A)[1]
    ldb = size(B)[1]
    ldc = size(C)[1]
    ldd = size(D)[1]

    #Validate all input
    if size(A)[2] != n
        error("A must have the same # of cols as n")
    elseif lda < lda_min
        error("LDA not in bounds")
    elseif size(B)[2] != bcol
        error("Number of B columns don't align")
    elseif ldb < ldb_min
        error("LDB not in bounds")
    elseif size(C)[2] != n
        error("C must have the same # of cols as n")
    elseif ldc < ldc_min
        error("LDC not in bounds")
    elseif size(D)[2] != m
        error("D must have the same # of cols as m")
    elseif ldd < ldd_min
        error("LDD not in bounds")
    end

    #Calculate the min bound on ldwork
    ldwork_min = max(1, n*(n + 1) + max(n*mp + 2*n + max(n, mp), 3*mp, pm))
    if ldwork < 0
        #If ldwork isn't specified, set it to the min value
        ldwork = ldwork_min
    elseif ldwork < ldwork_min
        error("ldwork is not >= the recommended size")
    end

    #Determine remainder of parameters
    NR = Array(BlasInt,1)
    INDEX = Array(BlasInt, porm)
    lddcoe= max(1,porm)
    DCOEFF = Array(Float64, lddcoe, n+1)
    lduc01 = max(1,porm)
    lduc02= max(1,porp)
    UCOEFF = Array(Float64, lduc01, lduc02, n+1)
    IWORK = Array(Float64, n+max(m,p))
    DWORK = Array(Float64, ldwork)
    INFO = Array(BlasInt, 1)

    #Create copies of arrays, to prevent change by reference
    AR = copy(A)
    BR = copy(B)
    CR = copy(C)
    DR = copy(D)

    #Call the subroutine
    ccall((:tb04ad_, "libslicot"), Void, 
            (Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}),
            &rowcol, &n, &m, &p, AR, &lda, BR, &ldb, CR, &ldc, DR, 
            &ldd, NR, INDEX, DCOEFF, &lddcoe, UCOEFF, &lduc01, &lduc02,
            &tol1, &tol2, IWORK, DWORK, &ldwork, INFO)
    
    #Check INFO for error value:
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04AD: the 
        %dth argument had an illegal value", -INFO[1])))
    else
        return (AR[:NR, :NR], BR[:NR, :m], CR[:p, :NR], NR[1], INDEX, 
        DCOEFF[:porm, :kdcoef], UCOEFF[:porm, :porp, :kdcoef], INFO[1])
    end
end

#TODO: Finish td04ad
#
#function td04ad(rowcol::Char, m::Integer, p::Integer, index::Vector{Integer},
#                dcoeff::Array{Float64, 2}, ucoeff::Array{Float64, 3},
#                tol=0.0, ldwork=-1)
#    ## Convert a transfer function or matrix of transfer functions to a ##
#    ## minimum state space realization.                                 ##
#
#    #COPIED FROM SLICOT: TRANSLATE
#    if ldwork < 0 
#        n = sum(index)
#        ldwork = max(1,n+max(n,max(3*m,3*p)))
#    end
#    
#    kdcoef = max(index)+1
#    if rowcol == 'R'
#        porm = p
#        if ndim(ucoeff) != 3:
#            error("The numerator is not a 3D array!")
#        elseif size(ucoeff) != (max(1,p), max(1,m), kdcoef)
#            error("The numerator shape is $(size(ucoeff)), but expected $(max(1,p)), $(max(1,m)),$kdcoef")
#        elseif size(dcoeff) != (max(1,p), kdcoef)
#            error("The denominator shape is $(size(dcoeff)), but expected $(max(1,p)), $kdcoef")
#        end
#    elseif rowcol == 'C'
#        porm = m
#        if ndim(ucoeff) != 3:
#            error("The numerator is not a 3D array!")
#        elseif size(ucoeff) != (max(1,m,p), max(1,m,p), kdcoef)
#            error("The numerator shape is $(size(ucoeff)), but expected $(max(1,m,p)), $(max(1,m,p)),$kdcoef")
#        elseif size(dcoeff) != (max(1,m), kdcoef)
#            error("The denominator shape is $(size(dcoeff)), but expected $(max(1,m)), $kdcoef")
#        end
#    else
#        error("Parameter rowcol had an illegal value")
#    end
#
#    #TODO: Call subroutine here!
#
#    if INFO[1] < 0
#        error(@sprintf("SlicotError in TB04AD: The %dth argument had
#        an illegal value", -INFO[1]))
#    elseif INFO[1] > 0
#        error("The leading coefficient of a denominator polynomial is nearly
#               zero; calculations would overflow; no state-space representation
#               was calculated. ABS(DCOEFF[$(INFO[1]),1]) = $(abs(dcoeff[INFO[1],1])) is too small.")
#    end
#    return Nr, A[:Nr,:Nr], B[:Nr,:m], C[:p,:Nr], D[:p,:m] 
#end

end     #module

