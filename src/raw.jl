## Raw call strings for SLICOT routines ##
# For most cases you probably want to use Slicot.Simple

#Call signatures in this module are the same as they are in the original
#fortran, with 2 exceptions:
#
# 1.) INFO is not passed in. This is to ensure error handling is *always*
#     valid
#
# 2.) If a parameter is an output (or input/output) it must be an array.
#     Even if the parameter is an integer, it must be passed in wrapped
#     in a 1d array. For example, TD04AD takes NR, which is an integer. 
#     This must be passed in as [some number].

module Raw

import Slicot: SlicotException, BlasInt, libslicot

function ab01md!(JOBZ::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,1}, NCONT::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    TAU::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab01md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOBZ, &N, A,
            &LDA, B, &NCONT, Z, &LDZ, TAU, &TOL, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab01nd!(JOBZ::Char, N::Integer, M::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer, NCONT::Integer,
    INDCON::Integer, NBLK::Array{BlasInt,1},
    Z::Array{Float64,2}, LDZ::Integer,
    TAU::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab01nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOBZ, &N, &M, A, &LDA, B, &LDB, &NCONT, &INDCON, NBLK,
            Z, &LDZ, TAU, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab01od!(STAGES::Char, JOBU::Char, JOBV::Char, N::Integer,
    M::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    U::Array{Float64,2}, LDU::Integer,
    V::Array{Float64,2}, LDV::Integer, NCONT::Integer,
    INDCON::Integer, KSTAIR::Array{BlasInt,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab01od_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &STAGES,
            &JOBU, &JOBV, &N, &M, A, &LDA, B, &LDB, U, &LDU, V,
            &LDV, &NCONT, &INDCON, KSTAIR, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab04md!(TYPE::Char, N::Integer, M::Integer, P::Integer,
    ALPHA::FloatingPoint, BETA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab04md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TYPE, &N, &M, &P, &ALPHA, &BETA, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB04MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05md!(UPLO::Char, OVER::Char, N1::Integer, M1::Integer,
    P1::Integer, N2::Integer, P2::Integer, A1::Array{Float64,2},
    LDA1::Integer, B1::Array{Float64,2}, LDB1::Integer,
    C1::Array{Float64,2}, LDC1::Integer,
    D1::Array{Float64,2}, LDD1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B2::Array{Float64,2}, LDB2::Integer,
    C2::Array{Float64,2}, LDC2::Integer,
    D2::Array{Float64,2}, LDD2::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab05md_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &UPLO, &OVER, &N1, &M1, &P1, &N2, &P2,
            A1, &LDA1, B1, &LDB1, C1, &LDC1, D1, &LDD1, A2, &LDA2,
            B2, &LDB2, C2, &LDC2, D2, &LDD2, &N, A, &LDA, B, &LDB,
            C, &LDC, D, &LDD, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05nd!(OVER::Char, N1::Integer, M1::Integer, P1::Integer,
    N2::Integer, ALPHA::FloatingPoint, A1::Array{Float64,2},
    LDA1::Integer, B1::Array{Float64,2}, LDB1::Integer,
    C1::Array{Float64,2}, LDC1::Integer,
    D1::Array{Float64,2}, LDD1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B2::Array{Float64,2}, LDB2::Integer,
    C2::Array{Float64,2}, LDC2::Integer,
    D2::Array{Float64,2}, LDD2::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab05nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &OVER, &N1, &M1, &P1, &N2, &ALPHA, A1,
            &LDA1, B1, &LDB1, C1, &LDC1, D1, &LDD1, A2, &LDA2, B2,
            &LDB2, C2, &LDC2, D2, &LDD2, &N, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05od!(OVER::Char, N1::Integer, M1::Integer, P1::Integer,
    N2::Integer, M2::Integer, ALPHA::FloatingPoint,
    A1::Array{Float64,2}, LDA1::Integer,
    B1::Array{Float64,2}, LDB1::Integer,
    C1::Array{Float64,2}, LDC1::Integer,
    D1::Array{Float64,2}, LDD1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B2::Array{Float64,2}, LDB2::Integer,
    C2::Array{Float64,2}, LDC2::Integer,
    D2::Array{Float64,2}, LDD2::Integer, N::Integer, M::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer)

    INFO = [0]

    ccall((:ab05od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &OVER, &N1, &M1, &P1, &N2, &M2, &ALPHA, A1, &LDA1, B1,
            &LDB1, C1, &LDC1, D1, &LDD1, A2, &LDA2, B2, &LDB2, C2,
            &LDC2, D2, &LDD2, &N, &M, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05pd!(OVER::Char, N1::Integer, M::Integer, P::Integer,
    N2::Integer, ALPHA::FloatingPoint, A1::Array{Float64,2},
    LDA1::Integer, B1::Array{Float64,2}, LDB1::Integer,
    C1::Array{Float64,2}, LDC1::Integer,
    D1::Array{Float64,2}, LDD1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B2::Array{Float64,2}, LDB2::Integer,
    C2::Array{Float64,2}, LDC2::Integer,
    D2::Array{Float64,2}, LDD2::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer)

    INFO = [0]

    ccall((:ab05pd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &OVER, &N1, &M, &P, &N2,
            &ALPHA, A1, &LDA1, B1, &LDB1, C1, &LDC1, D1, &LDD1, A2,
            &LDA2, B2, &LDB2, C2, &LDC2, D2, &LDD2, &N, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05qd!(OVER::Char, N1::Integer, M1::Integer, P1::Integer,
    N2::Integer, M2::Integer, P2::Integer, A1::Array{Float64,2},
    LDA1::Integer, B1::Array{Float64,2}, LDB1::Integer,
    C1::Array{Float64,2}, LDC1::Integer,
    D1::Array{Float64,2}, LDD1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B2::Array{Float64,2}, LDB2::Integer,
    C2::Array{Float64,2}, LDC2::Integer,
    D2::Array{Float64,2}, LDD2::Integer, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer)

    INFO = [0]

    ccall((:ab05qd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &OVER, &N1, &M1, &P1, &N2, &M2, &P2, A1,
            &LDA1, B1, &LDB1, C1, &LDC1, D1, &LDD1, A2, &LDA2, B2,
            &LDB2, C2, &LDC2, D2, &LDD2, &N, &M, &P, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05rd!(FBTYPE::Char, JOBD::Char, N::Integer, M::Integer,
    P::Integer, MV::Integer, PZ::Integer, ALPHA::FloatingPoint,
    BETA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    F::Array{Float64,2}, LDF::Integer,
    K::Array{Float64,2}, LDK::Integer,
    G::Array{Float64,2}, LDG::Integer,
    H::Array{Float64,2}, LDH::Integer, RCOND::FloatingPoint,
    BC::Array{Float64,2}, LDBC::Integer,
    CC::Array{Float64,2}, LDCC::Integer,
    DC::Array{Float64,2}, LDDC::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab05rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &FBTYPE, &JOBD, &N, &M, &P,
            &MV, &PZ, &ALPHA, &BETA, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, F, &LDF, K, &LDK, G, &LDG, H, &LDH, &RCOND, BC,
            &LDBC, CC, &LDCC, DC, &LDDC, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab05sd!(FBTYPE::Char, JOBD::Char, N::Integer, M::Integer,
    P::Integer, ALPHA::FloatingPoint, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    F::Array{Float64,2}, LDF::Integer, RCOND::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab05sd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &FBTYPE,
            &JOBD, &N, &M, &P, &ALPHA, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, F, &LDF, &RCOND, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB05SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab07md!(JOBD::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer)

    INFO = [0]

    ccall((:ab07md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOBD, &N,
            &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB07MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab07nd!(N::Integer, M::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, RCOND::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab07nd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &M, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, &RCOND, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB07ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab08md!(EQUIL::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, RANK::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab08md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &EQUIL, &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD,
            &RANK, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB08MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab08mz!(EQUIL::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Complex,2}, LDA::Integer, B::Array{Complex,2},
    LDB::Integer, C::Array{Complex,2}, LDC::Integer,
    D::Array{Complex,2}, LDD::Integer, RANK::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1},
    LZWORK::Integer)

    INFO = [0]

    ccall((:ab08mz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{BlasInt}), &EQUIL, &N, &M, &P, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, &RANK, &TOL, IWORK, DWORK, ZWORK,
            &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB08MZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab08nd!(EQUIL::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NU::Integer,
    RANK::Integer, DINFZ::Integer, NKROR::Integer, NKROL::Integer,
    INFZ::Array{BlasInt,1}, KRONR::Array{BlasInt,1},
    KRONL::Array{BlasInt,1}, AF::Array{Float64,2},
    LDAF::Integer, BF::Array{Float64,2}, LDBF::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab08nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &EQUIL, &N,
            &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, &NU, &RANK,
            &DINFZ, &NKROR, &NKROL, INFZ, KRONR, KRONL, AF, &LDAF,
            BF, &LDBF, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB08ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab08nx!(N::Integer, M::Integer, P::Integer, RO::Integer,
    SIGMA::Integer, SVLMAX::FloatingPoint,
    ABCD::Array{Float64,2}, LDABCD::Integer, NINFZ::Integer,
    INFZ::Array{BlasInt,1}, KRONL::Array{BlasInt,1}, MU::Integer,
    NU::Integer, NKROL::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab08nx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &M, &P, &RO, &SIGMA, &SVLMAX, ABCD,
            &LDABCD, &NINFZ, INFZ, KRONL, &MU, &NU, &NKROL, &TOL,
            IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB08NX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab08nz!(EQUIL::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Complex,2}, LDA::Integer, B::Array{Complex,2},
    LDB::Integer, C::Array{Complex,2}, LDC::Integer,
    D::Array{Complex,2}, LDD::Integer, NU::Integer, RANK::Integer,
    DINFZ::Integer, NKROR::Integer, NKROL::Integer,
    INFZ::Array{BlasInt,1}, KRONR::Array{BlasInt,1},
    KRONL::Array{BlasInt,1}, AF::Array{Complex,2}, LDAF::Integer,
    BF::Array{Complex,2}, LDBF::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    ZWORK::Array{Complex,1}, LZWORK::Integer)

    INFO = [0]

    ccall((:ab08nz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}), &EQUIL, &N, &M, &P, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, &NU, &RANK, &DINFZ,
            &NKROR, &NKROL, INFZ, KRONR, KRONL, AF, &LDAF, BF,
            &LDBF, &TOL, IWORK, DWORK, ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB08NZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09ad!(DICO::Char, JOB::Char, EQUIL::Char, ORDSEL::Char,
    N::Integer, M::Integer, P::Integer, NR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    HSV::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOB,
            &EQUIL, &ORDSEL, &N, &M, &P, &NR, A, &LDA, B, &LDB, C,
            &LDC, HSV, &TOL, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09ax!(DICO::Char, JOB::Char, ORDSEL::Char, N::Integer,
    M::Integer, P::Integer, NR::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    HSV::Array{Float64,1}, T::Array{Float64,2},
    LDT::Integer, TI::Array{Float64,2}, LDTI::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09ax_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOB, &ORDSEL, &N,
            &M, &P, &NR, A, &LDA, B, &LDB, C, &LDC, HSV, T, &LDT,
            TI, &LDTI, &TOL, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09AX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09bd!(DICO::Char, JOB::Char, EQUIL::Char, ORDSEL::Char,
    N::Integer, M::Integer, P::Integer, NR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOB, &EQUIL,
            &ORDSEL, &N, &M, &P, &NR, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, HSV, &TOL1, &TOL2, IWORK, DWORK, &LDWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09bx!(DICO::Char, JOB::Char, ORDSEL::Char, N::Integer,
    M::Integer, P::Integer, NR::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    HSV::Array{Float64,1}, T::Array{Float64,2},
    LDT::Integer, TI::Array{Float64,2}, LDTI::Integer,
    TOL1::FloatingPoint, TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09bx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &JOB, &ORDSEL, &N, &M, &P, &NR, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, HSV, T, &LDT, TI,
            &LDTI, &TOL1, &TOL2, IWORK, DWORK, &LDWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09BX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09cd!(DICO::Char, EQUIL::Char, ORDSEL::Char, N::Integer,
    M::Integer, P::Integer, NR::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09cd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &EQUIL, &ORDSEL, &N, &M, &P, &NR,
            A, &LDA, B, &LDB, C, &LDC, D, &LDD, HSV, &TOL1, &TOL2,
            IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09cx!(DICO::Char, ORDSEL::Char, N::Integer, M::Integer,
    P::Integer, NR::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09cx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &ORDSEL, &N, &M, &P, &NR, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, HSV, &TOL1, &TOL2, IWORK, DWORK, &LDWORK,
            &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09CX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09dd!(DICO::Char, N::Integer, M::Integer, P::Integer,
    NR::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, RCOND::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:ab09dd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &DICO, &N,
            &M, &P, &NR, A, &LDA, B, &LDB, C, &LDC, D, &LDD, &RCOND,
            IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09ed!(DICO::Char, EQUIL::Char, ORDSEL::Char, N::Integer,
    M::Integer, P::Integer, NR::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09ed_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO,
            &EQUIL, &ORDSEL, &N, &M, &P, &NR, &ALPHA, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, &NS, HSV, &TOL1, &TOL2, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09fd!(DICO::Char, JOBCF::Char, FACT::Char, JOBMR::Char,
    EQUIL::Char, ORDSEL::Char, N::Integer, M::Integer, P::Integer,
    NR::Integer, ALPHA::FloatingPoint, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NQ::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09fd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &JOBCF, &FACT, &JOBMR, &EQUIL, &ORDSEL, &N, &M,
            &P, &NR, &ALPHA, A, &LDA, B, &LDB, C, &LDC, &NQ, HSV,
            &TOL1, &TOL2, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09gd!(DICO::Char, JOBCF::Char, FACT::Char, JOBMR::Char,
    EQUIL::Char, ORDSEL::Char, N::Integer, M::Integer, P::Integer,
    NR::Integer, ALPHA::FloatingPoint, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NQ::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, TOL3::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09gd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO,
            &JOBCF, &FACT, &JOBMR, &EQUIL, &ORDSEL, &N, &M, &P, &NR,
            &ALPHA, A, &LDA, B, &LDB, C, &LDC, D, &LDD, &NQ, HSV,
            &TOL1, &TOL2, &TOL3, IWORK, DWORK, &LDWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09GD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09hd!(DICO::Char, JOB::Char, EQUIL::Char, ORDSEL::Char,
    N::Integer, M::Integer, P::Integer, NR::Integer,
    ALPHA::FloatingPoint, BETA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1}, IWARN::Integer)

    INFO = [0]

    ccall((:ab09hd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOB, &EQUIL,
            &ORDSEL, &N, &M, &P, &NR, &ALPHA, &BETA, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, &NS, HSV, &TOL1, &TOL2, IWORK,
            DWORK, &LDWORK, BWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09hx!(DICO::Char, JOB::Char, ORDSEL::Char, N::Integer,
    M::Integer, P::Integer, NR::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    HSV::Array{Float64,1}, T::Array{Float64,2},
    LDT::Integer, TI::Array{Float64,2}, LDTI::Integer,
    TOL1::FloatingPoint, TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1}, IWARN::Integer)

    INFO = [0]

    ccall((:ab09hx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOB, &ORDSEL, &N,
            &M, &P, &NR, A, &LDA, B, &LDB, C, &LDC, D, &LDD, HSV, T,
            &LDT, TI, &LDTI, &TOL1, &TOL2, IWORK, DWORK, &LDWORK,
            BWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09HX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09hy!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, SCALEC::FloatingPoint,
    SCALEO::FloatingPoint, S::Array{Float64,2}, LDS::Integer,
    R::Array{Float64,2}, LDR::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:ab09hy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}),
            &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, &SCALEC,
            &SCALEO, S, &LDS, R, &LDR, IWORK, DWORK, &LDWORK, BWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09HY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09id!(DICO::Char, JOBC::Char, JOBO::Char, JOB::Char,
    WEIGHT::Char, EQUIL::Char, ORDSEL::Char, N::Integer, M::Integer,
    P::Integer, NV::Integer, PV::Integer, NW::Integer, MW::Integer,
    NR::Integer, ALPHA::FloatingPoint, ALPHAC::FloatingPoint,
    ALPHAO::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AV::Array{Float64,2}, LDAV::Integer,
    BV::Array{Float64,2}, LDBV::Integer,
    CV::Array{Float64,2}, LDCV::Integer,
    DV::Array{Float64,2}, LDDV::Integer,
    AW::Array{Float64,2}, LDAW::Integer,
    BW::Array{Float64,2}, LDBW::Integer,
    CW::Array{Float64,2}, LDCW::Integer,
    DW::Array{Float64,2}, LDDW::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09id_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &JOBC, &JOBO, &JOB, &WEIGHT, &EQUIL, &ORDSEL, &N,
            &M, &P, &NV, &PV, &NW, &MW, &NR, &ALPHA, &ALPHAC,
            &ALPHAO, A, &LDA, B, &LDB, C, &LDC, D, &LDD, AV, &LDAV,
            BV, &LDBV, CV, &LDCV, DV, &LDDV, AW, &LDAW, BW, &LDBW,
            CW, &LDCW, DW, &LDDW, &NS, HSV, &TOL1, &TOL2, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09ix!(DICO::Char, JOB::Char, FACT::Char, ORDSEL::Char,
    N::Integer, M::Integer, P::Integer, NR::Integer,
    SCALEC::FloatingPoint, SCALEO::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    TI::Array{Float64,2}, LDTI::Integer,
    T::Array{Float64,2}, LDT::Integer, NMINR::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09ix_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &JOB, &FACT, &ORDSEL, &N, &M, &P,
            &NR, &SCALEC, &SCALEO, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, TI, &LDTI, T, &LDT, &NMINR, HSV, &TOL1, &TOL2,
            IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09IX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09iy!(DICO::Char, JOBC::Char, JOBO::Char, WEIGHT::Char,
    N::Integer, M::Integer, P::Integer, NV::Integer, PV::Integer,
    NW::Integer, MW::Integer, ALPHAC::FloatingPoint,
    ALPHAO::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    AV::Array{Float64,2}, LDAV::Integer,
    BV::Array{Float64,2}, LDBV::Integer,
    CV::Array{Float64,2}, LDCV::Integer,
    DV::Array{Float64,2}, LDDV::Integer,
    AW::Array{Float64,2}, LDAW::Integer,
    BW::Array{Float64,2}, LDBW::Integer,
    CW::Array{Float64,2}, LDCW::Integer,
    DW::Array{Float64,2}, LDDW::Integer, SCALEC::FloatingPoint,
    SCALEO::FloatingPoint, S::Array{Float64,2}, LDS::Integer,
    R::Array{Float64,2}, LDR::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab09iy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOBC, &JOBO,
            &WEIGHT, &N, &M, &P, &NV, &PV, &NW, &MW, &ALPHAC,
            &ALPHAO, A, &LDA, B, &LDB, C, &LDC, AV, &LDAV, BV,
            &LDBV, CV, &LDCV, DV, &LDDV, AW, &LDAW, BW, &LDBW, CW,
            &LDCW, DW, &LDDW, &SCALEC, &SCALEO, S, &LDS, R, &LDR,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09IY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09jd!(JOBV::Char, JOBW::Char, JOBINV::Char, DICO::Char,
    EQUIL::Char, ORDSEL::Char, N::Integer, NV::Integer, NW::Integer,
    M::Integer, P::Integer, NR::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AV::Array{Float64,2}, LDAV::Integer,
    BV::Array{Float64,2}, LDBV::Integer,
    CV::Array{Float64,2}, LDCV::Integer,
    DV::Array{Float64,2}, LDDV::Integer,
    AW::Array{Float64,2}, LDAW::Integer,
    BW::Array{Float64,2}, LDBW::Integer,
    CW::Array{Float64,2}, LDCW::Integer,
    DW::Array{Float64,2}, LDDW::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09jd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOBV, &JOBW, &JOBINV, &DICO, &EQUIL, &ORDSEL, &N, &NV,
            &NW, &M, &P, &NR, &ALPHA, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, AV, &LDAV, BV, &LDBV, CV, &LDCV, DV, &LDDV, AW,
            &LDAW, BW, &LDBW, CW, &LDCW, DW, &LDDW, &NS, HSV, &TOL1,
            &TOL2, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09JD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09jv!(JOB::Char, DICO::Char, JOBEV::Char, STBCHK::Char,
    N::Integer, M::Integer, P::Integer, NV::Integer, PV::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AV::Array{Float64,2}, LDAV::Integer,
    EV::Array{Float64,2}, LDEV::Integer,
    BV::Array{Float64,2}, LDBV::Integer,
    CV::Array{Float64,2}, LDCV::Integer,
    DV::Array{Float64,2}, LDDV::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab09jv_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOB, &DICO, &JOBEV, &STBCHK, &N, &M, &P,
            &NV, &PV, A, &LDA, B, &LDB, C, &LDC, D, &LDD, AV, &LDAV,
            EV, &LDEV, BV, &LDBV, CV, &LDCV, DV, &LDDV, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09JV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09jw!(JOB::Char, DICO::Char, JOBEW::Char, STBCHK::Char,
    N::Integer, M::Integer, P::Integer, NW::Integer, MW::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AW::Array{Float64,2}, LDAW::Integer,
    EW::Array{Float64,2}, LDEW::Integer,
    BW::Array{Float64,2}, LDBW::Integer,
    CW::Array{Float64,2}, LDCW::Integer,
    DW::Array{Float64,2}, LDDW::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ab09jw_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOB, &DICO, &JOBEW, &STBCHK, &N, &M, &P,
            &NW, &MW, A, &LDA, B, &LDB, C, &LDC, D, &LDD, AW, &LDAW,
            EW, &LDEW, BW, &LDBW, CW, &LDCW, DW, &LDDW, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09JW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09jx!(DICO::Char, STDOM::Char, EVTYPE::Char, N::Integer,
    ALPHA::FloatingPoint, ER::Array{Float64,1},
    EI::Array{Float64,1}, ED::Array{Float64,1},
    TOLINF::FloatingPoint)

    INFO = [0]

    ccall((:ab09jx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &DICO, &STDOM, &EVTYPE, &N, &ALPHA, ER, EI, ED, &TOLINF,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09JX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09kd!(JOB::Char, DICO::Char, WEIGHT::Char, EQUIL::Char,
    ORDSEL::Char, N::Integer, NV::Integer, NW::Integer, M::Integer,
    P::Integer, NR::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AV::Array{Float64,2}, LDAV::Integer,
    BV::Array{Float64,2}, LDBV::Integer,
    CV::Array{Float64,2}, LDCV::Integer,
    DV::Array{Float64,2}, LDDV::Integer,
    AW::Array{Float64,2}, LDAW::Integer,
    BW::Array{Float64,2}, LDBW::Integer,
    CW::Array{Float64,2}, LDCW::Integer,
    DW::Array{Float64,2}, LDDW::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09kd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &DICO,
            &WEIGHT, &EQUIL, &ORDSEL, &N, &NV, &NW, &M, &P, &NR,
            &ALPHA, A, &LDA, B, &LDB, C, &LDC, D, &LDD, AV, &LDAV,
            BV, &LDBV, CV, &LDCV, DV, &LDDV, AW, &LDAW, BW, &LDBW,
            CW, &LDCW, DW, &LDDW, &NS, HSV, &TOL1, &TOL2, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09kx!(JOB::Char, DICO::Char, WEIGHT::Char, N::Integer,
    NV::Integer, NW::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AV::Array{Float64,2}, LDAV::Integer,
    BV::Array{Float64,2}, LDBV::Integer,
    CV::Array{Float64,2}, LDCV::Integer,
    DV::Array{Float64,2}, LDDV::Integer,
    AW::Array{Float64,2}, LDAW::Integer,
    BW::Array{Float64,2}, LDBW::Integer,
    CW::Array{Float64,2}, LDCW::Integer,
    DW::Array{Float64,2}, LDDW::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09kx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &DICO, &WEIGHT, &N,
            &NV, &NW, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD,
            AV, &LDAV, BV, &LDBV, CV, &LDCV, DV, &LDDV, AW, &LDAW,
            BW, &LDBW, CW, &LDCW, DW, &LDDW, DWORK, &LDWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09KX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09md!(DICO::Char, JOB::Char, EQUIL::Char, ORDSEL::Char,
    N::Integer, M::Integer, P::Integer, NR::Integer,
    ALPHA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09md_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &JOB, &EQUIL, &ORDSEL, &N, &M, &P,
            &NR, &ALPHA, A, &LDA, B, &LDB, C, &LDC, &NS, HSV, &TOL,
            IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab09nd!(DICO::Char, JOB::Char, EQUIL::Char, ORDSEL::Char,
    N::Integer, M::Integer, P::Integer, NR::Integer,
    ALPHA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NS::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ab09nd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &JOB, &EQUIL, &ORDSEL, &N, &M, &P, &NR, &ALPHA,
            A, &LDA, B, &LDB, C, &LDC, D, &LDD, &NS, HSV, &TOL1,
            &TOL2, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB09ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab13dd!(DICO::Char, JOBE::Char, EQUIL::Char, JOBD::Char,
    N::Integer, M::Integer, P::Integer, FPEAK::Array{Float64,1},
    A::Array{Float64,1}, LDA::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    GPEAK::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, CWORK::Array{Complex,1}, LCWORK::Integer)

    INFO = [0]

    ccall((:ab13dd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &JOBE, &EQUIL, &JOBD, &N, &M, &P,
            FPEAK, A, &LDA, E, &LDE, B, &LDB, C, &LDC, D, &LDD,
            GPEAK, &TOL, IWORK, DWORK, &LDWORK, CWORK, &LCWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB13DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab13ed!(N::Integer, A::Array{Float64,2}, LDA::Integer,
    LOW::FloatingPoint, HIGH::FloatingPoint, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:ab13ed_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, A, &LDA,
            &LOW, &HIGH, &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB13ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab13fd!(N::Integer, A::Array{Float64,2}, LDA::Integer,
    BETA::FloatingPoint, OMEGA::FloatingPoint, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer,
    CWORK::Array{Complex,1}, LCWORK::Integer)

    INFO = [0]

    ccall((:ab13fd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, A, &LDA, &BETA, &OMEGA,
            &TOL, DWORK, &LDWORK, CWORK, &LCWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB13FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab13md!(FACT::Char, N::Integer, Z::Array{Complex,1},
    LDZ::Complex, M::Integer, NBLOCK::Array{BlasInt,1},
    ITYPE::Array{BlasInt,1}, X::Array{Float64,1},
    BOUND::FloatingPoint, D::Array{Float64,1},
    G::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    ZWORK::Array{Complex,1}, LZWORK::Integer)

    INFO = [0]

    ccall((:ab13md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{BlasInt}), &FACT, &N, Z, &LDZ, &M, NBLOCK, ITYPE, X,
            &BOUND, D, G, IWORK, DWORK, &LDWORK, ZWORK, &LZWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB13MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ab8nxz!(N::Integer, M::Integer, P::Integer, RO::Integer,
    SIGMA::Integer, SVLMAX::FloatingPoint, ABCD::Array{Complex,2},
    LDABCD::Integer, NINFZ::Integer, INFZ::Array{BlasInt,1},
    KRONL::Array{BlasInt,1}, MU::Integer, NU::Integer, NKROL::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1},
    LZWORK::Integer)

    INFO = [0]

    ccall((:ab8nxz_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &M,
            &P, &RO, &SIGMA, &SVLMAX, ABCD, &LDABCD, &NINFZ, INFZ,
            KRONL, &MU, &NU, &NKROL, &TOL, IWORK, DWORK, ZWORK,
            &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AB8NXZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ag07bd!(JOBE::Char, N::Integer, M::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AI::Array{Float64,2}, LDAI::Integer,
    EI::Array{Float64,2}, LDEI::Integer,
    BI::Array{Float64,2}, LDBI::Integer,
    CI::Array{Float64,2}, LDCI::Integer,
    DI::Array{Float64,2}, LDDI::Integer)

    INFO = [0]

    ccall((:ag07bd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBE, &N, &M, A, &LDA, E,
            &LDE, B, &LDB, C, &LDC, D, &LDD, AI, &LDAI, EI, &LDEI,
            BI, &LDBI, CI, &LDCI, DI, &LDDI, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AG07BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ag08bd!(EQUIL::Char, L::Integer, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NFZ::Integer,
    NRANK::Integer, NIZ::Integer, DINFZ::Integer, NKROR::Integer,
    NINFE::Integer, NKROL::Integer, INFZ::Array{BlasInt,1},
    KRONR::Array{BlasInt,1}, INFE::Array{BlasInt,1},
    KRONL::Array{BlasInt,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ag08bd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &EQUIL, &L, &N, &M, &P, A, &LDA, E, &LDE,
            B, &LDB, C, &LDC, D, &LDD, &NFZ, &NRANK, &NIZ, &DINFZ,
            &NKROR, &NINFE, &NKROL, INFZ, KRONR, INFE, KRONL, &TOL,
            IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AG08BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ag08by!(FIRST::Bool, N::Integer, M::Integer, P::Integer,
    SVLMAX::FloatingPoint, ABCD::Array{Float64,1},
    LDABCD::FloatingPoint, E::Array{Float64,1},
    LDE::FloatingPoint, NR::Integer, PR::Integer, NINFZ::Integer,
    DINFZ::Integer, NKRONL::Integer, INFZ::Array{BlasInt,1},
    KRONL::Array{BlasInt,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:ag08by_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &FIRST, &N,
            &M, &P, &SVLMAX, ABCD, &LDABCD, E, &LDE, &NR, &PR,
            &NINFZ, &DINFZ, &NKRONL, INFZ, KRONL, &TOL, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AG08BY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ag08bz!(EQUIL::Char, L::Integer, N::Integer, M::Integer,
    P::Integer, A::Array{Complex,2}, LDA::Integer,
    E::Array{Complex,2}, LDE::Integer, B::Array{Complex,2},
    LDB::Integer, C::Array{Complex,2}, LDC::Integer,
    D::Array{Complex,2}, LDD::Integer, NFZ::Integer, NRANK::Integer,
    NIZ::Integer, DINFZ::Integer, NKROR::Integer, NINFE::Integer,
    NKROL::Integer, INFZ::Array{BlasInt,1}, KRONR::Array{BlasInt,1},
    INFE::Array{BlasInt,1}, KRONL::Array{BlasInt,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1},
    LZWORK::Integer)

    INFO = [0]

    ccall((:ag08bz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt}), &EQUIL,
            &L, &N, &M, &P, A, &LDA, E, &LDE, B, &LDB, C, &LDC, D,
            &LDD, &NFZ, &NRANK, &NIZ, &DINFZ, &NKROR, &NINFE,
            &NKROL, INFZ, KRONR, INFE, KRONL, &TOL, IWORK, DWORK,
            ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AG08BZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ag8byz!(FIRST::Bool, N::Integer, M::Integer, P::Integer,
    SVLMAX::FloatingPoint, ABCD::Array{Complex,1}, LDABCD::Complex,
    E::Array{Complex,1}, LDE::Complex, NR::Integer, PR::Integer,
    NINFZ::Integer, DINFZ::Integer, NKRONL::Integer,
    INFZ::Array{BlasInt,1}, KRONL::Array{BlasInt,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1},
    LZWORK::Integer)

    INFO = [0]

    ccall((:ag8byz_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt}), &FIRST,
            &N, &M, &P, &SVLMAX, ABCD, &LDABCD, E, &LDE, &NR, &PR,
            &NINFZ, &DINFZ, &NKRONL, INFZ, KRONL, &TOL, IWORK,
            DWORK, ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in AG8BYZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function de01od!(CONV::Char, N::Integer, A::Array{Float64,1},
    B::Array{Float64,1})

    INFO = [0]

    ccall((:de01od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &CONV, &N, A,
            B, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DE01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function de01pd!(CONV::Char, WGHT::Char, N::Integer,
    A::Array{Float64,1}, B::Array{Float64,1},
    W::Array{Float64,1})

    INFO = [0]

    ccall((:de01pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &CONV, &WGHT, &N, A, B, W, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DE01PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function df01md!(SICO::Char, N::Integer, DT::FloatingPoint,
    A::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:df01md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &SICO, &N, &DT, A, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DF01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function dg01md!(INDI::Char, N::Integer, XR::Array{Float64,1},
    XI::Array{Float64,1})

    INFO = [0]

    ccall((:dg01md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &INDI, &N,
            XR, XI, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DG01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function dg01nd!(INDI::Char, N::Integer, XR::Array{Float64,1},
    XI::Array{Float64,1})

    INFO = [0]

    ccall((:dg01nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &INDI, &N,
            XR, XI, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DG01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function dg01ny!(INDI::Char, N::Integer, XR::Array{Float64,1},
    XI::Array{Float64,1})

    INFO = [0]

    ccall((:dg01ny_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}), &INDI, &N, XR, XI)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DG01NY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function dg01od!(SCR::Char, WGHT::Char, N::Integer,
    A::Array{Float64,1}, W::Array{Float64,1})

    INFO = [0]

    ccall((:dg01od_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &SCR, &WGHT, &N, A, W, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DG01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function dk01md!(TYPE::Char, N::Integer, A::Array{Float64,1})

    INFO = [0]

    ccall((:dk01md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &TYPE, &N, A, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in DK01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function fb01qd!(JOBK::Char, MULTBQ::Char, N::Integer, M::Integer,
    P::Integer, S::Array{Float64,2}, LDS::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    C::Array{Float64,2}, LDC::Integer,
    R::Array{Float64,2}, LDR::Integer,
    K::Array{Float64,2}, LDK::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:fb01qd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBK, &MULTBQ, &N, &M, &P,
            S, &LDS, A, &LDA, B, &LDB, Q, &LDQ, C, &LDC, R, &LDR, K,
            &LDK, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in FB01QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function fb01rd!(JOBK::Char, MULTBQ::Char, N::Integer, M::Integer,
    P::Integer, S::Array{Float64,2}, LDS::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    C::Array{Float64,2}, LDC::Integer,
    R::Array{Float64,2}, LDR::Integer,
    K::Array{Float64,2}, LDK::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:fb01rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBK, &MULTBQ, &N, &M, &P,
            S, &LDS, A, &LDA, B, &LDB, Q, &LDQ, C, &LDC, R, &LDR, K,
            &LDK, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in FB01RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function fb01sd!(JOBX::Char, MULTAB::Char, MULTRC::Char, N::Integer,
    M::Integer, P::Integer, SINV::Array{Float64,2},
    LDSINV::Integer, AINV::Array{Float64,2}, LDAINV::Integer,
    B::Array{Float64,2}, LDB::Integer,
    RINV::Array{Float64,2}, LDRINV::Integer,
    C::Array{Float64,2}, LDC::Integer,
    QINV::Array{Float64,2}, LDQINV::Integer,
    X::Array{Float64,1}, RINVY::Array{Float64,1},
    Z::Array{Float64,1}, E::Array{Float64,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:fb01sd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOBX, &MULTAB, &MULTRC, &N, &M, &P,
            SINV, &LDSINV, AINV, &LDAINV, B, &LDB, RINV, &LDRINV, C,
            &LDC, QINV, &LDQINV, X, RINVY, Z, E, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in FB01SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function fb01td!(JOBX::Char, MULTRC::Char, N::Integer, M::Integer,
    P::Integer, SINV::Array{Float64,2}, LDSINV::Integer,
    AINV::Array{Float64,2}, LDAINV::Integer,
    AINVB::Array{Float64,2}, LDAINB::Integer,
    RINV::Array{Float64,2}, LDRINV::Integer,
    C::Array{Float64,2}, LDC::Integer,
    QINV::Array{Float64,2}, LDQINV::Integer,
    X::Array{Float64,1}, RINVY::Array{Float64,1},
    Z::Array{Float64,1}, E::Array{Float64,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:fb01td_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOBX, &MULTRC, &N, &M, &P, SINV, &LDSINV, AINV,
            &LDAINV, AINVB, &LDAINB, RINV, &LDRINV, C, &LDC, QINV,
            &LDQINV, X, RINVY, Z, E, &TOL, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in FB01TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function fb01vd!(N::Integer, M::Integer, L::Integer,
    P::Array{Float64,2}, LDP::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer,
    K::Array{Float64,2}, LDK::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:fb01vd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, &M, &L, P, &LDP, A, &LDA, B, &LDB, C, &LDC, Q, &LDQ,
            R, &LDR, K, &LDK, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in FB01VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function fd01ad!(JP::Char, L::Integer, LAMBDA::FloatingPoint,
    XIN::FloatingPoint, YIN::FloatingPoint, EFOR::FloatingPoint,
    XF::Array{Float64,1}, EPSBCK::Array{Float64,1},
    CTETA::Array{Float64,1}, STETA::Array{Float64,1},
    YQ::Array{Float64,1}, EPOS::FloatingPoint,
    EOUT::FloatingPoint, SALPH::Array{Float64,1},
    IWARN::Integer)

    INFO = [0]

    ccall((:fd01ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JP, &L, &LAMBDA, &XIN,
            &YIN, &EFOR, XF, EPSBCK, CTETA, STETA, YQ, &EPOS, &EOUT,
            SALPH, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in FD01AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01ad!(METH::Char, ALG::Char, JOBD::Char, BATCH::Char,
    CONCT::Char, CTRL::Char, NOBR::Integer, M::Integer, L::Integer,
    NSMP::Integer, U::Array{Float64,1}, LDU::Integer,
    Y::Array{Float64,1}, LDY::Integer, N::Integer,
    R::Array{Float64,1}, LDR::Integer,
    SV::Array{Float64,1}, RCOND::FloatingPoint,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &METH, &ALG,
            &JOBD, &BATCH, &CONCT, &CTRL, &NOBR, &M, &L, &NSMP, U,
            &LDU, Y, &LDY, &N, R, &LDR, SV, &RCOND, &TOL, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01bd!(METH::Char, JOB::Char, JOBCK::Char, NOBR::Integer,
    N::Integer, M::Integer, L::Integer, NSMPL::Integer,
    R::Array{Float64,1}, LDR::Integer,
    A::Array{Float64,1}, LDA::Integer,
    C::Array{Float64,1}, LDC::Integer,
    B::Array{Float64,1}, LDB::Integer,
    D::Array{Float64,1}, LDD::Integer,
    Q::Array{Float64,1}, LDQ::Integer,
    RY::Array{Float64,1}, LDRY::Integer,
    S::Array{Float64,1}, LDS::Integer,
    K::Array{Float64,1}, LDK::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1}, IWARN::Integer)

    INFO = [0]

    ccall((:ib01bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}), &METH, &JOB,
            &JOBCK, &NOBR, &N, &M, &L, &NSMPL, R, &LDR, A, &LDA, C,
            &LDC, B, &LDB, D, &LDD, Q, &LDQ, RY, &LDRY, S, &LDS, K,
            &LDK, &TOL, IWORK, DWORK, &LDWORK, BWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01cd!(JOBX0::Char, COMUSE::Char, JOB::Char, N::Integer,
    M::Integer, L::Integer, NSMP::Integer, A::Array{Float64,1},
    LDA::Integer, B::Array{Float64,1}, LDB::Integer,
    C::Array{Float64,1}, LDC::Integer,
    D::Array{Float64,1}, LDD::Integer,
    U::Array{Float64,1}, LDU::Integer,
    Y::Array{Float64,1}, LDY::Integer,
    X0::Array{Float64,1}, V::Array{Float64,1},
    LDV::Integer, TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01cd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBX0, &COMUSE, &JOB, &N,
            &M, &L, &NSMP, A, &LDA, B, &LDB, C, &LDC, D, &LDD, U,
            &LDU, Y, &LDY, X0, V, &LDV, &TOL, IWORK, DWORK, &LDWORK,
            &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01md!(METH::Char, ALG::Char, BATCH::Char, CONCT::Char,
    NOBR::Integer, M::Integer, L::Integer, NSMP::Integer,
    U::Array{Float64,1}, LDU::Integer,
    Y::Array{Float64,1}, LDY::Integer,
    R::Array{Float64,1}, LDR::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01md_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &METH, &ALG, &BATCH, &CONCT, &NOBR, &M,
            &L, &NSMP, U, &LDU, Y, &LDY, R, &LDR, IWORK, DWORK,
            &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01my!(METH::Char, BATCH::Char, CONCT::Char, NOBR::Integer,
    M::Integer, L::Integer, NSMP::Integer, U::Array{Float64,1},
    LDU::Integer, Y::Array{Float64,1}, LDY::Integer,
    R::Array{Float64,1}, LDR::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01my_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &METH, &BATCH, &CONCT, &NOBR, &M, &L, &NSMP, U, &LDU, Y,
            &LDY, R, &LDR, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01MY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01nd!(METH::Char, JOBD::Char, NOBR::Integer, M::Integer,
    L::Integer, R::Array{Float64,1}, LDR::Integer,
    SV::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01nd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &METH, &JOBD, &NOBR, &M, &L, R, &LDR, SV, &TOL, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01od!(CTRL::Char, NOBR::Integer, L::Integer,
    SV::Array{Float64,1}, N::Integer, TOL::FloatingPoint,
    IWARN::Integer)

    INFO = [0]

    ccall((:ib01od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &CTRL, &NOBR, &L, SV, &N,
            &TOL, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01oy!(NS::Integer, NMAX::Integer, N::Integer,
    SV::Array{Float64,1})

    INFO = [0]

    ccall((:ib01oy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &NS, &NMAX,
            &N, SV, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01OY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01pd!(METH::Char, JOB::Char, JOBCV::Char, NOBR::Integer,
    N::Integer, M::Integer, L::Integer, NSMPL::Integer,
    R::Array{Float64,1}, LDR::Integer,
    A::Array{Float64,1}, LDA::Integer,
    C::Array{Float64,1}, LDC::Integer,
    B::Array{Float64,1}, LDB::Integer,
    D::Array{Float64,1}, LDD::Integer,
    Q::Array{Float64,1}, LDQ::Integer,
    RY::Array{Float64,1}, LDRY::Integer,
    S::Array{Float64,1}, LDS::Integer,
    O::Array{Float64,1}, LDO::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &METH, &JOB, &JOBCV, &NOBR,
            &N, &M, &L, &NSMPL, R, &LDR, A, &LDA, C, &LDC, B, &LDB,
            D, &LDD, Q, &LDQ, RY, &LDRY, S, &LDS, O, &LDO, &TOL,
            IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01px!(JOB::Char, NOBR::Integer, N::Integer, M::Integer,
    L::Integer, UF::Array{Float64,1}, LDUF::Integer,
    UN::Array{Float64,1}, LDUN::Integer,
    UL::Array{Float64,1}, LDUL::Integer,
    PGAL::Array{Float64,1}, LDPGAL::Integer,
    K::Array{Float64,1}, LDK::Integer,
    R::Array{Float64,1}, LDR::Integer,
    X::Array{Float64,1}, B::Array{Float64,1},
    LDB::Integer, D::Array{Float64,1}, LDD::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01px_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &NOBR, &N, &M, &L,
            UF, &LDUF, UN, &LDUN, UL, &LDUL, PGAL, &LDPGAL, K, &LDK,
            R, &LDR, X, B, &LDB, D, &LDD, &TOL, IWORK, DWORK,
            &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01PX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01py!(METH::Char, JOB::Char, NOBR::Integer, N::Integer,
    M::Integer, L::Integer, RANKR1::Integer,
    UL::Array{Float64,1}, LDUL::Integer,
    R1::Array{Float64,1}, LDR1::Integer,
    TAU1::Array{Float64,1}, PGAL::Array{Float64,1},
    LDPGAL::Integer, K::Array{Float64,1}, LDK::Integer,
    R::Array{Float64,1}, LDR::Integer,
    H::Array{Float64,1}, LDH::Integer,
    B::Array{Float64,1}, LDB::Integer,
    D::Array{Float64,1}, LDD::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01py_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &METH, &JOB, &NOBR, &N, &M, &L, &RANKR1, UL, &LDUL, R1,
            &LDR1, TAU1, PGAL, &LDPGAL, K, &LDK, R, &LDR, H, &LDH,
            B, &LDB, D, &LDD, &TOL, IWORK, DWORK, &LDWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01PY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01qd!(JOBX0::Char, JOB::Char, N::Integer, M::Integer,
    L::Integer, NSMP::Integer, A::Array{Float64,1},
    LDA::Integer, C::Array{Float64,1}, LDC::Integer,
    U::Array{Float64,1}, LDU::Integer,
    Y::Array{Float64,1}, LDY::Integer,
    X0::Array{Float64,1}, B::Array{Float64,1},
    LDB::Integer, D::Array{Float64,1}, LDD::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01qd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &JOBX0, &JOB,
            &N, &M, &L, &NSMP, A, &LDA, C, &LDC, U, &LDU, Y, &LDY,
            X0, B, &LDB, D, &LDD, &TOL, IWORK, DWORK, &LDWORK,
            &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib01rd!(JOB::Char, N::Integer, M::Integer, L::Integer,
    NSMP::Integer, A::Array{Float64,1}, LDA::Integer,
    B::Array{Float64,1}, LDB::Integer,
    C::Array{Float64,1}, LDC::Integer,
    D::Array{Float64,1}, LDD::Integer,
    U::Array{Float64,1}, LDU::Integer,
    Y::Array{Float64,1}, LDY::Integer,
    X0::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib01rd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &N, &M, &L, &NSMP, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, U, &LDU, Y, &LDY, X0,
            &TOL, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB01RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib03ad!(INIT::Char, ALG::Char, STOR::Char, NOBR::Integer,
    M::Integer, L::Integer, NSMP::Integer, N::Integer, NN::Integer,
    ITMAX1::Integer, ITMAX2::Integer, NPRINT::Integer,
    U::Array{Float64,1}, LDU::Integer,
    Y::Array{Float64,1}, LDY::Integer,
    X::Array{Float64,1}, LX::Integer, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib03ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &INIT, &ALG,
            &STOR, &NOBR, &M, &L, &NSMP, &N, &NN, &ITMAX1, &ITMAX2,
            &NPRINT, U, &LDU, Y, &LDY, X, &LX, &TOL1, &TOL2, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB03AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ib03bd!(INIT::Char, NOBR::Integer, M::Integer, L::Integer,
    NSMP::Integer, N::Integer, NN::Integer, ITMAX1::Integer,
    ITMAX2::Integer, NPRINT::Integer, U::Array{Float64,1},
    LDU::Integer, Y::Array{Float64,1}, LDY::Integer,
    X::Array{Float64,1}, LX::Integer, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:ib03bd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &INIT, &NOBR, &M, &L, &NSMP, &N, &NN,
            &ITMAX1, &ITMAX2, &NPRINT, U, &LDU, Y, &LDY, X, &LX,
            &TOL1, &TOL2, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in IB03BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma01ad!(XR::FloatingPoint, XI::FloatingPoint,
    YR::FloatingPoint, YI::FloatingPoint)

    INFO = [0]

    ccall((:ma01ad_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}), &XR, &XI, &YR, &YI)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA01AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma01bd!(BASE::FloatingPoint, LGBAS::FloatingPoint,
    K::Integer, S::Array{BlasInt,1}, A::Array{Float64,1},
    INCA::Integer, ALPHA::FloatingPoint, BETA::FloatingPoint,
    SCAL::Integer)

    INFO = [0]

    ccall((:ma01bd_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &BASE,
            &LGBAS, &K, S, A, &INCA, &ALPHA, &BETA, &SCAL)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA01BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02ad!(JOB::Char, M::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer)

    INFO = [0]

    ccall((:ma02ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &JOB, &M, &N, A, &LDA, B, &LDB)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02bd!(SIDE::Char, M::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer)

    INFO = [0]

    ccall((:ma02bd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &SIDE, &M,
            &N, A, &LDA)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02bz!(SIDE::Char, M::Integer, N::Integer,
    A::Array{Complex,2}, LDA::Integer)

    INFO = [0]

    ccall((:ma02bz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt}), &SIDE, &M,
            &N, A, &LDA)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02BZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02cd!(N::Integer, KL::Integer, KU::Integer,
    A::Array{Float64,2}, LDA::Integer)

    INFO = [0]

    ccall((:ma02cd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &N, &KL, &KU,
            A, &LDA)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02cz!(N::Integer, KL::Integer, KU::Integer,
    A::Array{Complex,2}, LDA::Integer)

    INFO = [0]

    ccall((:ma02cz_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt}), &N, &KL,
            &KU, A, &LDA)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02CZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02dd!(JOB::Char, UPLO::Char, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    AP::Array{Float64,1})

    INFO = [0]

    ccall((:ma02dd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}),
            &JOB, &UPLO, &N, A, &LDA, AP)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02ed!(UPLO::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer)

    INFO = [0]

    ccall((:ma02ed_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &UPLO, &N, A, &LDA)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02fd!(X1::FloatingPoint, X2::FloatingPoint,
    C::FloatingPoint, S::FloatingPoint)

    INFO = [0]

    ccall((:ma02fd_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &X1, &X2, &C,
            &S, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function ma02gd!(N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, K1::Integer, K2::Integer,
    IPIV::Array{BlasInt,1}, INCX::Integer)

    INFO = [0]

    ccall((:ma02gd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, A, &LDA, &K1, &K2, IPIV, &INCX)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MA02GD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01kd!(UPLO::Char, TRANS::Char, N::Integer, K::Integer,
    ALPHA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer, BETA::FloatingPoint,
    C::Array{Float64,2}, LDC::Integer)

    INFO = [0]

    ccall((:mb01kd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &UPLO,
            &TRANS, &N, &K, &ALPHA, A, &LDA, B, &LDB, &BETA, C,
            &LDC, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01ld!(UPLO::Char, TRANS::Char, M::Integer, N::Integer,
    ALPHA::FloatingPoint, BETA::FloatingPoint,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,2}, LDX::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb01ld_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &UPLO, &TRANS, &M, &N, &ALPHA, &BETA, R,
            &LDR, A, &LDA, X, &LDX, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01LD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01md!(UPLO::Char, N::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,1}, INCX::Integer, BETA::FloatingPoint,
    Y::Array{Float64,1}, INCY::Integer)

    INFO = [0]

    ccall((:mb01md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &UPLO, &N, &ALPHA, A, &LDA, X, &INCX, &BETA, Y, &INCY)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01nd!(UPLO::Char, N::Integer, ALPHA::FloatingPoint,
    X::Array{Float64,1}, INCX::Integer,
    Y::Array{Float64,1}, INCY::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint)

    INFO = [0]

    ccall((:mb01nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}), &UPLO, &N,
            &ALPHA, X, &INCX, Y, &INCY, A, &LDA)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01pd!(SCUN::Char, TYPE::Char, M::Integer, N::Integer,
    KL::Integer, KU::Integer, ANRM::FloatingPoint, NBL::Integer,
    NROWS::Integer, A::Array{Float64,1}, LDA::FloatingPoint)

    INFO = [0]

    ccall((:mb01pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &SCUN, &TYPE, &M, &N, &KL,
            &KU, &ANRM, &NBL, &NROWS, A, &LDA, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01qd!(TYPE::Char, M::Integer, N::Integer, KL::Integer,
    KU::Integer, CFROM::FloatingPoint, CTO::FloatingPoint,
    NBL::Integer, NROWS::Integer, A::Array{Float64,1},
    LDA::FloatingPoint)

    INFO = [0]

    ccall((:mb01qd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &TYPE, &M, &N, &KL, &KU,
            &CFROM, &CTO, &NBL, &NROWS, A, &LDA, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01rd!(UPLO::Char, TRANS::Char, M::Integer, N::Integer,
    ALPHA::FloatingPoint, BETA::FloatingPoint,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,2}, LDX::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb01rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &UPLO, &TRANS, &M, &N, &ALPHA, &BETA, R,
            &LDR, A, &LDA, X, &LDX, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01ru!(UPLO::Char, TRANS::Char, M::Integer, N::Integer,
    ALPHA::FloatingPoint, BETA::FloatingPoint,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,2}, LDX::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb01ru_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &UPLO, &TRANS, &M, &N, &ALPHA, &BETA, R,
            &LDR, A, &LDA, X, &LDX, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01RU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01rw!(UPLO::Char, TRANS::Char, M::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb01rw_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &UPLO, &TRANS, &M, &N, A, &LDA, Z, &LDZ, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01RW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01rx!(SIDE::Char, UPLO::Char, TRANS::Char, M::Integer,
    N::Integer, ALPHA::FloatingPoint, BETA::FloatingPoint,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer)

    INFO = [0]

    ccall((:mb01rx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &SIDE, &UPLO, &TRANS, &M, &N, &ALPHA, &BETA, R, &LDR, A,
            &LDA, B, &LDB, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01RX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01ry!(SIDE::Char, UPLO::Char, TRANS::Char, M::Integer,
    ALPHA::FloatingPoint, BETA::FloatingPoint,
    R::Array{Float64,2}, LDR::Integer,
    H::Array{Float64,2}, LDH::Integer,
    B::Array{Float64,2}, LDB::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb01ry_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &SIDE, &UPLO, &TRANS, &M, &ALPHA, &BETA, R, &LDR, H,
            &LDH, B, &LDB, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01RY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01sd!(JOBS::Char, M::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    R::Array{Float64,1}, C::Array{Float64,1})

    INFO = [0]

    ccall((:mb01sd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}), &JOBS, &M, &N, A, &LDA, R, C)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01td!(N::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb01td_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &N, A, &LDA, B, &LDB, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01ud!(SIDE::Char, TRANS::Char, M::Integer, N::Integer,
    ALPHA::FloatingPoint, H::Array{Float64,2}, LDH::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer)

    INFO = [0]

    ccall((:mb01ud_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &SIDE, &TRANS, &M, &N,
            &ALPHA, H, &LDH, A, &LDA, B, &LDB, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01uw!(SIDE::Char, TRANS::Char, M::Integer, N::Integer,
    ALPHA::FloatingPoint, H::Array{Float64,2}, LDH::Integer,
    A::Array{Float64,2}, LDA::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb01uw_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &SIDE, &TRANS, &M, &N,
            &ALPHA, H, &LDH, A, &LDA, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01UW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01ux!(SIDE::Char, UPLO::Char, TRANS::Char, M::Integer,
    N::Integer, ALPHA::FloatingPoint, T::Array{Float64,2},
    LDT::Integer, A::Array{Float64,2}, LDA::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb01ux_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &SIDE, &UPLO,
            &TRANS, &M, &N, &ALPHA, T, &LDT, A, &LDA, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01UX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01vd!(TRANA::Char, TRANB::Char, MA::Integer, NA::Integer,
    MB::Integer, NB::Integer, ALPHA::FloatingPoint,
    BETA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, MC::Integer, NC::Integer)

    INFO = [0]

    ccall((:mb01vd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &TRANA,
            &TRANB, &MA, &NA, &MB, &NB, &ALPHA, &BETA, A, &LDA, B,
            &LDB, C, &LDC, &MC, &NC, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01wd!(DICO::Char, UPLO::Char, TRANS::Char, HESS::Char,
    N::Integer, ALPHA::FloatingPoint, BETA::FloatingPoint,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    T::Array{Float64,2}, LDT::Integer)

    INFO = [0]

    ccall((:mb01wd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &UPLO, &TRANS, &HESS, &N, &ALPHA, &BETA, R, &LDR,
            A, &LDA, T, &LDT, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01xd!(UPLO::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint)

    INFO = [0]

    ccall((:mb01xd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &UPLO, &N, A,
            &LDA, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01XD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01xy!(UPLO::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint)

    INFO = [0]

    ccall((:mb01xy_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &UPLO, &N, A,
            &LDA, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01XY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01yd!(UPLO::Char, TRANS::Char, N::Integer, K::Integer,
    L::Integer, ALPHA::FloatingPoint, BETA::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint)

    INFO = [0]

    ccall((:mb01yd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &UPLO, &TRANS, &N, &K, &L,
            &ALPHA, &BETA, A, &LDA, C, &LDC, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01YD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb01zd!(SIDE::Char, UPLO::Char, TRANST::Char, DIAG::Char,
    M::Integer, N::Integer, L::Integer, ALPHA::FloatingPoint,
    T::Array{Float64,1}, LDT::FloatingPoint,
    H::Array{Float64,1}, LDH::FloatingPoint)

    INFO = [0]

    ccall((:mb01zd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &SIDE, &UPLO,
            &TRANST, &DIAG, &M, &N, &L, &ALPHA, T, &LDT, H, &LDH,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB01ZD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02cd!(JOB::Char, TYPET::Char, K::Integer, N::Integer,
    T::Array{Float64,2}, LDT::Integer,
    G::Array{Float64,1}, LDG::Integer,
    R::Array{Float64,2}, LDR::Integer,
    L::Array{Float64,2}, LDL::Integer,
    CS::Array{Float64,1}, LCS::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02cd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &TYPET,
            &K, &N, T, &LDT, G, &LDG, R, &LDR, L, &LDL, CS, &LCS,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02cu!(TYPEG::Char, K::Integer, P::Integer, Q::Integer,
    NB::Integer, A1::Array{Float64,2}, LDA1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B::Array{Float64,2}, LDB::Integer, RNK::Integer,
    IPVT::Array{BlasInt,1}, CS::Array{Float64,1},
    TOL::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb02cu_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TYPEG, &K, &P, &Q, &NB, A1, &LDA1, A2, &LDA2, B, &LDB,
            &RNK, IPVT, CS, &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02CU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02cv!(TYPEG::Char, STRUCG::Char, K::Integer, N::Integer,
    P::Integer, Q::Integer, NB::Integer, RNK::Integer,
    A1::Array{Float64,2}, LDA1::Integer,
    A2::Array{Float64,2}, LDA2::Integer,
    B::Array{Float64,2}, LDB::Integer,
    F1::Array{Float64,2}, LDF1::Integer,
    F2::Array{Float64,2}, LDF2::Integer,
    G::Array{Float64,2}, LDG::Integer,
    CS::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb02cv_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &TYPEG, &STRUCG, &K, &N,
            &P, &Q, &NB, &RNK, A1, &LDA1, A2, &LDA2, B, &LDB, F1,
            &LDF1, F2, &LDF2, G, &LDG, CS, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02CV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02cx!(TYPET::Char, P::Integer, Q::Integer, K::Integer,
    A::Array{Float64,1}, LDA::Integer,
    B::Array{Float64,1}, LDB::Integer,
    CS::Array{Float64,1}, LCS::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02cx_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &TYPET, &P,
            &Q, &K, A, &LDA, B, &LDB, CS, &LCS, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02CX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02cy!(TYPET::Char, STRUCG::Char, P::Integer, Q::Integer,
    N::Integer, K::Integer, A::Array{Float64,1}, LDA::Integer,
    B::Array{Float64,1}, LDB::Integer,
    H::Array{Float64,2}, LDH::Integer,
    CS::Array{Float64,1}, LCS::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02cy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &TYPET,
            &STRUCG, &P, &Q, &N, &K, A, &LDA, B, &LDB, H, &LDH, CS,
            &LCS, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02CY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02dd!(JOB::Char, TYPET::Char, K::Integer, M::Integer,
    N::Integer, TA::Array{Float64,2}, LDTA::Integer,
    T::Array{Float64,2}, LDT::Integer,
    G::Array{Float64,1}, LDG::Integer,
    R::Array{Float64,2}, LDR::Integer,
    L::Array{Float64,2}, LDL::Integer,
    CS::Array{Float64,1}, LCS::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02dd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &TYPET, &K, &M, &N,
            TA, &LDTA, T, &LDT, G, &LDG, R, &LDR, L, &LDL, CS, &LCS,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02ed!(TYPET::Char, K::Integer, N::Integer, NRHS::Integer,
    T::Array{Float64,2}, LDT::Integer,
    B::Array{Float64,2}, LDB::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02ed_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &TYPET, &K, &N, &NRHS, T, &LDT, B, &LDB,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02fd!(TYPET::Char, K::Integer, N::Integer, P::Integer,
    S::Integer, T::Array{Float64,2}, LDT::Integer,
    R::Array{Float64,2}, LDR::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02fd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &TYPET, &K, &N, &P, &S, T,
            &LDT, R, &LDR, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02gd!(TYPET::Char, TRIU::Char, K::Integer, N::Integer,
    NL::Integer, P::Integer, S::Integer, T::Array{Float64,2},
    LDT::Integer, RB::Array{Float64,2}, LDRB::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02gd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TYPET, &TRIU, &K, &N, &NL, &P, &S, T, &LDT, RB, &LDRB,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02GD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02hd!(TRIU::Char, K::Integer, L::Integer, M::Integer,
    ML::Integer, N::Integer, NU::Integer, P::Integer, S::Integer,
    TC::Array{Float64,2}, LDTC::Integer,
    TR::Array{Float64,2}, LDTR::Integer,
    RB::Array{Float64,2}, LDRB::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02hd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TRIU, &K, &L, &M, &ML, &N, &NU, &P, &S, TC, &LDTC, TR,
            &LDTR, RB, &LDRB, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02id!(JOB::Char, K::Integer, L::Integer, M::Integer,
    N::Integer, RB::Integer, RC::Integer, TC::Array{Float64,2},
    LDTC::Integer, TR::Array{Float64,2}, LDTR::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02id_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &K, &L, &M, &N, &RB, &RC, TC, &LDTC, TR, &LDTR, B,
            &LDB, C, &LDC, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02jd!(JOB::Char, K::Integer, L::Integer, M::Integer,
    N::Integer, P::Integer, S::Integer, TC::Array{Float64,2},
    LDTC::Integer, TR::Array{Float64,2}, LDTR::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02jd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &K, &L, &M, &N, &P, &S, TC, &LDTC, TR, &LDTR, Q,
            &LDQ, R, &LDR, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02JD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02jx!(JOB::Char, K::Integer, L::Integer, M::Integer,
    N::Integer, TC::Array{Float64,2}, LDTC::Integer,
    TR::Array{Float64,2}, LDTR::Integer, RNK::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer, JPVT::Array{BlasInt,1},
    TOL1::FloatingPoint, TOL2::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02jx_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &K, &L, &M, &N, TC,
            &LDTC, TR, &LDTR, &RNK, Q, &LDQ, R, &LDR, JPVT, &TOL1,
            &TOL2, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02JX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02kd!(LDBLK::Char, TRANS::Char, K::Integer, L::Integer,
    M::Integer, N::Integer, R::Integer, ALPHA::FloatingPoint,
    BETA::FloatingPoint, TC::Array{Float64,2}, LDTC::Integer,
    TR::Array{Float64,2}, LDTR::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02kd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &LDBLK, &TRANS, &K, &L, &M,
            &N, &R, &ALPHA, &BETA, TC, &LDTC, TR, &LDTR, B, &LDB, C,
            &LDC, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02md!(JOB::Char, M::Integer, N::Integer, L::Integer,
    RANK::Integer, C::Array{Float64,2}, LDC::Integer,
    S::Array{Float64,1}, X::Array{Float64,2},
    LDX::Integer, TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:mb02md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &M, &N, &L, &RANK, C,
            &LDC, S, X, &LDX, &TOL, IWORK, DWORK, &LDWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02nd!(M::Integer, N::Integer, L::Integer, RANK::Integer,
    THETA::FloatingPoint, C::Array{Float64,2}, LDC::Integer,
    X::Array{Float64,2}, LDX::Integer,
    Q::Array{Float64,1}, INUL::Array{Bool,1},
    TOL::FloatingPoint, RELTOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1}, IWARN::Integer)

    INFO = [0]

    ccall((:mb02nd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Bool}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}), &M, &N, &L, &RANK, &THETA, C, &LDC, X,
            &LDX, Q, INUL, &TOL, &RELTOL, IWORK, DWORK, &LDWORK,
            BWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02ny!(UPDATU::Bool, UPDATV::Bool, M::Integer, N::Integer,
    I::Integer, K::Integer, Q::Array{Float64,1},
    E::Array{Float64,1}, U::Array{Float64,2},
    LDU::Integer, V::Array{Float64,2}, LDV::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb02ny_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}), &UPDATU,
            &UPDATV, &M, &N, &I, &K, Q, E, U, &LDU, V, &LDV, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02NY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02od!(SIDE::Char, UPLO::Char, TRANS::Char, DIAG::Char,
    NORM::Char, M::Integer, N::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer, RCOND::FloatingPoint,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb02od_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &SIDE, &UPLO,
            &TRANS, &DIAG, &NORM, &M, &N, &ALPHA, A, &LDA, B, &LDB,
            &RCOND, &TOL, IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02pd!(FACT::Char, TRANS::Char, N::Integer, NRHS::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    AF::Array{Float64,1}, LDAF::FloatingPoint,
    IPIV::Array{BlasInt,1}, EQUED::Char, R::Array{Float64,1},
    C::Array{Float64,1}, B::Array{Float64,1},
    LDB::FloatingPoint, X::Array{Float64,1}, LDX::FloatingPoint,
    RCOND::FloatingPoint, FERR::Array{Float64,1},
    BERR::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb02pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Char},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &FACT, &TRANS, &N, &NRHS, A, &LDA, AF, &LDAF, IPIV,
            &EQUED, R, C, B, &LDB, X, &LDX, &RCOND, FERR, BERR,
            IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02qd!(JOB::Char, INIPER::Char, M::Integer, N::Integer,
    NRHS::Integer, RCOND::FloatingPoint, SVLMAX::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint, Y::FloatingPoint,
    JPVT::Array{BlasInt,1}, RANK::Integer,
    SVAL::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb02qd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &INIPER, &M, &N, &NRHS, &RCOND, &SVLMAX, A, &LDA,
            B, &LDB, &Y, JPVT, &RANK, SVAL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02qy!(M::Integer, N::Integer, NRHS::Integer, RANK::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    JPVT::Array{BlasInt,1}, B::Array{Float64,1},
    LDB::FloatingPoint, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02qy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &M, &N,
            &NRHS, &RANK, A, &LDA, JPVT, B, &LDB, TAU, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02QY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02rd!(TRANS::Char, N::Integer, NRHS::Integer,
    H::Array{Float64,1}, LDH::FloatingPoint,
    IPIV::Array{BlasInt,1}, B::Array{Float64,1},
    LDB::FloatingPoint)

    INFO = [0]

    ccall((:mb02rd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &TRANS, &N,
            &NRHS, H, &LDH, IPIV, B, &LDB, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02rz!(TRANS::Char, N::Integer, NRHS::Integer,
    H::Array{Complex,1}, LDH::Complex, IPIV::Array{BlasInt,1},
    B::Array{Complex,1}, LDB::Complex)

    INFO = [0]

    ccall((:mb02rz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{BlasInt}), &TRANS, &N, &NRHS, H, &LDH, IPIV, B,
            &LDB, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02RZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02sd!(N::Integer, H::Array{Float64,2}, LDH::Integer,
    IPIV::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb02sd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &N, H, &LDH,
            IPIV, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02sz!(N::Integer, H::Array{Complex,2}, LDH::Integer,
    IPIV::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb02sz_, libslicot), Void, (Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &N, H, &LDH,
            IPIV, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02SZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02td!(NORM::Char, N::Integer, HNORM::FloatingPoint,
    H::Array{Float64,1}, LDH::FloatingPoint,
    IPIV::Array{BlasInt,1}, RCOND::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb02td_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &NORM, &N, &HNORM, H, &LDH, IPIV, &RCOND, IWORK, DWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02tz!(NORM::Char, N::Integer, HNORM::FloatingPoint,
    H::Array{Complex,1}, LDH::Complex, IPIV::Array{BlasInt,1},
    RCOND::FloatingPoint, DWORK::Array{Float64,1},
    ZWORK::Array{Complex,1})

    INFO = [0]

    ccall((:mb02tz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}), &NORM, &N, &HNORM, H,
            &LDH, IPIV, &RCOND, DWORK, ZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02TZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02ud!(FACT::Char, SIDE::Char, TRANS::Char, JOBP::Char,
    M::Integer, N::Integer, ALPHA::FloatingPoint,
    RCOND::FloatingPoint, RANK::Integer, R::Array{Float64,2},
    LDR::Integer, Q::Array{Float64,2}, LDQ::Integer,
    SV::Array{Float64,1}, B::Array{Float64,2},
    LDB::Integer, RP::Array{Float64,2}, LDRP::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02ud_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &FACT, &SIDE,
            &TRANS, &JOBP, &M, &N, &ALPHA, &RCOND, &RANK, R, &LDR,
            Q, &LDQ, SV, B, &LDB, RP, &LDRP, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02uu!(N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, RHS::Array{Float64,1},
    IPIV::Array{BlasInt,1}, JPIV::Array{BlasInt,1},
    SCALE::FloatingPoint)

    INFO = [0]

    ccall((:mb02uu_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}), &N, A, &LDA, RHS, IPIV, JPIV, &SCALE)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02UU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02uv!(N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, IPIV::Array{BlasInt,1},
    JPIV::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb02uv_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, A, &LDA, IPIV, JPIV, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02UV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02uw!(LTRANS::Bool, N::Integer, M::Integer,
    PAR::Array{Float64,1}, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    SCALE::FloatingPoint, IWARN::Integer)

    INFO = [0]

    ccall((:mb02uw_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &LTRANS, &N, &M, PAR, A, &LDA, B, &LDB, &SCALE, &IWARN)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02UW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02vd!(TRANS::Char, M::Integer, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    IPIV::Array{BlasInt,1}, B::Array{Float64,1},
    LDB::FloatingPoint)

    INFO = [0]

    ccall((:mb02vd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &TRANS, &M,
            &N, A, &LDA, IPIV, B, &LDB, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb02yd!(COND::Char, N::Integer, R::Array{Float64,2},
    LDR::Integer, IPVT::Array{BlasInt,1},
    DIAG::Array{Float64,1}, QTB::Array{Float64,1},
    RANK::Integer, X::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb02yd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &COND, &N, R,
            &LDR, IPVT, DIAG, QTB, &RANK, X, &TOL, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB02YD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ad!(SHFT::Char, K::Integer, N::Integer,
    AMAP::Array{BlasInt,1}, S::Array{BlasInt,1}, SINV::Integer,
    A::Array{Float64,3}, LDA1::Integer, LDA2::Integer,
    C1::FloatingPoint, S1::FloatingPoint, C2::FloatingPoint,
    S2::FloatingPoint)

    INFO = [0]

    ccall((:mb03ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}), &SHFT, &K,
            &N, AMAP, S, &SINV, A, &LDA1, &LDA2, &C1, &S1, &C2, &S2)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ba!(K::Integer, H::Integer, S::Array{BlasInt,1},
    SMULT::Integer, AMAP::Array{BlasInt,1}, QMAP::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb03ba_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &K, &H, S, &SMULT, AMAP, QMAP)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03BA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03bb!(BASE::FloatingPoint, LGBAS::FloatingPoint,
    ULP::FloatingPoint, K::Integer, AMAP::Array{BlasInt,1},
    S::Array{BlasInt,1}, SINV::Integer, A::Array{Float64,3},
    LDA1::Integer, LDA2::Integer, ALPHAR::Array{Float64,1},
    ALPHAI::Array{Float64,1}, BETA::Array{Float64,1},
    SCAL::Array{BlasInt,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03bb_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &BASE, &LGBAS, &ULP, &K,
            AMAP, S, &SINV, A, &LDA1, &LDA2, ALPHAR, ALPHAI, BETA,
            SCAL, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03BB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03bc!(K::Integer, AMAP::Array{BlasInt,1},
    S::Array{BlasInt,1}, SINV::Integer, A::Array{Float64,3},
    LDA1::Integer, LDA2::Integer, MACPAR::Array{Float64,1},
    CV::Array{Float64,1}, SV::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03bc_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}), &K, AMAP, S, &SINV, A, &LDA1, &LDA2,
            MACPAR, CV, SV, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03BC: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03bd!(JOB::Char, DEFL::Char, COMPQ::Char,
    QIND::Array{BlasInt,1}, K::Integer, N::Integer, H::Integer,
    ILO::Integer, IHI::Integer, S::Array{BlasInt,1},
    A::Array{Float64,3}, LDA1::Integer, LDA2::Integer,
    Q::Array{Float64,3}, LDQ1::Integer, LDQ2::Integer,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, SCAL::Array{BlasInt,1},
    IWORK::Array{BlasInt,1}, LIWORK::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:mb03bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &DEFL, &COMPQ, QIND, &K, &N, &H, &ILO, &IHI, S, A,
            &LDA1, &LDA2, Q, &LDQ1, &LDQ2, ALPHAR, ALPHAI, BETA,
            SCAL, IWORK, &LIWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03be!(K::Integer, AMAP::Array{BlasInt,1},
    S::Array{BlasInt,1}, SINV::Integer, A::Array{Float64,3},
    LDA1::Integer, LDA2::Integer)

    INFO = [0]

    ccall((:mb03be_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &K, AMAP, S, &SINV, A, &LDA1, &LDA2)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03BE: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03cd!(UPLO::Char, N1::Integer, N2::Integer,
    PREC::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    Q3::Array{Float64,1}, LDQ3::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03cd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &UPLO, &N1, &N2, &PREC, A, &LDA, B, &LDB,
            D, &LDD, Q1, &LDQ1, Q2, &LDQ2, Q3, &LDQ3, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03dd!(UPLO::Char, N1::Integer, N2::Integer,
    PREC::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03dd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &UPLO, &N1, &N2, &PREC, A, &LDA, B, &LDB,
            Q1, &LDQ1, Q2, &LDQ2, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ed!(N::Integer, PREC::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    Q3::Array{Float64,1}, LDQ3::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03ed_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &PREC, A,
            &LDA, B, &LDB, D, &LDD, Q1, &LDQ1, Q2, &LDQ2, Q3, &LDQ3,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03fd!(N::Integer, PREC::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03fd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &PREC, A,
            &LDA, B, &LDB, Q1, &LDQ1, Q2, &LDQ2, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03gd!(N::Integer, B::Array{Float64,1},
    LDB::FloatingPoint, D::Array{Float64,1}, LDD::FloatingPoint,
    MACPAR::Array{Float64,1}, Q::Array{Float64,1},
    LDQ::FloatingPoint, U::Array{Float64,1}, LDU::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03gd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, B, &LDB,
            D, &LDD, MACPAR, Q, &LDQ, U, &LDU, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03GD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03hd!(N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    MACPAR::Array{Float64,1}, Q::Array{Float64,1},
    LDQ::FloatingPoint, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03hd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, A, &LDA, B, &LDB, MACPAR, Q, &LDQ, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03id!(COMPQ::Char, COMPU::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    U1::Array{Float64,1}, LDU1::FloatingPoint,
    U2::Array{Float64,1}, LDU2::FloatingPoint, NEIG::Integer,
    IWORK::Array{BlasInt,1}, LIWORK::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03id_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &COMPQ,
            &COMPU, &N, A, &LDA, C, &LDC, D, &LDD, B, &LDB, F, &LDF,
            Q, &LDQ, U1, &LDU1, U2, &LDU2, &NEIG, IWORK, &LIWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03jd!(COMPQ::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, D::Array{Float64,1}, LDD::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint, NEIG::Integer,
    IWORK::Array{BlasInt,1}, LIWORK::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03jd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &COMPQ, &N, A, &LDA, D, &LDD, B, &LDB, F, &LDF, Q, &LDQ,
            &NEIG, IWORK, &LIWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03JD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ka!(COMPQ::Char, WHICHQ::Array{BlasInt,1}, WS::Bool,
    K::Integer, NC::Integer, KSCHUR::Integer, IFST::Integer,
    ILST::Integer, N::Array{BlasInt,1}, NI::Array{BlasInt,1},
    S::Array{BlasInt,1}, T::Array{Float64,1},
    LDT::Array{BlasInt,1}, IXT::Array{BlasInt,1},
    Q::Array{Float64,1}, LDQ::Array{BlasInt,1},
    IXQ::Array{BlasInt,1}, TOL::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03ka_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &COMPQ, WHICHQ, &WS, &K, &NC, &KSCHUR, &IFST, &ILST, N,
            NI, S, T, LDT, IXT, Q, LDQ, IXQ, TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03KA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03kb!(COMPQ::Char, WHICHQ::Array{BlasInt,1}, WS::Bool,
    K::Integer, NC::Integer, KSCHUR::Integer, J1::Integer,
    N1::Integer, N2::Integer, N::Array{BlasInt,1},
    NI::Array{BlasInt,1}, S::Array{BlasInt,1},
    T::Array{Float64,1}, LDT::Array{BlasInt,1},
    IXT::Array{BlasInt,1}, Q::Array{Float64,1},
    LDQ::Array{BlasInt,1}, IXQ::Array{BlasInt,1},
    TOL::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03kb_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &COMPQ, WHICHQ, &WS, &K, &NC, &KSCHUR,
            &J1, &N1, &N2, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ, TOL,
            IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03KB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03kc!(K::Integer, KHESS::Integer, N::Integer, R::Integer,
    S::Array{BlasInt,1}, A::Array{Float64,1}, LDA::Integer,
    V::Array{Float64,1}, TAU::Array{Float64,1})

    INFO = [0]

    ccall((:mb03kc_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}), &K, &KHESS,
            &N, &R, S, A, &LDA, V, TAU)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03KC: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03kd!(COMPQ::Char, WHICHQ::Array{BlasInt,1}, STRONG::Char,
    K::Integer, NC::Integer, KSCHUR::Integer, N::Array{BlasInt,1},
    NI::Array{BlasInt,1}, S::Array{BlasInt,1}, SELECT::Array{Bool,1},
    T::Array{Float64,1}, LDT::Array{BlasInt,1},
    IXT::Array{BlasInt,1}, Q::Array{Float64,1},
    LDQ::Array{BlasInt,1}, IXQ::Array{BlasInt,1}, M::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03kd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Bool},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &COMPQ, WHICHQ, &STRONG, &K, &NC, &KSCHUR, N, NI, S,
            SELECT, T, LDT, IXT, Q, LDQ, IXQ, &M, &TOL, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ke!(TRANA::Bool, TRANB::Bool, ISGN::Integer, K::Integer,
    M::Integer, N::Integer, PREC::FloatingPoint, SMIN::FloatingPoint,
    S::Array{BlasInt,1}, A::Array{Float64,1},
    B::Array{Float64,1}, C::Array{Float64,1},
    SCALE::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03ke_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &TRANA, &TRANB, &ISGN, &K,
            &M, &N, &PREC, &SMIN, S, A, B, C, &SCALE, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03KE: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ld!(COMPQ::Char, ORTH::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    DE::Array{Float64,1}, LDDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    FG::Array{Float64,1}, LDFG::FloatingPoint, NEIG::Integer,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, BWORK::Array{Bool,1},
    IWORK::Array{BlasInt,1}, LIWORK::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03ld_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &COMPQ, &ORTH, &N, A, &LDA, DE, &LDDE, B,
            &LDB, FG, &LDFG, &NEIG, Q, &LDQ, ALPHAR, ALPHAI, BETA,
            BWORK, IWORK, &LIWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03LD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03md!(N::Integer, L::Integer, THETA::FloatingPoint,
    Q::Array{Float64,1}, E::Array{Float64,1},
    Q2::Array{Float64,1}, E2::Array{Float64,1},
    PIVMIN::FloatingPoint, TOL::FloatingPoint, RELTOL::FloatingPoint,
    IWARN::Integer)

    INFO = [0]

    ccall((:mb03md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &L, &THETA, Q, E, Q2,
            E2, &PIVMIN, &TOL, &RELTOL, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03od!(JOBQR::Char, M::Integer, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    JPVT::Array{BlasInt,1}, RCOND::FloatingPoint,
    SVLMAX::FloatingPoint, TAU::Array{Float64,1}, RANK::Integer,
    SVAL::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOBQR, &M, &N, A, &LDA, JPVT, &RCOND, &SVLMAX, TAU,
            &RANK, SVAL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03oy!(M::Integer, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, RCOND::FloatingPoint, SVLMAX::FloatingPoint,
    RANK::Integer, SVAL::Array{Float64,1},
    JPVT::Array{BlasInt,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03oy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &M, &N, A, &LDA, &RCOND,
            &SVLMAX, &RANK, SVAL, JPVT, TAU, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03OY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03pd!(JOBRQ::Char, M::Integer, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    JPVT::Array{BlasInt,1}, RCOND::FloatingPoint,
    SVLMAX::FloatingPoint, TAU::Array{Float64,1}, RANK::Integer,
    SVAL::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03pd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &JOBRQ, &M,
            &N, A, &LDA, JPVT, &RCOND, &SVLMAX, TAU, &RANK, SVAL,
            DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03py!(M::Integer, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, RCOND::FloatingPoint, SVLMAX::FloatingPoint,
    RANK::Integer, SVAL::Array{Float64,1},
    JPVT::Array{BlasInt,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03py_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &M, &N, A, &LDA, &RCOND,
            &SVLMAX, &RANK, SVAL, JPVT, TAU, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03PY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03qd!(DICO::Char, STDOM::Char, JOBU::Char, N::Integer,
    NLOW::Integer, NSUP::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    U::Array{Float64,2}, LDU::Integer, NDIM::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03qd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &DICO, &STDOM, &JOBU, &N, &NLOW, &NSUP, &ALPHA, A, &LDA,
            U, &LDU, &NDIM, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03qx!(N::Integer, T::Array{Float64,1}, LDT::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1})

    INFO = [0]

    ccall((:mb03qx_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, T, &LDT, WR, WI, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03QX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03qy!(N::Integer, L::Integer, A::Array{Float64,2},
    LDA::Integer, U::Array{Float64,2}, LDU::Integer,
    E1::FloatingPoint, E2::FloatingPoint)

    INFO = [0]

    ccall((:mb03qy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &N, &L, A,
            &LDA, U, &LDU, &E1, &E2, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03QY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03rd!(JOBX::Char, SORT::Char, N::Integer,
    PMAX::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,2}, LDX::Integer, NBLCKS::Integer,
    BLSIZE::Array{BlasInt,1}, WR::Array{Float64,1},
    WI::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &JOBX, &SORT, &N, &PMAX, A, &LDA, X,
            &LDX, &NBLCKS, BLSIZE, WR, WI, &TOL, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03rx!(JOBV::Char, N::Integer, KL::Integer, KU::Integer,
    A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,2}, LDX::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03rx_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}), &JOBV, &N, &KL, &KU, A, &LDA, X, &LDX,
            WR, WI, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03RX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ry!(M::Integer, N::Integer, PMAX::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer)

    INFO = [0]

    ccall((:mb03ry_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &M, &N, &PMAX, A, &LDA, B, &LDB, C, &LDC, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03RY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03sd!(JOBSCL::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03sd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOBSCL, &N, A, &LDA, QG, &LDQG, WR, WI,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03td!(TYP::Char, COMPU::Char, SELECT::Array{Bool,1},
    LOWER::Array{Bool,1}, N::Integer, A::Array{Float64,2},
    LDA::Integer, G::Array{Float64,2}, LDG::Integer,
    U1::Array{Float64,2}, LDU1::Integer,
    U2::Array{Float64,2}, LDU2::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    M::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03td_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Bool}, Ptr{Bool}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &TYP, &COMPU, SELECT, LOWER, &N, A, &LDA,
            G, &LDG, U1, &LDU1, U2, &LDU2, WR, WI, &M, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ts!(ISHAM::Bool, WANTU::Bool, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    G::Array{Float64,2}, LDG::Integer,
    U1::Array{Float64,2}, LDU1::Integer,
    U2::Array{Float64,2}, LDU2::Integer, J1::Integer,
    N1::Integer, N2::Integer, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03ts_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &ISHAM, &WANTU, &N, A,
            &LDA, G, &LDG, U1, &LDU1, U2, &LDU2, &J1, &N1, &N2,
            DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03TS: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ud!(JOBQ::Char, JOBP::Char, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    SV::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03ud_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOBQ, &JOBP, &N, A, &LDA, Q, &LDQ, SV,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03vd!(N::Integer, P::Integer, ILO::Integer, IHI::Integer,
    A::Array{Float64,1}, LDA1::FloatingPoint,
    LDA2::FloatingPoint, TAU::Array{Float64,1},
    LDTAU::FloatingPoint, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03vd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &N, &P, &ILO, &IHI, A, &LDA1, &LDA2, TAU,
            &LDTAU, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03vy!(N::Integer, P::Integer, ILO::Integer, IHI::Integer,
    A::Array{Float64,1}, LDA1::FloatingPoint,
    LDA2::FloatingPoint, TAU::Array{Float64,1},
    LDTAU::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03vy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &P, &ILO, &IHI, A,
            &LDA1, &LDA2, TAU, &LDTAU, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03VY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03wa!(WANTQ::Bool, WANTZ::Bool, N1::Integer, N2::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer)

    INFO = [0]

    ccall((:mb03wa_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &WANTQ,
            &WANTZ, &N1, &N2, A, &LDA, B, &LDB, Q, &LDQ, Z, &LDZ,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03WA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03wd!(JOB::Char, COMPZ::Char, N::Integer, P::Integer,
    ILO::Integer, IHI::Integer, ILOZ::Integer, IHIZ::Integer,
    H::Array{Float64,1}, LDH1::FloatingPoint,
    LDH2::FloatingPoint, Z::Array{Float64,1},
    LDZ1::FloatingPoint, LDZ2::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03wd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOB, &COMPZ, &N, &P, &ILO, &IHI, &ILOZ,
            &IHIZ, H, &LDH1, &LDH2, Z, &LDZ1, &LDZ2, WR, WI, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03wx!(N::Integer, P::Integer, T::Array{Float64,1},
    LDT1::FloatingPoint, LDT2::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1})

    INFO = [0]

    ccall((:mb03wx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &N, &P, T, &LDT1, &LDT2,
            WR, WI, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03WX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03xd!(BALANC::Char, JOB::Char, JOBU::Char, JOBV::Char,
    N::Integer, A::Array{Float64,2}, LDA::Integer,
    QG::Array{Float64,2}, LDQG::Integer,
    T::Array{Float64,2}, LDT::Integer,
    U1::Array{Float64,2}, LDU1::Integer,
    U2::Array{Float64,2}, LDU2::Integer,
    V1::Array{Float64,2}, LDV1::Integer,
    V2::Array{Float64,2}, LDV2::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    ILO::Integer, SCALE::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03xd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &BALANC, &JOB, &JOBU, &JOBV, &N, A, &LDA, QG, &LDQG, T,
            &LDT, U1, &LDU1, U2, &LDU2, V1, &LDV1, V2, &LDV2, WR,
            WI, &ILO, SCALE, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03XD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03xp!(JOB::Char, COMPQ::Char, COMPZ::Char, N::Integer,
    ILO::Integer, IHI::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03xp_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &COMPQ, &COMPZ, &N,
            &ILO, &IHI, A, &LDA, B, &LDB, Q, &LDQ, Z, &LDZ, ALPHAR,
            ALPHAI, BETA, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03XP: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03xu!(LTRA::Bool, LTRB::Bool, N::Integer, K::Integer,
    NB::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    XA::Array{Float64,2}, LDXA::Integer,
    XB::Array{Float64,2}, LDXB::Integer,
    XG::Array{Float64,2}, LDXG::Integer,
    XQ::Array{Float64,2}, LDXQ::Integer,
    YA::Array{Float64,2}, LDYA::Integer,
    YB::Array{Float64,2}, LDYB::Integer,
    YG::Array{Float64,2}, LDYG::Integer,
    YQ::Array{Float64,2}, LDYQ::Integer,
    CSL::Array{Float64,1}, CSR::Array{Float64,1},
    TAUL::Array{Float64,1}, TAUR::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb03xu_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}),
            &LTRA, &LTRB, &N, &K, &NB, A, &LDA, B, &LDB, G, &LDG, Q,
            &LDQ, XA, &LDXA, XB, &LDXB, XG, &LDXG, XQ, &LDXQ, YA,
            &LDYA, YB, &LDYB, YG, &LDYG, YQ, &LDYQ, CSL, CSR, TAUL,
            TAUR, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03XU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03ya!(WANTT::Bool, WANTQ::Bool, WANTZ::Bool, N::Integer,
    ILO::Integer, IHI::Integer, ILOQ::Integer, IHIQ::Integer,
    POS::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer)

    INFO = [0]

    ccall((:mb03ya_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &WANTT, &WANTQ, &WANTZ, &N, &ILO, &IHI, &ILOQ, &IHIQ,
            &POS, A, &LDA, B, &LDB, Q, &LDQ, Z, &LDZ, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03YA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03yd!(WANTT::Bool, WANTQ::Bool, WANTZ::Bool, N::Integer,
    ILO::Integer, IHI::Integer, ILOQ::Integer, IHIQ::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03yd_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &WANTT, &WANTQ, &WANTZ, &N, &ILO, &IHI, &ILOQ, &IHIQ, A,
            &LDA, B, &LDB, Q, &LDQ, Z, &LDZ, ALPHAR, ALPHAI, BETA,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03YD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03yt!(A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, CSL::FloatingPoint,
    SNL::FloatingPoint, CSR::FloatingPoint, SNR::FloatingPoint)

    INFO = [0]

    ccall((:mb03yt_, libslicot), Void, (Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}), A, &LDA, B, &LDB, ALPHAR, ALPHAI, BETA,
            &CSL, &SNL, &CSR, &SNR)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03YT: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03za!(COMPC::Char, COMPU::Char, COMPV::Char, COMPW::Char,
    WHICH::Char, SELECT::Array{Bool,1}, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    U1::Array{Float64,2}, LDU1::Integer,
    U2::Array{Float64,2}, LDU2::Integer,
    V1::Array{Float64,2}, LDV1::Integer,
    V2::Array{Float64,2}, LDV2::Integer,
    W::Array{Float64,2}, LDW::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    M::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb03za_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &COMPC,
            &COMPU, &COMPV, &COMPW, &WHICH, SELECT, &N, A, &LDA, B,
            &LDB, C, &LDC, U1, &LDU1, U2, &LDU2, V1, &LDV1, V2,
            &LDV2, W, &LDW, WR, WI, &M, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03ZA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb03zd!(WHICH::Char, METH::Char, STAB::Char, BALANC::Char,
    ORTBAL::Char, SELECT::Array{Bool,1}, N::Integer, MM::Integer,
    ILO::Integer, SCALE::Array{Float64,1},
    S::Array{Float64,2}, LDS::Integer,
    T::Array{Float64,2}, LDT::Integer,
    G::Array{Float64,2}, LDG::Integer,
    U1::Array{Float64,2}, LDU1::Integer,
    U2::Array{Float64,2}, LDU2::Integer,
    V1::Array{Float64,2}, LDV1::Integer,
    V2::Array{Float64,2}, LDV2::Integer, M::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    US::Array{Float64,2}, LDUS::Integer,
    UU::Array{Float64,2}, LDUU::Integer, LWORK::Array{Bool,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb03zd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &WHICH, &METH, &STAB,
            &BALANC, &ORTBAL, SELECT, &N, &MM, &ILO, SCALE, S, &LDS,
            T, &LDT, G, &LDG, U1, &LDU1, U2, &LDU2, V1, &LDV1, V2,
            &LDV2, &M, WR, WI, US, &LDUS, UU, &LDUU, LWORK, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB03ZD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ad!(JOB::Char, COMPQ1::Char, COMPQ2::Char, COMPU1::Char,
    COMPU2::Char, N::Integer, Z::Array{Float64,1},
    LDZ::FloatingPoint, H::Array{Float64,1}, LDH::FloatingPoint,
    T::Array{Float64,1}, LDT::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    U11::Array{Float64,1}, LDU11::FloatingPoint,
    U12::Array{Float64,1}, LDU12::FloatingPoint,
    U21::Array{Float64,1}, LDU21::FloatingPoint,
    U22::Array{Float64,1}, LDU22::FloatingPoint,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, IWORK::Array{BlasInt,1},
    LIWORK::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &COMPQ1, &COMPQ2,
            &COMPU1, &COMPU2, &N, Z, &LDZ, H, &LDH, T, &LDT, Q1,
            &LDQ1, Q2, &LDQ2, U11, &LDU11, U12, &LDU12, U21, &LDU21,
            U22, &LDU22, ALPHAR, ALPHAI, BETA, IWORK, &LIWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04bd!(JOB::Char, COMPQ1::Char, COMPQ2::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    DE::Array{Float64,1}, LDDE::FloatingPoint,
    C1::Array{Float64,1}, LDC1::FloatingPoint,
    VW::Array{Float64,1}, LDVW::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    C2::Array{Float64,1}, LDC2::FloatingPoint,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, IWORK::Array{BlasInt,1},
    LIWORK::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &COMPQ1, &COMPQ2, &N, A, &LDA, DE, &LDDE, C1,
            &LDC1, VW, &LDVW, Q1, &LDQ1, Q2, &LDQ2, B, &LDB, F,
            &LDF, C2, &LDC2, ALPHAR, ALPHAI, BETA, IWORK, &LIWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04dd!(JOB::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    ILO::Integer, SCALE::Array{Float64,1})

    INFO = [0]

    ccall((:mb04dd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &JOB, &N, A,
            &LDA, QG, &LDQG, &ILO, SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04di!(JOB::Char, SGN::Char, N::Integer, ILO::Integer,
    SCALE::Array{Float64,1}, M::Integer,
    V1::Array{Float64,2}, LDV1::Integer,
    V2::Array{Float64,2}, LDV2::Integer)

    INFO = [0]

    ccall((:mb04di_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOB, &SGN, &N, &ILO, SCALE, &M, V1,
            &LDV1, V2, &LDV2, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04DI: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ds!(JOB::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    ILO::Integer, SCALE::Array{Float64,1})

    INFO = [0]

    ccall((:mb04ds_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &JOB, &N, A,
            &LDA, QG, &LDQG, &ILO, SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04DS: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04dy!(JOBSCL::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    D::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04dy_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &JOBSCL, &N,
            A, &LDA, QG, &LDQG, D, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04DY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04gd!(M::Integer, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, JPVT::Array{BlasInt,1},
    TAU::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04gd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &M, &N, A, &LDA, JPVT, TAU,
            DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04GD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04hd!(COMPQ1::Char, COMPQ2::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    Q1::Array{Float64,1}, LDQ1::FloatingPoint,
    Q2::Array{Float64,1}, LDQ2::FloatingPoint,
    BWORK::Array{Bool,1}, IWORK::Array{BlasInt,1}, LIWORK::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04hd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &COMPQ1,
            &COMPQ2, &N, A, &LDA, B, &LDB, Q1, &LDQ1, Q2, &LDQ2,
            BWORK, IWORK, &LIWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04id!(N::Integer, M::Integer, P::Integer, L::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    TAU::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb04id_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &P, &L, A, &LDA, B,
            &LDB, TAU, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04iy!(SIDE::Char, TRANS::Char, N::Integer, M::Integer,
    K::Integer, P::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, TAU::Array{Float64,1},
    C::Array{Float64,1}, LDC::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04iy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &SIDE, &TRANS, &N, &M, &K, &P, A, &LDA, TAU, C, &LDC,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04IY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04iz!(N::Integer, M::Integer, P::Integer, L::Integer,
    A::Array{Complex,2}, LDA::Integer, B::Array{Complex,2},
    LDB::Integer, TAU::Array{Complex,1}, ZWORK::Array{Complex,1},
    LZWORK::Integer)

    INFO = [0]

    ccall((:mb04iz_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &M, &P, &L, A, &LDA, B, &LDB, TAU,
            ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04IZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04jd!(N::Integer, M::Integer, P::Integer, L::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    TAU::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb04jd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &P, &L, A, &LDA, B,
            &LDB, TAU, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04JD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04kd!(UPLO::Char, N::Integer, M::Integer, P::Integer,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    TAU::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04kd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}),
            &UPLO, &N, &M, &P, R, &LDR, A, &LDA, B, &LDB, C, &LDC,
            TAU, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ld!(UPLO::Char, N::Integer, M::Integer, P::Integer,
    L::Array{Float64,2}, LDL::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    TAU::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04ld_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}),
            &UPLO, &N, &M, &P, L, &LDL, A, &LDA, B, &LDB, C, &LDC,
            TAU, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04LD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04md!(N::Integer, MAXRED::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    SCALE::Array{Float64,1})

    INFO = [0]

    ccall((:mb04md_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &MAXRED, A, &LDA, SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04nd!(UPLO::Char, N::Integer, M::Integer, P::Integer,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    TAU::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}),
            &UPLO, &N, &M, &P, R, &LDR, A, &LDA, B, &LDB, C, &LDC,
            TAU, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ny!(M::Integer, N::Integer, V::Array{Float64,1},
    INCV::Integer, TAU::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04ny_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64}),
            &M, &N, V, &INCV, &TAU, A, &LDA, B, &LDB, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04NY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04od!(UPLO::Char, N::Integer, M::Integer, P::Integer,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    TAU::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}),
            &UPLO, &N, &M, &P, R, &LDR, A, &LDA, B, &LDB, C, &LDC,
            TAU, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ow!(M::Integer, N::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    T::Array{Float64,2}, LDT::Integer,
    X::Array{Float64,1}, INCX::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,1}, INCD::Integer)

    INFO = [0]

    ccall((:mb04ow_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &M, &N, &P, A, &LDA, T, &LDT, X, &INCX,
            B, &LDB, C, &LDC, D, &INCD)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04OW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ox!(N::Integer, A::Array{Float64,2}, LDA::Integer,
    X::Array{Float64,1}, INCX::Integer)

    INFO = [0]

    ccall((:mb04ox_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}), &N, A, &LDA,
            X, &INCX)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04OX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04oy!(M::Integer, N::Integer, V::Array{Float64,1},
    TAU::FloatingPoint, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04oy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}), &M, &N, V,
            &TAU, A, &LDA, B, &LDB, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04OY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04pa!(LHAM::Bool, N::Integer, K::Integer, NB::Integer,
    A::Array{Float64,2}, LDA::Integer,
    QG::Array{Float64,2}, LDQG::Integer,
    XA::Array{Float64,2}, LDXA::Integer,
    XG::Array{Float64,2}, LDXG::Integer,
    XQ::Array{Float64,2}, LDXQ::Integer,
    YA::Array{Float64,2}, LDYA::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04pa_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}), &LHAM, &N, &K, &NB, A, &LDA, QG, &LDQG,
            XA, &LDXA, XG, &LDXG, XQ, &LDXQ, YA, &LDYA, CS, TAU,
            DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04PA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04pb!(N::Integer, ILO::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04pb_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &ILO, A, &LDA, QG, &LDQG, CS, TAU,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04PB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04pu!(N::Integer, ILO::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04pu_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &ILO, A, &LDA, QG, &LDQG, CS, TAU,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04PU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04py!(SIDE::Char, M::Integer, N::Integer,
    V::Array{Float64,1}, TAU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04py_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}), &SIDE, &M, &N, V, &TAU, C,
            &LDC, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04PY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04qb!(TRANC::Char, TRAND::Char, TRANQ::Char, STOREV::Char,
    STOREW::Char, M::Integer, N::Integer, K::Integer,
    V::Array{Float64,2}, LDV::Integer,
    W::Array{Float64,2}, LDW::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04qb_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &TRANC,
            &TRAND, &TRANQ, &STOREV, &STOREW, &M, &N, &K, V, &LDV,
            W, &LDW, C, &LDC, D, &LDD, CS, TAU, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04QB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04qc!(STRUCT::Char, TRANA::Char, TRANB::Char, TRANQ::Char,
    DIRECT::Char, STOREV::Char, STOREW::Char, M::Integer, N::Integer,
    K::Integer, V::Array{Float64,2}, LDV::Integer,
    W::Array{Float64,2}, LDW::Integer,
    RS::Array{Float64,2}, LDRS::Integer,
    T::Array{Float64,2}, LDT::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04qc_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}),
            &STRUCT, &TRANA, &TRANB, &TRANQ, &DIRECT, &STOREV,
            &STOREW, &M, &N, &K, V, &LDV, W, &LDW, RS, &LDRS, T,
            &LDT, A, &LDA, B, &LDB, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04QC: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04qf!(DIRECT::Char, STOREV::Char, STOREW::Char, N::Integer,
    K::Integer, V::Array{Float64,2}, LDV::Integer,
    W::Array{Float64,2}, LDW::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    RS::Array{Float64,2}, LDRS::Integer,
    T::Array{Float64,2}, LDT::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04qf_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}), &DIRECT, &STOREV, &STOREW,
            &N, &K, V, &LDV, W, &LDW, CS, TAU, RS, &LDRS, T, &LDT,
            DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04QF: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04qu!(TRANC::Char, TRAND::Char, TRANQ::Char, STOREV::Char,
    STOREW::Char, M::Integer, N::Integer, K::Integer,
    V::Array{Float64,2}, LDV::Integer,
    W::Array{Float64,2}, LDW::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04qu_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &TRANC,
            &TRAND, &TRANQ, &STOREV, &STOREW, &M, &N, &K, V, &LDV,
            W, &LDW, C, &LDC, D, &LDD, CS, TAU, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04QU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04su!(M::Integer, N::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04su_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &M, &N, A, &LDA, B, &LDB, CS, TAU, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04SU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04tb!(TRANA::Char, TRANB::Char, N::Integer, ILO::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    CSL::Array{Float64,1}, CSR::Array{Float64,1},
    TAUL::Array{Float64,1}, TAUR::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04tb_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &TRANA, &TRANB, &N, &ILO, A, &LDA, B,
            &LDB, G, &LDG, Q, &LDQ, CSL, CSR, TAUL, TAUR, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ts!(TRANA::Char, TRANB::Char, N::Integer, ILO::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    CSL::Array{Float64,1}, CSR::Array{Float64,1},
    TAUL::Array{Float64,1}, TAUR::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04ts_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &TRANA, &TRANB, &N, &ILO, A, &LDA, B,
            &LDB, G, &LDG, Q, &LDQ, CSL, CSR, TAUL, TAUR, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TS: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04tt!(UPDATQ::Bool, UPDATZ::Bool, M::Integer, N::Integer,
    IFIRA::Integer, IFICA::Integer, NCA::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer, ISTAIR::Array{BlasInt,1},
    RANK::Integer, TOL::FloatingPoint, IWORK::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb04tt_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &UPDATQ, &UPDATZ, &M, &N, &IFIRA, &IFICA,
            &NCA, A, &LDA, E, &LDE, Q, &LDQ, Z, &LDZ, ISTAIR, &RANK,
            &TOL, IWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TT: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04tu!(N::Integer, X::Array{Float64,1}, INCX::Integer,
    Y::Array{Float64,1}, INCY::Integer, C::FloatingPoint,
    S::FloatingPoint)

    INFO = [0]

    ccall((:mb04tu_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}), &N, X, &INCX, Y, &INCY, &C, &S)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04tv!(UPDATZ::Bool, N::Integer, NRA::Integer, NCA::Integer,
    IFIRA::Integer, IFICA::Integer, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    Z::Array{Float64,2}, LDZ::Integer)

    INFO = [0]

    ccall((:mb04tv_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &UPDATZ, &N, &NRA, &NCA,
            &IFIRA, &IFICA, A, &LDA, E, &LDE, Z, &LDZ)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04tw!(UPDATQ::Bool, M::Integer, N::Integer, NRE::Integer,
    NCE::Integer, IFIRE::Integer, IFICE::Integer, IFICA::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer)

    INFO = [0]

    ccall((:mb04tw_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &UPDATQ, &M, &N, &NRE, &NCE, &IFIRE, &IFICE, &IFICA, A,
            &LDA, E, &LDE, Q, &LDQ)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04tx!(UPDATQ::Bool, UPDATZ::Bool, M::Integer, N::Integer,
    NBLCKS::Integer, INUK::Array{BlasInt,1}, IMUK::Array{BlasInt,1},
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer, MNEI::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb04tx_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &UPDATQ, &UPDATZ, &M, &N,
            &NBLCKS, INUK, IMUK, A, &LDA, E, &LDE, Q, &LDQ, Z, &LDZ,
            MNEI)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ty!(UPDATQ::Bool, UPDATZ::Bool, M::Integer, N::Integer,
    NBLCKS::Integer, INUK::Array{BlasInt,1}, IMUK::Array{BlasInt,1},
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer)

    INFO = [0]

    ccall((:mb04ty_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &UPDATQ, &UPDATZ, &M, &N,
            &NBLCKS, INUK, IMUK, A, &LDA, E, &LDE, Q, &LDQ, Z, &LDZ,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04TY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04ud!(JOBQ::Char, JOBZ::Char, M::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer, RANKE::Integer,
    ISTAIR::Array{BlasInt,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04ud_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &JOBQ, &JOBZ,
            &M, &N, A, &LDA, E, &LDE, Q, &LDQ, Z, &LDZ, &RANKE,
            ISTAIR, &TOL, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04vd!(MODE::Char, JOBQ::Char, JOBZ::Char, M::Integer,
    N::Integer, RANKE::Integer, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer, ISTAIR::Array{BlasInt,1},
    NBLCKS::Integer, NBLCKI::Integer, IMUK::Array{BlasInt,1},
    INUK::Array{BlasInt,1}, IMUK0::Array{BlasInt,1},
    MNEI::Array{BlasInt,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb04vd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &MODE, &JOBQ, &JOBZ, &M,
            &N, &RANKE, A, &LDA, E, &LDE, Q, &LDQ, Z, &LDZ, ISTAIR,
            &NBLCKS, &NBLCKI, IMUK, INUK, IMUK0, MNEI, &TOL, IWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04vx!(UPDATQ::Bool, UPDATZ::Bool, M::Integer, N::Integer,
    NBLCKS::Integer, INUK::Array{BlasInt,1}, IMUK::Array{BlasInt,1},
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer, MNEI::Array{BlasInt,1})

    INFO = [0]

    ccall((:mb04vx_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &UPDATQ, &UPDATZ, &M, &N,
            &NBLCKS, INUK, IMUK, A, &LDA, E, &LDE, Q, &LDQ, Z, &LDZ,
            MNEI)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04VX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04wd!(TRANQ1::Char, TRANQ2::Char, M::Integer, N::Integer,
    K::Integer, Q1::Array{Float64,2}, LDQ1::Integer,
    Q2::Array{Float64,2}, LDQ2::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04wd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TRANQ1, &TRANQ2, &M, &N, &K, Q1, &LDQ1, Q2, &LDQ2, CS,
            TAU, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04wp!(N::Integer, ILO::Integer, U1::Array{Float64,2},
    LDU1::Integer, U2::Array{Float64,2}, LDU2::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04wp_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &ILO, U1, &LDU1, U2, &LDU2, CS, TAU,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04WP: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04wr!(JOB::Char, TRANS::Char, N::Integer, ILO::Integer,
    Q1::Array{Float64,2}, LDQ1::Integer,
    Q2::Array{Float64,2}, LDQ2::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04wr_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &TRANS,
            &N, &ILO, Q1, &LDQ1, Q2, &LDQ2, CS, TAU, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04WR: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04wu!(TRANQ1::Char, TRANQ2::Char, M::Integer, N::Integer,
    K::Integer, Q1::Array{Float64,2}, LDQ1::Integer,
    Q2::Array{Float64,2}, LDQ2::Integer,
    CS::Array{Float64,1}, TAU::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb04wu_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TRANQ1, &TRANQ2, &M, &N, &K, Q1, &LDQ1, Q2, &LDQ2, CS,
            TAU, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04WU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04xd!(JOBU::Char, JOBV::Char, M::Integer, N::Integer,
    RANK::Integer, THETA::FloatingPoint, A::Array{Float64,2},
    LDA::Integer, U::Array{Float64,2}, LDU::Integer,
    V::Array{Float64,2}, LDV::Integer,
    Q::Array{Float64,1}, INUL::Array{Bool,1},
    TOL::FloatingPoint, RELTOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:mb04xd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Bool},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBU, &JOBV, &M, &N,
            &RANK, &THETA, A, &LDA, U, &LDU, V, &LDV, Q, INUL, &TOL,
            &RELTOL, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04XD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04xy!(JOBU::Char, JOBV::Char, M::Integer, N::Integer,
    X::Array{Float64,2}, LDX::Integer,
    TAUP::Array{Float64,1}, TAUQ::Array{Float64,1},
    U::Array{Float64,2}, LDU::Integer,
    V::Array{Float64,2}, LDV::Integer, INUL::Array{Bool,1})

    INFO = [0]

    ccall((:mb04xy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}),
            &JOBU, &JOBV, &M, &N, X, &LDX, TAUP, TAUQ, U, &LDU, V,
            &LDV, INUL, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04XY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04yd!(JOBU::Char, JOBV::Char, M::Integer, N::Integer,
    RANK::Integer, THETA::FloatingPoint, Q::Array{Float64,1},
    E::Array{Float64,1}, U::Array{Float64,2},
    LDU::Integer, V::Array{Float64,2}, LDV::Integer,
    INUL::Array{Bool,1}, TOL::FloatingPoint, RELTOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:mb04yd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOBU, &JOBV, &M, &N, &RANK, &THETA, Q,
            E, U, &LDU, V, &LDV, INUL, &TOL, &RELTOL, DWORK,
            &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04YD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04yw!(QRIT::Bool, UPDATU::Bool, UPDATV::Bool, M::Integer,
    N::Integer, L::Integer, K::Integer, SHIFT::FloatingPoint,
    D::Array{Float64,1}, E::Array{Float64,1},
    U::Array{Float64,1}, LDU::FloatingPoint,
    V::Array{Float64,1}, LDV::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04yw_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}), &QRIT, &UPDATU, &UPDATV, &M, &N, &L, &K,
            &SHIFT, D, E, U, &LDU, V, &LDV, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04YW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb04zd!(COMPU::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, QG::Array{Float64,2}, LDQG::Integer,
    U::Array{Float64,2}, LDU::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mb04zd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &COMPU, &N, A, &LDA, QG, &LDQG, U, &LDU, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB04ZD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb05md!(BALANC::Char, N::Integer, DELTA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    V::Array{Float64,2}, LDV::Integer,
    Y::Array{Float64,2}, LDY::Integer,
    VALR::Array{Float64,1}, VALI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:mb05md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &BALANC, &N, &DELTA, A, &LDA, V, &LDV, Y,
            &LDY, VALR, VALI, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB05MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb05my!(BALANC::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, WR::Array{Float64,1},
    WI::Array{Float64,1}, R::Array{Float64,1},
    LDR::FloatingPoint, Q::Array{Float64,1}, LDQ::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb05my_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &BALANC, &N,
            A, &LDA, WR, WI, R, &LDR, Q, &LDQ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB05MY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb05nd!(N::Integer, DELTA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    EX::Array{Float64,2}, LDEX::Integer,
    EXINT::Array{Float64,2}, LDEXIN::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mb05nd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &DELTA,
            A, &LDA, EX, &LDEX, EXINT, &LDEXIN, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB05ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb05od!(BALANC::Char, N::Integer, NDIAG::Integer,
    DELTA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    MDIG::Integer, IDIG::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:mb05od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &BALANC, &N,
            &NDIAG, &DELTA, A, &LDA, &MDIG, &IDIG, IWORK, DWORK,
            &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB05OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb05oy!(JOB::Char, N::Integer, LOW::Integer, IGH::Integer,
    A::Array{Float64,2}, LDA::Integer,
    SCALE::Array{Float64,1})

    INFO = [0]

    ccall((:mb05oy_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &JOB, &N, &LOW, &IGH, A,
            &LDA, SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB05OY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb3oyz!(M::Integer, N::Integer, A::Array{Complex,1},
    LDA::Complex, RCOND::FloatingPoint, SVLMAX::FloatingPoint,
    RANK::Integer, SVAL::Array{Float64,1},
    JPVT::Array{BlasInt,1}, TAU::Array{Complex,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1})

    INFO = [0]

    ccall((:mb3oyz_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Float64}, Ptr{Complex128},
            Ptr{BlasInt}), &M, &N, A, &LDA, &RCOND, &SVLMAX, &RANK,
            SVAL, JPVT, TAU, DWORK, ZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB3OYZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mb3pyz!(M::Integer, N::Integer, A::Array{Complex,1},
    LDA::Complex, RCOND::FloatingPoint, SVLMAX::FloatingPoint,
    RANK::Integer, SVAL::Array{Float64,1},
    JPVT::Array{BlasInt,1}, TAU::Array{Complex,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1})

    INFO = [0]

    ccall((:mb3pyz_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Float64}, Ptr{Complex128},
            Ptr{BlasInt}), &M, &N, A, &LDA, &RCOND, &SVLMAX, &RANK,
            SVAL, JPVT, TAU, DWORK, ZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MB3PYZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01md!(DP::Integer, ALPHA::FloatingPoint, K::Integer,
    P::Array{Float64,1}, Q::Array{Float64,1})

    INFO = [0]

    ccall((:mc01md_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &DP, &ALPHA, &K, P, Q, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01nd!(DP::Integer, XR::FloatingPoint, XI::FloatingPoint,
    P::Array{Float64,1}, VR::FloatingPoint, VI::FloatingPoint)

    INFO = [0]

    ccall((:mc01nd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &DP, &XR, &XI, P, &VR, &VI, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01od!(K::Integer, REZ::Array{Float64,1},
    IMZ::Array{Float64,1}, REP::Array{Float64,1},
    IMP::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mc01od_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &K, REZ, IMZ, REP, IMP, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01pd!(K::Integer, REZ::Array{Float64,1},
    IMZ::Array{Float64,1}, P::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mc01pd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &K, REZ, IMZ, P, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01py!(K::Integer, REZ::Array{Float64,1},
    IMZ::Array{Float64,1}, P::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mc01py_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &K, REZ, IMZ, P, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01PY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01qd!(DA::Integer, DB::Integer, A::Array{Float64,1},
    B::Array{Float64,1}, RQ::Array{Float64,1},
    IWARN::Integer)

    INFO = [0]

    ccall((:mc01qd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &DA, &DB, A, B, RQ, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01rd!(DP1::Integer, DP2::Integer, DP3::Integer,
    ALPHA::FloatingPoint, P1::Array{Float64,1},
    P2::Array{Float64,1}, P3::Array{Float64,1})

    INFO = [0]

    ccall((:mc01rd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &DP1, &DP2, &DP3, &ALPHA,
            P1, P2, P3, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01sd!(DP::Integer, P::Array{Float64,1}, S::Integer,
    T::Integer, MANT::Array{Float64,1}, E::Array{BlasInt,1},
    IWORK::Array{BlasInt,1})

    INFO = [0]

    ccall((:mc01sd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DP, P, &S, &T, MANT, E,
            IWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01sw!(A::FloatingPoint, B::Integer, M::FloatingPoint,
    E::Integer)

    INFO = [0]

    ccall((:mc01sw_, libslicot), Void, (Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &A, &B, &M, &E)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01SW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01sy!(M::FloatingPoint, E::Integer, B::Integer,
    A::FloatingPoint, OVFLOW::Bool)

    INFO = [0]

    ccall((:mc01sy_, libslicot), Void, (Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Bool}), &M, &E, &B, &A,
            &OVFLOW)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01SY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01td!(DICO::Char, DP::Integer, P::Array{Float64,1},
    STABLE::Bool, NZ::Integer, DWORK::Array{Float64,1},
    IWARN::Integer)

    INFO = [0]

    ccall((:mc01td_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Bool}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &DP, P, &STABLE,
            &NZ, DWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01vd!(A::FloatingPoint, B::FloatingPoint, C::FloatingPoint,
    Z1RE::FloatingPoint, Z1IM::FloatingPoint, Z2RE::FloatingPoint,
    Z2IM::FloatingPoint)

    INFO = [0]

    ccall((:mc01vd_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &A, &B, &C, &Z1RE, &Z1IM,
            &Z2RE, &Z2IM, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc01wd!(DP::Integer, P::Array{Float64,1},
    U1::FloatingPoint, U2::FloatingPoint, Q::Array{Float64,1})

    INFO = [0]

    ccall((:mc01wd_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &DP, P, &U1, &U2, Q, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC01WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc03md!(RP1::Integer, CP1::Integer, CP2::Integer,
    DP1::Integer, DP2::Integer, DP3::Integer, ALPHA::FloatingPoint,
    P1::Array{Float64,3}, LDP11::Integer, LDP12::Integer,
    P2::Array{Float64,3}, LDP21::Integer, LDP22::Integer,
    P3::Array{Float64,3}, LDP31::Integer, LDP32::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:mc03md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &RP1, &CP1, &CP2, &DP1, &DP2, &DP3, &ALPHA, P1, &LDP11,
            &LDP12, P2, &LDP21, &LDP22, P3, &LDP31, &LDP32, DWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC03MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc03nd!(MP::Integer, NP::Integer, DP::Integer,
    P::Array{Float64,3}, LDP1::Integer, LDP2::Integer,
    DK::Integer, GAM::Array{BlasInt,1},
    NULLSP::Array{Float64,2}, LDNULL::Integer,
    KER::Array{Float64,3}, LDKER1::Integer, LDKER2::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:mc03nd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &MP, &NP, &DP, P, &LDP1, &LDP2, &DK, GAM, NULLSP,
            &LDNULL, KER, &LDKER1, &LDKER2, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC03ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc03nx!(MP::Integer, NP::Integer, DP::Integer,
    P::Array{Float64,3}, LDP1::Integer, LDP2::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer)

    INFO = [0]

    ccall((:mc03nx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &MP, &NP, &DP, P, &LDP1, &LDP2, A, &LDA, E, &LDE)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC03NX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function mc03ny!(NBLCKS::Integer, NRA::Integer, NCA::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer, IMUK::Array{BlasInt,1},
    INUK::Array{BlasInt,1}, VEPS::Array{Float64,2},
    LDVEPS::Integer)

    INFO = [0]

    ccall((:mc03ny_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &NBLCKS, &NRA, &NCA, A,
            &LDA, E, &LDE, IMUK, INUK, VEPS, &LDVEPS, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MC03NY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function md03ba!(N::Integer, IPAR::Array{BlasInt,1}, LIPAR::Integer,
    FNORM::FloatingPoint, J::Array{Float64,1}, LDJ::Integer,
    E::Array{Float64,1}, JNORMS::Array{Float64,1},
    GNORM::FloatingPoint, IPVT::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:md03ba_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, IPAR,
            &LIPAR, &FNORM, J, &LDJ, E, JNORMS, &GNORM, IPVT, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MD03BA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function md03bb!(COND::Char, N::Integer, IPAR::Array{BlasInt,1},
    LIPAR::Integer, R::Array{Float64,2}, LDR::Integer,
    IPVT::Array{BlasInt,1}, DIAG::Array{Float64,1},
    QTB::Array{Float64,1}, DELTA::FloatingPoint,
    PAR::FloatingPoint, RANKS::Array{BlasInt,1},
    X::Array{Float64,1}, RX::Array{Float64,1},
    TOL::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:md03bb_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &COND, &N, IPAR, &LIPAR, R, &LDR, IPVT, DIAG, QTB,
            &DELTA, &PAR, RANKS, X, RX, &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MD03BB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function md03bf!(IFLAG::Integer, M::Integer, N::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer,
    DPAR1::Array{Float64,1}, LDPAR1::Integer,
    DPAR2::Array{Float64,1}, LDPAR2::Integer,
    X::Array{Float64,1}, NFEVL::Integer,
    E::Array{Float64,1}, J::Array{Float64,2},
    LDJ::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:md03bf_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &IFLAG, &M,
            &N, IPAR, &LIPAR, DPAR1, &LDPAR1, DPAR2, &LDPAR2, X,
            &NFEVL, E, J, &LDJ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MD03BF: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function md03bx!(M::Integer, N::Integer, FNORM::FloatingPoint,
    J::Array{Float64,1}, LDJ::Integer,
    E::Array{Float64,1}, JNORMS::Array{Float64,1},
    GNORM::FloatingPoint, IPVT::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:md03bx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &M, &N, &FNORM, J, &LDJ, E,
            JNORMS, &GNORM, IPVT, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MD03BX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function md03by!(COND::Char, N::Integer, R::Array{Float64,2},
    LDR::Integer, IPVT::Array{BlasInt,1},
    DIAG::Array{Float64,1}, QTB::Array{Float64,1},
    DELTA::FloatingPoint, PAR::FloatingPoint, RANK::Integer,
    X::Array{Float64,1}, RX::Array{Float64,1},
    TOL::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:md03by_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &COND, &N, R, &LDR, IPVT,
            DIAG, QTB, &DELTA, &PAR, &RANK, X, RX, &TOL, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in MD03BY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01ad!(NSMP::Integer, M::Integer, L::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, X::Array{Float64,1},
    LX::Integer, U::Array{Float64,2}, LDU::Integer,
    Y::Array{Float64,2}, LDY::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01ad_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &NSMP, &M, &L, IPAR, &LIPAR, X, &LX, U, &LDU, Y, &LDY,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01ay!(NSMP::Integer, NZ::Integer, L::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer,
    WB::Array{Float64,1}, LWB::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    Y::Array{Float64,2}, LDY::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01ay_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &NSMP, &NZ, &L, IPAR, &LIPAR, WB, &LWB, Z, &LDZ, Y,
            &LDY, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01AY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01ba!(IFLAG::Integer, NSMP::Integer, N::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, Z::Array{Float64,2},
    LDZ::Integer, Y::Array{Float64,2}, LDY::Integer,
    X::Array{Float64,1}, NFEVL::Integer,
    E::Array{Float64,1}, J::Array{Float64,2},
    LDJ::Integer, JTE::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01ba_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &IFLAG, &NSMP, &N, IPAR, &LIPAR, Z, &LDZ, Y, &LDY, X,
            &NFEVL, E, J, &LDJ, JTE, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BA: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bb!(IFLAG::Integer, NFUN::Integer, LX::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, U::Array{Float64,2},
    LDU::Integer, Y::Array{Float64,2}, LDY::Integer,
    X::Array{Float64,1}, NFEVL::Integer,
    E::Array{Float64,1}, J::Array{Float64,2},
    LDJ::Integer, JTE::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bb_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &IFLAG, &NFUN, &LX, IPAR, &LIPAR, U, &LDU, Y, &LDY, X,
            &NFEVL, E, J, &LDJ, JTE, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BB: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bd!(CJTE::Char, NSMP::Integer, M::Integer, L::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, X::Array{Float64,1},
    LX::Integer, U::Array{Float64,2}, LDU::Integer,
    E::Array{Float64,1}, J::Array{Float64,1},
    LDJ::Integer, JTE::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &CJTE, &NSMP,
            &M, &L, IPAR, &LIPAR, X, &LX, U, &LDU, E, J, &LDJ, JTE,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01be!(IFLAG::Integer, NSMP::Integer, N::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, Z::Array{Float64,2},
    LDZ::Integer, Y::Array{Float64,2}, LDY::Integer,
    X::Array{Float64,1}, NFEVL::Integer,
    E::Array{Float64,1}, J::Array{Float64,2},
    LDJ::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01be_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &IFLAG,
            &NSMP, &N, IPAR, &LIPAR, Z, &LDZ, Y, &LDY, X, &NFEVL, E,
            J, &LDJ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BE: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bf!(IFLAG::Integer, NFUN::Integer, LX::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, U::Array{Float64,2},
    LDU::Integer, Y::Array{Float64,2}, LDY::Integer,
    X::Array{Float64,1}, NFEVL::Integer,
    E::Array{Float64,1}, J::Array{Float64,2},
    LDJ::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bf_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &IFLAG,
            &NFUN, &LX, IPAR, &LIPAR, U, &LDU, Y, &LDY, X, &NFEVL,
            E, J, &LDJ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BF: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bp!(COND::Char, N::Integer, IPAR::Array{BlasInt,1},
    LIPAR::Integer, R::Array{Float64,2}, LDR::Integer,
    IPVT::Array{BlasInt,1}, DIAG::Array{Float64,1},
    QTB::Array{Float64,1}, DELTA::FloatingPoint,
    PAR::FloatingPoint, RANKS::Array{BlasInt,1},
    X::Array{Float64,1}, RX::Array{Float64,1},
    TOL::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bp_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &COND, &N, IPAR, &LIPAR, R, &LDR, IPVT, DIAG, QTB,
            &DELTA, &PAR, RANKS, X, RX, &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BP: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bq!(COND::Char, N::Integer, IPAR::Array{BlasInt,1},
    LIPAR::Integer, R::Array{Float64,2}, LDR::Integer,
    IPVT::Array{BlasInt,1}, DIAG::Array{Float64,1},
    QTB::Array{Float64,1}, RANKS::Array{BlasInt,1},
    X::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bq_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &COND, &N, IPAR, &LIPAR, R, &LDR, IPVT,
            DIAG, QTB, RANKS, X, &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BQ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01br!(COND::Char, UPLO::Char, TRANS::Char, N::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer, R::Array{Float64,2},
    LDR::Integer, SDIAG::Array{Float64,1},
    S::Array{Float64,2}, LDS::Integer,
    B::Array{Float64,1}, RANKS::Array{BlasInt,1},
    TOL::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:nf01br_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &COND, &UPLO,
            &TRANS, &N, IPAR, &LIPAR, R, &LDR, SDIAG, S, &LDS, B,
            RANKS, &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BR: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bs!(N::Integer, IPAR::Array{BlasInt,1}, LIPAR::Integer,
    FNORM::FloatingPoint, J::Array{Float64,1}, LDJ::Integer,
    E::Array{Float64,1}, JNORMS::Array{Float64,1},
    GNORM::FloatingPoint, IPVT::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bs_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, IPAR,
            &LIPAR, &FNORM, J, &LDJ, E, JNORMS, &GNORM, IPVT, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BS: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bu!(STOR::Char, UPLO::Char, N::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer,
    DPAR::Array{Float64,1}, LDPAR::Integer,
    J::Array{Float64,2}, LDJ::Integer,
    JTJ::Array{Float64,1}, LDJTJ::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bu_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &STOR, &UPLO, &N, IPAR, &LIPAR, DPAR, &LDPAR, J, &LDJ,
            JTJ, &LDJTJ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bv!(STOR::Char, UPLO::Char, N::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer,
    DPAR::Array{Float64,1}, LDPAR::Integer,
    J::Array{Float64,2}, LDJ::Integer,
    JTJ::Array{Float64,1}, LDJTJ::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bv_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &STOR, &UPLO, &N, IPAR, &LIPAR, DPAR, &LDPAR, J, &LDJ,
            JTJ, &LDJTJ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bw!(N::Integer, IPAR::Array{BlasInt,1}, LIPAR::Integer,
    DPAR::Array{Float64,1}, LDPAR::Integer,
    J::Array{Float64,2}, LDJ::Integer,
    X::Array{Float64,1}, INCX::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bw_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, IPAR, &LIPAR, DPAR,
            &LDPAR, J, &LDJ, X, &INCX, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01bx!(N::Integer, IPAR::Array{BlasInt,1}, LIPAR::Integer,
    DPAR::Array{Float64,1}, LDPAR::Integer,
    J::Array{Float64,2}, LDJ::Integer,
    X::Array{Float64,1}, INCX::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01bx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, IPAR, &LIPAR, DPAR,
            &LDPAR, J, &LDJ, X, &INCX, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function nf01by!(CJTE::Char, NSMP::Integer, NZ::Integer, L::Integer,
    IPAR::Array{BlasInt,1}, LIPAR::Integer,
    WB::Array{Float64,1}, LWB::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    E::Array{Float64,1}, J::Array{Float64,2},
    LDJ::Integer, JTE::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:nf01by_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &CJTE, &NSMP,
            &NZ, &L, IPAR, &LIPAR, WB, &LWB, Z, &LDZ, E, J, &LDJ,
            JTE, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in NF01BY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb01bd!(DICO::Char, N::Integer, M::Integer, NP::Integer,
    ALPHA::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    NFP::Integer, NAP::Integer, NUP::Integer,
    F::Array{Float64,2}, LDF::Integer,
    Z::Array{Float64,2}, LDZ::Integer, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb01bd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &N, &M, &NP, &ALPHA, A, &LDA, B,
            &LDB, WR, WI, &NFP, &NAP, &NUP, F, &LDF, Z, &LDZ, &TOL,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB01BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb01bx!(REIG::Bool, N::Integer, XR::FloatingPoint,
    XI::FloatingPoint, WR::Array{Float64,1},
    WI::Array{Float64,1}, S::FloatingPoint, P::FloatingPoint)

    INFO = [0]

    ccall((:sb01bx_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}), &REIG, &N, &XR, &XI, WR,
            WI, &S, &P)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB01BX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb01by!(N::Integer, M::Integer, S::FloatingPoint,
    P::FloatingPoint, A::Array{Float64,2},
    B::Array{Float64,2}, F::Array{Float64,2},
    TOL::FloatingPoint, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb01by_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &S, &P, A, B, F, &TOL, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB01BY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb01dd!(N::Integer, M::Integer, INDCON::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    NBLK::Array{BlasInt,1}, WR::Array{Float64,1},
    WI::Array{Float64,1}, Z::Array{Float64,1},
    LDZ::FloatingPoint, Y::Array{Float64,1}, COUNT::Integer,
    G::Array{Float64,1}, LDG::FloatingPoint, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb01dd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &M,
            &INDCON, A, &LDA, B, &LDB, NBLK, WR, WI, Z, &LDZ, Y,
            &COUNT, G, &LDG, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB01DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb01fy!(DISCR::Bool, N::Integer, M::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    F::Array{Float64,2}, LDF::Integer,
    V::Array{Float64,2}, LDV::Integer)

    INFO = [0]

    ccall((:sb01fy_, libslicot), Void, (Ptr{Bool}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DISCR, &N, &M, A, &LDA, B,
            &LDB, F, &LDF, V, &LDV, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB01FY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb01md!(NCONT::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,1}, WR::Array{Float64,1},
    WI::Array{Float64,1}, Z::Array{Float64,2},
    LDZ::Integer, G::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb01md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &NCONT, &N, A, &LDA, B, WR,
            WI, Z, &LDZ, G, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02md!(DICO::Char, HINV::Char, UPLO::Char, SCAL::Char,
    SORT::Char, N::Integer, A::Array{Float64,2}, LDA::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer, RCOND::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    S::Array{Float64,2}, LDS::Integer,
    U::Array{Float64,2}, LDU::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb02md_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}), &DICO, &HINV, &UPLO, &SCAL,
            &SORT, &N, A, &LDA, G, &LDG, Q, &LDQ, &RCOND, WR, WI, S,
            &LDS, U, &LDU, IWORK, DWORK, &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02mt!(JOBG::Char, JOBL::Char, FACT::Char, UPLO::Char,
    N::Integer, M::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer,
    L::Array{Float64,2}, LDL::Integer, IPIV::Array{BlasInt,1},
    OUFACT::Integer, G::Array{Float64,2}, LDG::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb02mt_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBG, &JOBL, &FACT, &UPLO,
            &N, &M, A, &LDA, B, &LDB, Q, &LDQ, R, &LDR, L, &LDL,
            IPIV, &OUFACT, G, &LDG, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02MT: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02mu!(DICO::Char, HINV::Char, UPLO::Char, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    S::Array{Float64,2}, LDS::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb02mu_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &HINV, &UPLO, &N, A,
            &LDA, G, &LDG, Q, &LDQ, S, &LDS, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02MU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02nd!(DICO::Char, FACT::Char, UPLO::Char, JOBL::Char,
    N::Integer, M::Integer, P::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    R::Array{Float64,2}, LDR::Integer, IPIV::Array{BlasInt,1},
    L::Array{Float64,2}, LDL::Integer,
    X::Array{Float64,2}, LDX::Integer, RNORM::FloatingPoint,
    F::Array{Float64,2}, LDF::Integer, OUFACT::Array{BlasInt,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb02nd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &FACT, &UPLO, &JOBL, &N, &M, &P, A, &LDA, B,
            &LDB, R, &LDR, IPIV, L, &LDL, X, &LDX, &RNORM, F, &LDF,
            OUFACT, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02od!(DICO::Char, JOBB::Char, FACT::Char, UPLO::Char,
    JOBL::Char, SORT::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer,
    L::Array{Float64,2}, LDL::Integer, RCOND::FloatingPoint,
    X::Array{Float64,2}, LDX::Integer,
    ALFAR::Array{Float64,1}, ALFAI::Array{Float64,1},
    BETA::Array{Float64,1}, S::Array{Float64,2},
    LDS::Integer, T::Array{Float64,2}, LDT::Integer,
    U::Array{Float64,2}, LDU::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb02od_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}), &DICO, &JOBB,
            &FACT, &UPLO, &JOBL, &SORT, &N, &M, &P, A, &LDA, B,
            &LDB, Q, &LDQ, R, &LDR, L, &LDL, &RCOND, X, &LDX, ALFAR,
            ALFAI, BETA, S, &LDS, T, &LDT, U, &LDU, &TOL, IWORK,
            DWORK, &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02oy!(TYPE::Char, DICO::Char, JOBB::Char, FACT::Char,
    UPLO::Char, JOBL::Char, JOBE::Char, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer,
    L::Array{Float64,2}, LDL::Integer,
    E::Array{Float64,2}, LDE::Integer,
    AF::Array{Float64,2}, LDAF::Integer,
    BF::Array{Float64,2}, LDBF::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb02oy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &TYPE, &DICO, &JOBB, &FACT, &UPLO, &JOBL, &JOBE, &N, &M,
            &P, A, &LDA, B, &LDB, Q, &LDQ, R, &LDR, L, &LDL, E,
            &LDE, AF, &LDAF, BF, &LDBF, &TOL, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02OY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02pd!(JOB::Char, TRANA::Char, UPLO::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    G::Array{Float64,1}, LDG::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    RCOND::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb02pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &TRANA, &UPLO, &N, A,
            &LDA, G, &LDG, Q, &LDQ, X, &LDX, &RCOND, &FERR, WR, WI,
            IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02qd!(JOB::Char, FACT::Char, TRANA::Char, UPLO::Char,
    LYAPUN::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    G::Array{Float64,1}, LDG::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint, SEP::FloatingPoint,
    RCOND::FloatingPoint, FERR::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb02qd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &FACT,
            &TRANA, &UPLO, &LYAPUN, &N, A, &LDA, T, &LDT, U, &LDU,
            G, &LDG, Q, &LDQ, X, &LDX, &SEP, &RCOND, &FERR, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02rd!(JOB::Char, DICO::Char, HINV::Char, TRANA::Char,
    UPLO::Char, SCAL::Char, SORT::Char, FACT::Char, LYAPUN::Char,
    N::Integer, A::Array{Float64,2}, LDA::Integer,
    T::Array{Float64,2}, LDT::Integer,
    V::Array{Float64,2}, LDV::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    X::Array{Float64,2}, LDX::Integer, SEP::FloatingPoint,
    RCOND::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    S::Array{Float64,2}, LDS::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb02rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}), &JOB, &DICO,
            &HINV, &TRANA, &UPLO, &SCAL, &SORT, &FACT, &LYAPUN, &N,
            A, &LDA, T, &LDT, V, &LDV, G, &LDG, Q, &LDQ, X, &LDX,
            &SEP, &RCOND, &FERR, WR, WI, S, &LDS, IWORK, DWORK,
            &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02ru!(DICO::Char, HINV::Char, TRANA::Char, UPLO::Char,
    N::Integer, A::Array{Float64,2}, LDA::Integer,
    G::Array{Float64,2}, LDG::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    S::Array{Float64,2}, LDS::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb02ru_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &HINV,
            &TRANA, &UPLO, &N, A, &LDA, G, &LDG, Q, &LDQ, S, &LDS,
            IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02RU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb02sd!(JOB::Char, FACT::Char, TRANA::Char, UPLO::Char,
    LYAPUN::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    G::Array{Float64,1}, LDG::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    SEPD::FloatingPoint, RCOND::FloatingPoint, FERR::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb02sd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &FACT,
            &TRANA, &UPLO, &LYAPUN, &N, A, &LDA, T, &LDT, U, &LDU,
            G, &LDG, Q, &LDQ, X, &LDX, &SEPD, &RCOND, &FERR, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB02SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03md!(DICO::Char, JOB::Char, FACT::Char, TRANA::Char,
    N::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint, SEP::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03md_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOB, &FACT, &TRANA,
            &N, A, &LDA, U, &LDU, C, &LDC, &SCALE, &SEP, &FERR, WR,
            WI, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03mu!(LTRANL::Bool, LTRANR::Bool, ISGN::Integer,
    N1::Integer, N2::Integer, TL::Array{Float64,1},
    LDTL::FloatingPoint, TR::Array{Float64,1},
    LDTR::FloatingPoint, B::Array{Float64,1},
    LDB::FloatingPoint, SCALE::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    XNORM::FloatingPoint)

    INFO = [0]

    ccall((:sb03mu_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &LTRANL, &LTRANR, &ISGN,
            &N1, &N2, TL, &LDTL, TR, &LDTR, B, &LDB, &SCALE, X,
            &LDX, &XNORM, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03MU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03mv!(LTRAN::Bool, LUPPER::Bool, T::Array{Float64,1},
    LDT::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    SCALE::FloatingPoint, X::Array{Float64,1},
    LDX::FloatingPoint, XNORM::FloatingPoint)

    INFO = [0]

    ccall((:sb03mv_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &LTRAN, &LUPPER, T, &LDT, B, &LDB,
            &SCALE, X, &LDX, &XNORM, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03MV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03mw!(LTRAN::Bool, LUPPER::Bool, T::Array{Float64,1},
    LDT::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    SCALE::FloatingPoint, X::Array{Float64,1},
    LDX::FloatingPoint, XNORM::FloatingPoint)

    INFO = [0]

    ccall((:sb03mw_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &LTRAN, &LUPPER, T, &LDT, B, &LDB,
            &SCALE, X, &LDX, &XNORM, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03MW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03mx!(TRANA::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb03mx_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &TRANA, &N,
            A, &LDA, C, &LDC, &SCALE, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03MX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03my!(TRANA::Char, N::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint)

    INFO = [0]

    ccall((:sb03my_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &TRANA, &N, A, &LDA, C,
            &LDC, &SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03MY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03od!(DICO::Char, FACT::Char, TRANS::Char, N::Integer,
    M::Integer, A::Array{Float64,2}, LDA::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    B::Array{Float64,2}, LDB::Integer, SCALE::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb03od_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &FACT,
            &TRANS, &N, &M, A, &LDA, Q, &LDQ, B, &LDB, &SCALE, WR,
            WI, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03or!(DISCR::Bool, LTRANS::Bool, N::Integer, M::Integer,
    S::Array{Float64,1}, LDS::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint)

    INFO = [0]

    ccall((:sb03or_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &DISCR, &LTRANS, &N, &M, S,
            &LDS, A, &LDA, C, &LDC, &SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03OR: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03ot!(DISCR::Bool, LTRANS::Bool, N::Integer,
    S::Array{Float64,2}, LDS::Integer,
    R::Array{Float64,2}, LDR::Integer, SCALE::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb03ot_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &DISCR, &LTRANS, &N, S, &LDS, R, &LDR, &SCALE, DWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03OT: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03ou!(DISCR::Bool, LTRANS::Bool, N::Integer, M::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    TAU::Array{Float64,1}, U::Array{Float64,2},
    LDU::Integer, SCALE::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03ou_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &DISCR, &LTRANS, &N, &M, A, &LDA, B,
            &LDB, TAU, U, &LDU, &SCALE, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03OU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03ov!(A::Array{Float64,1}, B::FloatingPoint,
    C::Array{Float64,1}, S::FloatingPoint)

    INFO = [0]

    ccall((:sb03ov_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}), A, &B, C, &S)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03OV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03oy!(DISCR::Bool, LTRANS::Bool, ISGN::Integer,
    S::Array{Float64,2}, LDS::Integer,
    R::Array{Float64,2}, LDR::Integer,
    A::Array{Float64,2}, LDA::Integer, SCALE::FloatingPoint)

    INFO = [0]

    ccall((:sb03oy_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &DISCR, &LTRANS, &ISGN, S, &LDS, R, &LDR,
            A, &LDA, &SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03OY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03pd!(JOB::Char, FACT::Char, TRANA::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint, SEPD::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOB, &FACT, &TRANA, &N, A, &LDA, U,
            &LDU, C, &LDC, &SCALE, &SEPD, &FERR, WR, WI, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03qd!(JOB::Char, FACT::Char, TRANA::Char, UPLO::Char,
    LYAPUN::Char, N::Integer, SCALE::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint, SEP::FloatingPoint,
    RCOND::FloatingPoint, FERR::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03qd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &FACT, &TRANA, &UPLO,
            &LYAPUN, &N, &SCALE, A, &LDA, T, &LDT, U, &LDU, C, &LDC,
            X, &LDX, &SEP, &RCOND, &FERR, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03qx!(TRANA::Char, UPLO::Char, LYAPUN::Char, N::Integer,
    XANORM::FloatingPoint, T::Array{Float64,1},
    LDT::FloatingPoint, U::Array{Float64,1}, LDU::FloatingPoint,
    R::Array{Float64,1}, LDR::FloatingPoint,
    FERR::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb03qx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &TRANA, &UPLO, &LYAPUN, &N,
            &XANORM, T, &LDT, U, &LDU, R, &LDR, &FERR, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03QX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03qy!(JOB::Char, TRANA::Char, LYAPUN::Char, N::Integer,
    T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint, SEP::FloatingPoint,
    THNORM::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb03qy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &TRANA, &LYAPUN, &N,
            T, &LDT, U, &LDU, X, &LDX, &SEP, &THNORM, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03QY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03rd!(JOB::Char, FACT::Char, TRANA::Char, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint, SEP::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOB, &FACT, &TRANA, &N, A, &LDA, U,
            &LDU, C, &LDC, &SCALE, &SEP, &FERR, WR, WI, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03sd!(JOB::Char, FACT::Char, TRANA::Char, UPLO::Char,
    LYAPUN::Char, N::Integer, SCALE::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    SEPD::FloatingPoint, RCOND::FloatingPoint, FERR::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03sd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &FACT, &TRANA, &UPLO,
            &LYAPUN, &N, &SCALE, A, &LDA, T, &LDT, U, &LDU, C, &LDC,
            X, &LDX, &SEPD, &RCOND, &FERR, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03sx!(TRANA::Char, UPLO::Char, LYAPUN::Char, N::Integer,
    XANORM::FloatingPoint, T::Array{Float64,1},
    LDT::FloatingPoint, U::Array{Float64,1}, LDU::FloatingPoint,
    R::Array{Float64,1}, LDR::FloatingPoint,
    FERR::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb03sx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &TRANA, &UPLO, &LYAPUN, &N,
            &XANORM, T, &LDT, U, &LDU, R, &LDR, &FERR, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03SX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03sy!(JOB::Char, TRANA::Char, LYAPUN::Char, N::Integer,
    T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    XA::Array{Float64,1}, LDXA::FloatingPoint,
    SEPD::FloatingPoint, THNORM::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03sy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &TRANA, &LYAPUN, &N,
            T, &LDT, U, &LDU, XA, &LDXA, &SEPD, &THNORM, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03SY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03td!(JOB::Char, FACT::Char, TRANA::Char, UPLO::Char,
    LYAPUN::Char, N::Integer, SCALE::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint, SEP::FloatingPoint,
    RCOND::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03td_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &FACT, &TRANA, &UPLO, &LYAPUN, &N, &SCALE, A,
            &LDA, T, &LDT, U, &LDU, C, &LDC, X, &LDX, &SEP, &RCOND,
            &FERR, WR, WI, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb03ud!(JOB::Char, FACT::Char, TRANA::Char, UPLO::Char,
    LYAPUN::Char, N::Integer, SCALE::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    T::Array{Float64,1}, LDT::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    SEPD::FloatingPoint, RCOND::FloatingPoint, FERR::FloatingPoint,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb03ud_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOB, &FACT, &TRANA, &UPLO, &LYAPUN, &N, &SCALE, A,
            &LDA, T, &LDT, U, &LDU, C, &LDC, X, &LDX, &SEPD, &RCOND,
            &FERR, WR, WI, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB03UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04md!(N::Integer, M::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    Z::Array{Float64,2}, LDZ::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb04md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, &M, A, &LDA, B, &LDB, C, &LDC, Z, &LDZ, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04mr!(M::Integer, D::Array{Float64,1},
    IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04mr_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &M, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04MR: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04mu!(N::Integer, M::Integer, IND::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,1}, IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04mu_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &IND, A, &LDA, B,
            &LDB, C, &LDC, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04MU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04mw!(M::Integer, D::Array{Float64,1},
    IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04mw_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &M, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04MW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04my!(N::Integer, M::Integer, IND::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,1}, IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04my_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &IND, A, &LDA, B,
            &LDB, C, &LDC, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04MY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04nd!(ABSCHU::Char, ULA::Char, ULB::Char, N::Integer,
    M::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb04nd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &ABSCHU, &ULA, &ULB, &N,
            &M, A, &LDA, B, &LDB, C, &LDC, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04nv!(ABSCHR::Char, UL::Char, N::Integer, M::Integer,
    C::Array{Float64,2}, LDC::Integer, INDX::Integer,
    AB::Array{Float64,2}, LDAB::Integer,
    D::Array{Float64,1})

    INFO = [0]

    ccall((:sb04nv_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}),
            &ABSCHR, &UL, &N, &M, C, &LDC, &INDX, AB, &LDAB, D)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04NV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04nw!(ABSCHR::Char, UL::Char, N::Integer, M::Integer,
    C::Array{Float64,2}, LDC::Integer, INDX::Integer,
    AB::Array{Float64,2}, LDAB::Integer,
    D::Array{Float64,1})

    INFO = [0]

    ccall((:sb04nw_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}),
            &ABSCHR, &UL, &N, &M, C, &LDC, &INDX, AB, &LDAB, D)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04NW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04nx!(RC::Char, UL::Char, M::Integer,
    A::Array{Float64,2}, LDA::Integer, LAMBD1::FloatingPoint,
    LAMBD2::FloatingPoint, LAMBD3::FloatingPoint,
    LAMBD4::FloatingPoint, D::Array{Float64,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,2}, LDDWOR::Integer)

    INFO = [0]

    ccall((:sb04nx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &RC, &UL, &M, A, &LDA, &LAMBD1, &LAMBD2,
            &LAMBD3, &LAMBD4, D, &TOL, IWORK, DWORK, &LDDWOR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04NX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04ny!(RC::Char, UL::Char, M::Integer,
    A::Array{Float64,2}, LDA::Integer, LAMBDA::FloatingPoint,
    D::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,2},
    LDDWOR::Integer)

    INFO = [0]

    ccall((:sb04ny_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &RC, &UL, &M, A, &LDA,
            &LAMBDA, D, &TOL, IWORK, DWORK, &LDDWOR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04NY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04od!(REDUCE::Char, TRANS::Char, JOBD::Char, M::Integer,
    N::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    E::Array{Float64,2}, LDE::Integer,
    F::Array{Float64,2}, LDF::Integer, SCALE::FloatingPoint,
    DIF::FloatingPoint, P::Array{Float64,2}, LDP::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    U::Array{Float64,2}, LDU::Integer,
    V::Array{Float64,2}, LDV::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb04od_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &REDUCE, &TRANS, &JOBD, &M, &N, A, &LDA,
            B, &LDB, C, &LDC, D, &LDD, E, &LDE, F, &LDF, &SCALE,
            &DIF, P, &LDP, Q, &LDQ, U, &LDU, V, &LDV, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04ow!(M::Integer, N::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    E::Array{Float64,2}, LDE::Integer,
    F::Array{Float64,2}, LDF::Integer, SCALE::FloatingPoint,
    IWORK::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04ow_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &M, &N, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, E, &LDE, F, &LDF,
            &SCALE, IWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04OW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04pd!(DICO::Char, FACTA::Char, FACTB::Char, TRANA::Char,
    TRANB::Char, ISGN::Integer, M::Integer, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    U::Array{Float64,1}, LDU::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    V::Array{Float64,1}, LDV::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb04pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &FACTA, &FACTB, &TRANA, &TRANB, &ISGN, &M, &N, A,
            &LDA, U, &LDU, B, &LDB, V, &LDV, C, &LDC, &SCALE, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04px!(LTRANL::Bool, LTRANR::Bool, ISGN::Integer,
    N1::Integer, N2::Integer, TL::Array{Float64,1},
    LDTL::FloatingPoint, TR::Array{Float64,1},
    LDTR::FloatingPoint, B::Array{Float64,1},
    LDB::FloatingPoint, SCALE::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    XNORM::FloatingPoint)

    INFO = [0]

    ccall((:sb04px_, libslicot), Void, (Ptr{Bool}, Ptr{Bool},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}), &LTRANL, &LTRANR, &ISGN,
            &N1, &N2, TL, &LDTL, TR, &LDTR, B, &LDB, &SCALE, X,
            &LDX, &XNORM, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04PX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04py!(TRANA::Char, TRANB::Char, ISGN::Integer, M::Integer,
    N::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::FloatingPoint, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb04py_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &TRANA, &TRANB, &ISGN, &M, &N, A, &LDA, B, &LDB, C,
            &LDC, &SCALE, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04PY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04qd!(N::Integer, M::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    Z::Array{Float64,2}, LDZ::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb04qd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, &M, A, &LDA, B, &LDB, C, &LDC, Z, &LDZ, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04qr!(M::Integer, D::Array{Float64,1},
    IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04qr_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &M, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04QR: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04qu!(N::Integer, M::Integer, IND::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,1}, IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04qu_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &IND, A, &LDA, B,
            &LDB, C, &LDC, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04QU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04qy!(N::Integer, M::Integer, IND::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,1}, IPR::Array{BlasInt,1})

    INFO = [0]

    ccall((:sb04qy_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &IND, A, &LDA, B,
            &LDB, C, &LDC, D, IPR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04QY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04rd!(ABSCHU::Char, ULA::Char, ULB::Char, N::Integer,
    M::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb04rd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &ABSCHU, &ULA, &ULB, &N,
            &M, A, &LDA, B, &LDB, C, &LDC, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04rv!(ABSCHR::Char, UL::Char, N::Integer, M::Integer,
    C::Array{Float64,2}, LDC::Integer, INDX::Integer,
    AB::Array{Float64,2}, LDAB::Integer,
    BA::Array{Float64,2}, LDBA::Integer,
    D::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb04rv_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}), &ABSCHR, &UL,
            &N, &M, C, &LDC, &INDX, AB, &LDAB, BA, &LDBA, D, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04RV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04rw!(ABSCHR::Char, UL::Char, N::Integer, M::Integer,
    C::Array{Float64,2}, LDC::Integer, INDX::Integer,
    AB::Array{Float64,2}, LDAB::Integer,
    BA::Array{Float64,2}, LDBA::Integer,
    D::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb04rw_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}), &ABSCHR, &UL,
            &N, &M, C, &LDC, &INDX, AB, &LDAB, BA, &LDBA, D, DWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04RW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04rx!(RC::Char, UL::Char, M::Integer,
    A::Array{Float64,2}, LDA::Integer, LAMBD1::FloatingPoint,
    LAMBD2::FloatingPoint, LAMBD3::FloatingPoint,
    LAMBD4::FloatingPoint, D::Array{Float64,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,2}, LDDWOR::Integer)

    INFO = [0]

    ccall((:sb04rx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &RC, &UL, &M, A, &LDA, &LAMBD1, &LAMBD2,
            &LAMBD3, &LAMBD4, D, &TOL, IWORK, DWORK, &LDDWOR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04RX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb04ry!(RC::Char, UL::Char, M::Integer,
    A::Array{Float64,2}, LDA::Integer, LAMBDA::FloatingPoint,
    D::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,2},
    LDDWOR::Integer)

    INFO = [0]

    ccall((:sb04ry_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &RC, &UL, &M, A, &LDA,
            &LAMBDA, D, &TOL, IWORK, DWORK, &LDDWOR, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB04RY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb06nd!(N::Integer, M::Integer, KMAX::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer, KSTAIR::Array{BlasInt,1},
    U::Array{Float64,2}, LDU::Integer,
    F::Array{Float64,2}, LDF::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb06nd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &KMAX, A, &LDA, B, &LDB, KSTAIR, U, &LDU, F,
            &LDF, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB06ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08cd!(DICO::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NQ::Integer, NR::Integer,
    BR::Array{Float64,2}, LDBR::Integer,
    DR::Array{Float64,2}, LDDR::Integer, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb08cd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &N, &M, &P, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, &NQ, &NR, BR, &LDBR, DR, &LDDR, &TOL,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08dd!(DICO::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NQ::Integer, NR::Integer,
    CR::Array{Float64,2}, LDCR::Integer,
    DR::Array{Float64,2}, LDDR::Integer, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb08dd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &N, &M, &P, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, &NQ, &NR, CR, &LDCR, DR, &LDDR, &TOL,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08ed!(DICO::Char, N::Integer, M::Integer, P::Integer,
    ALPHA::Array{Float64,1}, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NQ::Integer, NR::Integer,
    BR::Array{Float64,2}, LDBR::Integer,
    DR::Array{Float64,2}, LDDR::Integer, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb08ed_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &N, &M, &P, ALPHA,
            A, &LDA, B, &LDB, C, &LDC, D, &LDD, &NQ, &NR, BR, &LDBR,
            DR, &LDDR, &TOL, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08fd!(DICO::Char, N::Integer, M::Integer, P::Integer,
    ALPHA::Array{Float64,1}, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NQ::Integer, NR::Integer,
    CR::Array{Float64,2}, LDCR::Integer,
    DR::Array{Float64,2}, LDDR::Integer, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb08fd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &N, &M, &P, ALPHA,
            A, &LDA, B, &LDB, C, &LDC, D, &LDD, &NQ, &NR, CR, &LDCR,
            DR, &LDDR, &TOL, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08gd!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    BR::Array{Float64,2}, LDBR::Integer,
    DR::Array{Float64,2}, LDDR::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb08gd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, BR,
            &LDBR, DR, &LDDR, IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08GD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08hd!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    CR::Array{Float64,2}, LDCR::Integer,
    DR::Array{Float64,2}, LDDR::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sb08hd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, CR,
            &LDCR, DR, &LDDR, IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08md!(ACONA::Char, DA::Integer, A::Array{Float64,1},
    RES::FloatingPoint, E::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb08md_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &ACONA, &DA, A, &RES, E,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08my!(DA::Integer, A::Array{Float64,1},
    B::Array{Float64,1}, EPSB::FloatingPoint)

    INFO = [0]

    ccall((:sb08my_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}), &DA, A, B, &EPSB)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08MY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08nd!(ACONA::Char, DA::Integer, A::Array{Float64,1},
    RES::FloatingPoint, E::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb08nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &ACONA, &DA, A, &RES, E,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb08ny!(DA::Integer, A::Array{Float64,1},
    B::Array{Float64,1}, EPSB::FloatingPoint)

    INFO = [0]

    ccall((:sb08ny_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}), &DA, A, B, &EPSB)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB08NY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb09md!(N::Integer, NC::Integer, NB::Integer,
    H1::Array{Float64,2}, LDH1::Integer,
    H2::Array{Float64,2}, LDH2::Integer,
    SS::Array{Float64,2}, LDSS::Integer,
    SE::Array{Float64,2}, LDSE::Integer,
    PRE::Array{Float64,2}, LDPRE::Integer, TOL::FloatingPoint)

    INFO = [0]

    ccall((:sb09md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &N, &NC, &NB, H1, &LDH1, H2, &LDH2, SS,
            &LDSS, SE, &LDSE, PRE, &LDPRE, &TOL, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB09MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10ad!(JOB::Integer, N::Integer, M::Integer, NP::Integer,
    NCON::Integer, NMEAS::Integer, GAMMA::FloatingPoint,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    AC::Array{Float64,1}, LDAC::FloatingPoint,
    BC::Array{Float64,1}, LDBC::FloatingPoint,
    CC::Array{Float64,1}, LDCC::FloatingPoint,
    DC::Array{Float64,1}, LDDC::FloatingPoint,
    RCOND::Array{Float64,1}, GTOL::FloatingPoint,
    ACTOL::FloatingPoint, IWORK::Array{BlasInt,1}, LIWORK::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1}, LBWORK::Integer)

    INFO = [0]

    ccall((:sb10ad_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &N, &M,
            &NP, &NCON, &NMEAS, &GAMMA, A, &LDA, B, &LDB, C, &LDC,
            D, &LDD, AK, &LDAK, BK, &LDBK, CK, &LDCK, DK, &LDDK, AC,
            &LDAC, BC, &LDBC, CC, &LDCC, DC, &LDDC, RCOND, &GTOL,
            &ACTOL, IWORK, &LIWORK, DWORK, &LDWORK, BWORK, &LBWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10dd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, GAMMA::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10dd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP,
            &NCON, &NMEAS, &GAMMA, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, AK, &LDAK, BK, &LDBK, CK, &LDCK, DK, &LDDK, X,
            &LDX, Z, &LDZ, RCOND, &TOL, IWORK, DWORK, &LDWORK,
            BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10ed!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10ed_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP, &NCON, &NMEAS, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, AK, &LDAK, BK, &LDBK,
            CK, &LDCK, DK, &LDDK, RCOND, &TOL, IWORK, DWORK,
            &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10fd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, GAMMA::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10fd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP,
            &NCON, &NMEAS, &GAMMA, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, AK, &LDAK, BK, &LDBK, CK, &LDCK, DK, &LDDK, RCOND,
            &TOL, IWORK, DWORK, &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10hd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10hd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP, &NCON, &NMEAS, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, AK, &LDAK, BK, &LDBK,
            CK, &LDCK, DK, &LDDK, RCOND, &TOL, IWORK, DWORK,
            &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10id!(N::Integer, M::Integer, NP::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    FACTOR::FloatingPoint, NK::Integer, AK::Array{Float64,1},
    LDAK::FloatingPoint, BK::Array{Float64,1},
    LDBK::FloatingPoint, CK::Array{Float64,1},
    LDCK::FloatingPoint, DK::Array{Float64,1},
    LDDK::FloatingPoint, RCOND::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10id_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool},
            Ptr{BlasInt}), &N, &M, &NP, A, &LDA, B, &LDB, C, &LDC,
            D, &LDD, &FACTOR, &NK, AK, &LDAK, BK, &LDBK, CK, &LDCK,
            DK, &LDDK, RCOND, IWORK, DWORK, &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10jd!(N::Integer, M::Integer, NP::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint, NSYS::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb10jd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &NP,
            A, &LDA, B, &LDB, C, &LDC, D, &LDD, E, &LDE, &NSYS,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10JD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10kd!(N::Integer, M::Integer, NP::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    FACTOR::FloatingPoint, AK::Array{Float64,1},
    LDAK::FloatingPoint, BK::Array{Float64,1},
    LDBK::FloatingPoint, CK::Array{Float64,1},
    LDCK::FloatingPoint, DK::Array{Float64,1},
    LDDK::FloatingPoint, RCOND::Array{Float64,1},
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10kd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP, A, &LDA, B, &LDB,
            C, &LDC, &FACTOR, AK, &LDAK, BK, &LDBK, CK, &LDCK, DK,
            &LDDK, RCOND, IWORK, DWORK, &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10ld!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    AC::Array{Float64,1}, LDAC::FloatingPoint,
    BC::Array{Float64,1}, LDBC::FloatingPoint,
    CC::Array{Float64,1}, LDCC::FloatingPoint,
    DC::Array{Float64,1}, LDDC::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb10ld_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &NP,
            &NCON, &NMEAS, A, &LDA, B, &LDB, C, &LDC, D, &LDD, AK,
            &LDAK, BK, &LDBK, CK, &LDCK, DK, &LDDK, AC, &LDAC, BC,
            &LDBC, CC, &LDCC, DC, &LDDC, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10LD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10md!(NC::Integer, MP::Integer, LENDAT::Integer,
    F::Integer, ORD::Integer, MNB::Integer, NBLOCK::Array{BlasInt,1},
    ITYPE::Array{BlasInt,1}, QUTOL::FloatingPoint,
    A::Array{Float64,1}, LDA::Integer,
    B::Array{Float64,1}, LDB::Integer,
    C::Array{Float64,1}, LDC::Integer,
    D::Array{Float64,1}, LDD::Integer,
    OMEGA::Array{Float64,1}, TOTORD::Integer,
    AD::Array{Float64,1}, LDAD::Integer,
    BD::Array{Float64,1}, LDBD::Integer,
    CD::Array{Float64,1}, LDCD::Integer,
    DD::Array{Float64,1}, LDDD::Integer,
    MJU::Array{Float64,1}, IWORK::Array{BlasInt,1},
    LIWORK::Integer, DWORK::Array{Float64,1}, LDWORK::Integer,
    ZWORK::Array{Complex,1}, LZWORK::Integer)

    INFO = [0]

    ccall((:sb10md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}), &NC, &MP, &LENDAT, &F,
            &ORD, &MNB, NBLOCK, ITYPE, &QUTOL, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, OMEGA, &TOTORD, AD, &LDAD, BD, &LDBD, CD,
            &LDCD, DD, &LDDD, MJU, IWORK, &LIWORK, DWORK, &LDWORK,
            ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10pd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    TU::Array{Float64,1}, LDTU::FloatingPoint,
    TY::Array{Float64,1}, LDTY::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb10pd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, &M, &NP, &NCON, &NMEAS, A, &LDA, B, &LDB, C, &LDC,
            D, &LDD, TU, &LDTU, TY, &LDTY, RCOND, &TOL, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10qd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, GAMMA::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    H::Array{Float64,1}, LDH::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    Y::Array{Float64,1}, LDY::FloatingPoint,
    XYCOND::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10qd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP, &NCON, &NMEAS,
            &GAMMA, A, &LDA, B, &LDB, C, &LDC, D, &LDD, F, &LDF, H,
            &LDH, X, &LDX, Y, &LDY, XYCOND, IWORK, DWORK, &LDWORK,
            BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10rd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, GAMMA::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    H::Array{Float64,1}, LDH::FloatingPoint,
    TU::Array{Float64,1}, LDTU::FloatingPoint,
    TY::Array{Float64,1}, LDTY::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    Y::Array{Float64,1}, LDY::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sb10rd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, &M, &NP, &NCON, &NMEAS, &GAMMA, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, F, &LDF, H, &LDH, TU, &LDTU, TY, &LDTY,
            X, &LDX, Y, &LDY, AK, &LDAK, BK, &LDBK, CK, &LDCK, DK,
            &LDDK, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10sd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    Y::Array{Float64,1}, LDY::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10sd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP, &NCON, &NMEAS, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, AK, &LDAK, BK, &LDBK,
            CK, &LDCK, DK, &LDDK, X, &LDX, Y, &LDY, RCOND, &TOL,
            IWORK, DWORK, &LDWORK, BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10SD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10td!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, D::Array{Float64,1}, LDD::FloatingPoint,
    TU::Array{Float64,1}, LDTU::FloatingPoint,
    TY::Array{Float64,1}, LDTY::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    RCOND::FloatingPoint, TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb10td_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &NP,
            &NCON, &NMEAS, D, &LDD, TU, &LDTU, TY, &LDTY, AK, &LDAK,
            BK, &LDBK, CK, &LDCK, DK, &LDDK, &RCOND, &TOL, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10ud!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    TU::Array{Float64,1}, LDTU::FloatingPoint,
    TY::Array{Float64,1}, LDTY::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb10ud_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &NP, &NCON, &NMEAS,
            B, &LDB, C, &LDC, D, &LDD, TU, &LDTU, TY, &LDTY, RCOND,
            &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10vd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    H::Array{Float64,1}, LDH::FloatingPoint,
    X::Array{Float64,1}, LDX::FloatingPoint,
    Y::Array{Float64,1}, LDY::FloatingPoint,
    XYCOND::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10vd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Bool}, Ptr{BlasInt}), &N, &M, &NP,
            &NCON, &NMEAS, A, &LDA, B, &LDB, C, &LDC, F, &LDF, H,
            &LDH, X, &LDX, Y, &LDY, XYCOND, IWORK, DWORK, &LDWORK,
            BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10wd!(N::Integer, M::Integer, NP::Integer, NCON::Integer,
    NMEAS::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint,
    F::Array{Float64,1}, LDF::FloatingPoint,
    H::Array{Float64,1}, LDH::FloatingPoint,
    TU::Array{Float64,1}, LDTU::FloatingPoint,
    TY::Array{Float64,1}, LDTY::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint)

    INFO = [0]

    ccall((:sb10wd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &NP, &NCON, &NMEAS, A, &LDA, B, &LDB, C, &LDC,
            D, &LDD, F, &LDF, H, &LDH, TU, &LDTU, TY, &LDTY, AK,
            &LDAK, BK, &LDBK, CK, &LDCK, DK, &LDDK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10yd!(DISCFL::Integer, FLAG::Integer, LENDAT::Integer,
    RFRDAT::Array{Float64,1}, IFRDAT::Array{Float64,1},
    OMEGA::Array{Float64,1}, N::Integer,
    A::Array{Float64,1}, LDA::Integer,
    B::Array{Float64,1}, C::Array{Float64,1},
    D::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, ZWORK::Array{Complex,1}, LZWORK::Integer)

    INFO = [0]

    ccall((:sb10yd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}), &DISCFL, &FLAG, &LENDAT,
            RFRDAT, IFRDAT, OMEGA, &N, A, &LDA, B, C, D, &TOL,
            IWORK, DWORK, &LDWORK, ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10YD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10zd!(N::Integer, M::Integer, NP::Integer,
    A::FloatingPoint, LDA::FloatingPoint, B::FloatingPoint,
    LDB::FloatingPoint, C::FloatingPoint, LDC::FloatingPoint,
    D::FloatingPoint, LDD::FloatingPoint, FACTOR::FloatingPoint,
    AK::Array{Float64,1}, LDAK::FloatingPoint,
    BK::Array{Float64,1}, LDBK::FloatingPoint,
    CK::Array{Float64,1}, LDCK::FloatingPoint,
    DK::Array{Float64,1}, LDDK::FloatingPoint,
    RCOND::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1})

    INFO = [0]

    ccall((:sb10zd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Bool},
            Ptr{BlasInt}), &N, &M, &NP, &A, &LDA, &B, &LDB, &C,
            &LDC, &D, &LDD, &FACTOR, AK, &LDAK, BK, &LDBK, CK,
            &LDCK, DK, &LDDK, RCOND, &TOL, IWORK, DWORK, &LDWORK,
            BWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10ZD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb10zp!(DISCFL::Integer, N::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, C::Array{Float64,1},
    D::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb10zp_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &DISCFL, &N, A, &LDA, B, C, D, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB10ZP: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb16ad!(DICO::Char, JOBC::Char, JOBO::Char, JOBMR::Char,
    WEIGHT::Char, EQUIL::Char, ORDSEL::Char, N::Integer, M::Integer,
    P::Integer, NC::Integer, NCR::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AC::Array{Float64,2}, LDAC::Integer,
    BC::Array{Float64,2}, LDBC::Integer,
    CC::Array{Float64,2}, LDCC::Integer,
    DC::Array{Float64,2}, LDDC::Integer, NCS::Integer,
    HSVC::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb16ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOBC,
            &JOBO, &JOBMR, &WEIGHT, &EQUIL, &ORDSEL, &N, &M, &P,
            &NC, &NCR, &ALPHA, A, &LDA, B, &LDB, C, &LDC, D, &LDD,
            AC, &LDAC, BC, &LDBC, CC, &LDCC, DC, &LDDC, &NCS, HSVC,
            &TOL1, &TOL2, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB16AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb16ay!(DICO::Char, JOBC::Char, JOBO::Char, WEIGHT::Char,
    N::Integer, M::Integer, P::Integer, NC::Integer, NCS::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    AC::Array{Float64,2}, LDAC::Integer,
    BC::Array{Float64,2}, LDBC::Integer,
    CC::Array{Float64,2}, LDCC::Integer,
    DC::Array{Float64,2}, LDDC::Integer, SCALEC::FloatingPoint,
    SCALEO::FloatingPoint, S::Array{Float64,2}, LDS::Integer,
    R::Array{Float64,2}, LDR::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb16ay_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &DICO, &JOBC, &JOBO, &WEIGHT, &N, &M, &P,
            &NC, &NCS, A, &LDA, B, &LDB, C, &LDC, D, &LDD, AC,
            &LDAC, BC, &LDBC, CC, &LDCC, DC, &LDDC, &SCALEC,
            &SCALEO, S, &LDS, R, &LDR, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB16AY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb16bd!(DICO::Char, JOBD::Char, JOBMR::Char, JOBCF::Char,
    EQUIL::Char, ORDSEL::Char, N::Integer, M::Integer, P::Integer,
    NCR::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    F::Array{Float64,2}, LDF::Integer,
    G::Array{Float64,2}, LDG::Integer,
    DC::Array{Float64,2}, LDDC::Integer,
    HSV::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb16bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOBD, &JOBMR,
            &JOBCF, &EQUIL, &ORDSEL, &N, &M, &P, &NCR, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, F, &LDF, G, &LDG, DC, &LDDC,
            HSV, &TOL1, &TOL2, IWORK, DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB16BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb16cd!(DICO::Char, JOBD::Char, JOBMR::Char, JOBCF::Char,
    ORDSEL::Char, N::Integer, M::Integer, P::Integer, NCR::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    F::Array{Float64,2}, LDF::Integer,
    G::Array{Float64,2}, LDG::Integer,
    HSV::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, IWARN::Integer)

    INFO = [0]

    ccall((:sb16cd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOBD, &JOBMR,
            &JOBCF, &ORDSEL, &N, &M, &P, &NCR, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, F, &LDF, G, &LDG, HSV, &TOL, IWORK,
            DWORK, &LDWORK, &IWARN, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB16CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sb16cy!(DICO::Char, JOBCF::Char, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    F::Array{Float64,2}, LDF::Integer,
    G::Array{Float64,2}, LDG::Integer, SCALEC::FloatingPoint,
    SCALEO::FloatingPoint, S::Array{Float64,2}, LDS::Integer,
    R::Array{Float64,2}, LDR::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sb16cy_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOBCF, &N, &M, &P,
            A, &LDA, B, &LDB, C, &LDC, F, &LDF, G, &LDG, &SCALEC,
            &SCALEO, S, &LDS, R, &LDR, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SB16CY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg02ad!(DICO::Char, JOBB::Char, FACT::Char, UPLO::Char,
    JOBL::Char, SCAL::Char, SORT::Char, ACC::Char, N::Integer,
    M::Integer, P::Integer, A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    R::Array{Float64,2}, LDR::Integer,
    L::Array{Float64,2}, LDL::Integer, RCONDU::FloatingPoint,
    X::Array{Float64,2}, LDX::Integer,
    ALFAR::Array{Float64,1}, ALFAI::Array{Float64,1},
    BETA::Array{Float64,1}, S::Array{Float64,2},
    LDS::Integer, T::Array{Float64,2}, LDT::Integer,
    U::Array{Float64,2}, LDU::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer, BWORK::Array{Bool,1}, IWARN::Integer)

    INFO = [0]

    ccall((:sg02ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Bool}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &JOBB,
            &FACT, &UPLO, &JOBL, &SCAL, &SORT, &ACC, &N, &M, &P, A,
            &LDA, E, &LDE, B, &LDB, Q, &LDQ, R, &LDR, L, &LDL,
            &RCONDU, X, &LDX, ALFAR, ALFAI, BETA, S, &LDS, T, &LDT,
            U, &LDU, &TOL, IWORK, DWORK, &LDWORK, BWORK, &IWARN,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG02AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03ad!(DICO::Char, JOB::Char, FACT::Char, TRANS::Char,
    UPLO::Char, N::Integer, A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    X::Array{Float64,2}, LDX::Integer, SCALE::FloatingPoint,
    SEP::FloatingPoint, FERR::FloatingPoint,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:sg03ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{Char}, Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &JOB, &FACT, &TRANS, &UPLO, &N, A, &LDA, E, &LDE,
            Q, &LDQ, Z, &LDZ, X, &LDX, &SCALE, &SEP, &FERR, ALPHAR,
            ALPHAI, BETA, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03ax!(TRANS::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    X::Array{Float64,2}, LDX::Integer, SCALE::FloatingPoint)

    INFO = [0]

    ccall((:sg03ax_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &TRANS, &N, A, &LDA, E, &LDE, X, &LDX, &SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03AX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03ay!(TRANS::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    X::Array{Float64,2}, LDX::Integer, SCALE::FloatingPoint)

    INFO = [0]

    ccall((:sg03ay_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &TRANS, &N, A, &LDA, E, &LDE, X, &LDX, &SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03AY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03bd!(DICO::Char, FACT::Char, TRANS::Char, N::Integer,
    M::Integer, A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    B::Array{Float64,2}, LDB::Integer, SCALE::FloatingPoint,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:sg03bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &DICO, &FACT, &TRANS, &N, &M, A, &LDA, E, &LDE, Q, &LDQ,
            Z, &LDZ, B, &LDB, &SCALE, ALPHAR, ALPHAI, BETA, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03bu!(TRANS::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer, SCALE::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sg03bu_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &TRANS, &N, A, &LDA, E, &LDE, B, &LDB,
            &SCALE, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03BU: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03bv!(TRANS::Char, N::Integer, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer, SCALE::FloatingPoint,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:sg03bv_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &TRANS, &N, A, &LDA, E, &LDE, B, &LDB,
            &SCALE, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03BV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03bw!(TRANS::Char, M::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    C::Array{Float64,2}, LDC::Integer,
    E::Array{Float64,2}, LDE::Integer,
    D::Array{Float64,2}, LDD::Integer,
    X::Array{Float64,2}, LDX::Integer, SCALE::FloatingPoint)

    INFO = [0]

    ccall((:sg03bw_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &TRANS, &M, &N, A, &LDA, C, &LDC, E,
            &LDE, D, &LDD, X, &LDX, &SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03BW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03bx!(DICO::Char, TRANS::Char, A::Array{Float64,2},
    LDA::Integer, E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer,
    U::Array{Float64,2}, LDU::Integer, SCALE::FloatingPoint,
    M1::Array{Float64,2}, LDM1::Integer,
    M2::Array{Float64,2}, LDM2::Integer)

    INFO = [0]

    ccall((:sg03bx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &DICO, &TRANS, A, &LDA, E,
            &LDE, B, &LDB, U, &LDU, &SCALE, M1, &LDM1, M2, &LDM2,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03BX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function sg03by!(XR::FloatingPoint, XI::FloatingPoint,
    YR::FloatingPoint, YI::FloatingPoint, CR::FloatingPoint,
    CI::FloatingPoint, SR::FloatingPoint, SI::FloatingPoint,
    Z::FloatingPoint)

    INFO = [0]

    ccall((:sg03by_, libslicot), Void, (Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}), &XR, &XI,
            &YR, &YI, &CR, &CI, &SR, &SI, &Z)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in SG03BY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01id!(JOB::Char, N::Integer, M::Integer, P::Integer,
    MAXRED::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    SCALE::Array{Float64,1})

    INFO = [0]

    ccall((:tb01id_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &JOB, &N, &M,
            &P, &MAXRED, A, &LDA, B, &LDB, C, &LDC, SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01iz!(JOB::Char, N::Integer, M::Integer, P::Integer,
    MAXRED::FloatingPoint, A::Array{Complex,1}, LDA::Complex,
    B::Array{Complex,1}, LDB::Complex, C::Array{Complex,1},
    LDC::Complex, SCALE::Array{Float64,1})

    INFO = [0]

    ccall((:tb01iz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Float64}, Ptr{BlasInt}), &JOB, &N, &M, &P, &MAXRED,
            A, &LDA, B, &LDB, C, &LDC, SCALE, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01IZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01kd!(DICO::Char, STDOM::Char, JOBA::Char, N::Integer,
    M::Integer, P::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NDIM::Integer,
    U::Array{Float64,2}, LDU::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb01kd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO,
            &STDOM, &JOBA, &N, &M, &P, &ALPHA, A, &LDA, B, &LDB, C,
            &LDC, &NDIM, U, &LDU, WR, WI, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01KD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01ld!(DICO::Char, STDOM::Char, JOBA::Char, N::Integer,
    M::Integer, P::Integer, ALPHA::FloatingPoint,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NDIM::Integer,
    U::Array{Float64,2}, LDU::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb01ld_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &DICO,
            &STDOM, &JOBA, &N, &M, &P, &ALPHA, A, &LDA, B, &LDB, C,
            &LDC, &NDIM, U, &LDU, WR, WI, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01LD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01md!(JOBU::Char, UPLO::Char, N::Integer, M::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    U::Array{Float64,2}, LDU::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tb01md_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &JOBU, &UPLO, &N, &M, A,
            &LDA, B, &LDB, U, &LDU, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01nd!(JOBU::Char, UPLO::Char, N::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    C::Array{Float64,2}, LDC::Integer,
    U::Array{Float64,2}, LDU::Integer,
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tb01nd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &JOBU, &UPLO, &N, &P, A,
            &LDA, C, &LDC, U, &LDU, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01pd!(JOB::Char, EQUIL::Char, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NR::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb01pd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB, &EQUIL,
            &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, &NR, &TOL, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01td!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, LOW::Integer,
    IGH::Integer, SCSTAT::Array{Float64,1},
    SCIN::Array{Float64,1}, SCOUT::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tb01td_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, &LOW,
            &IGH, SCSTAT, SCIN, SCOUT, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01TD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01ty!(MODE::Integer, IOFF::Integer, JOFF::Integer,
    NROW::Integer, NCOL::Integer, SIZE::FloatingPoint,
    X::Array{Float64,2}, LDX::Integer,
    BVECT::Array{Float64,1})

    INFO = [0]

    ccall((:tb01ty_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}), &MODE, &IOFF,
            &JOFF, &NROW, &NCOL, &SIZE, X, &LDX, BVECT)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01TY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01ud!(JOBZ::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NCONT::Integer,
    INDCON::Integer, NBLK::Array{BlasInt,1},
    Z::Array{Float64,2}, LDZ::Integer,
    TAU::Array{Float64,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:tb01ud_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOBZ, &N,
            &M, &P, A, &LDA, B, &LDB, C, &LDC, &NCONT, &INDCON,
            NBLK, Z, &LDZ, TAU, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01UD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01vd!(APPLY::Char, N::Integer, M::Integer, L::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    X0::Array{Float64,1}, THETA::Array{Float64,1},
    LTHETA::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb01vd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &APPLY, &N, &M, &L, A, &LDA, B, &LDB, C, &LDC, D, &LDD,
            X0, THETA, &LTHETA, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01VD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01vy!(APPLY::Char, N::Integer, M::Integer, L::Integer,
    THETA::Array{Float64,1}, LTHETA::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    X0::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:tb01vy_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &APPLY, &N, &M, &L, THETA, &LTHETA, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, X0, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01VY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01wd!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    U::Array{Float64,2}, LDU::Integer,
    WR::Array{Float64,1}, WI::Array{Float64,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb01wd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &P, A, &LDA, B,
            &LDB, C, &LDC, U, &LDU, WR, WI, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01xd!(JOBD::Char, N::Integer, M::Integer, P::Integer,
    KL::Integer, KU::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    D::Array{Float64,1}, LDD::FloatingPoint)

    INFO = [0]

    ccall((:tb01xd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}), &JOBD, &N, &M, &P, &KL, &KU, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01XD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01xz!(JOBD::Char, N::Integer, M::Integer, P::Integer,
    KL::Integer, KU::Integer, A::Array{Complex,1}, LDA::Complex,
    B::Array{Complex,1}, LDB::Complex, C::Array{Complex,1},
    LDC::Complex, D::Array{Complex,1}, LDD::Complex)

    INFO = [0]

    ccall((:tb01xz_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{BlasInt}), &JOBD,
            &N, &M, &P, &KL, &KU, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01XZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01yd!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint)

    INFO = [0]

    ccall((:tb01yd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01YD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb01zd!(JOBZ::Char, N::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,1}, C::Array{Float64,2},
    LDC::Integer, NCONT::Integer, Z::Array{Float64,2},
    LDZ::Integer, TAU::Array{Float64,1}, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb01zd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &JOBZ, &N, &P, A, &LDA, B,
            C, &LDC, &NCONT, Z, &LDZ, TAU, &TOL, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB01ZD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb03ad!(LERI::Char, EQUIL::Char, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NR::Integer,
    INDEX::Array{BlasInt,1}, PCOEFF::Array{Float64,3},
    LDPCO1::Integer, LDPCO2::Integer, QCOEFF::Array{Float64,3},
    LDQCO1::Integer, LDQCO2::Integer, VCOEFF::Array{Float64,3},
    LDVCO1::Integer, LDVCO2::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:tb03ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &LERI,
            &EQUIL, &N, &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD,
            &NR, INDEX, PCOEFF, &LDPCO1, &LDPCO2, QCOEFF, &LDQCO1,
            &LDQCO2, VCOEFF, &LDVCO1, &LDVCO2, &TOL, IWORK, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB03AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb03ay!(NR::Integer, A::Array{Float64,2}, LDA::Integer,
    INDBLK::Integer, NBLK::Array{BlasInt,1},
    VCOEFF::Array{Float64,3}, LDVCO1::Integer, LDVCO2::Integer,
    PCOEFF::Array{Float64,3}, LDPCO1::Integer, LDPCO2::Integer)

    INFO = [0]

    ccall((:tb03ay_, libslicot), Void, (Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}), &NR, A, &LDA, &INDBLK,
            NBLK, VCOEFF, &LDVCO1, &LDVCO2, PCOEFF, &LDPCO1,
            &LDPCO2, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB03AY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb04ad!(ROWCOL::Char, N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NR::Array{BlasInt,1},
    INDEX::Array{BlasInt,1}, DCOEFF::Array{Float64,2},
    LDDCOE::Integer, UCOEFF::Array{Float64,3}, LDUCO1::Integer,
    LDUCO2::Integer, TOL1::FloatingPoint, TOL2::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = BlasInt[0]

    ccall((:tb04ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &ROWCOL, &N,
            &M, &P, A, &LDA, B, &LDB, C, &LDC, D, &LDD, NR, INDEX,
            DCOEFF, &LDDCOE, UCOEFF, &LDUCO1, &LDUCO2, &TOL1, &TOL2,
            IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
    INFO
end


function tb04ay!(N::Integer, MWORK::Integer, PWORK::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NCONT::Integer,
    INDEXD::Array{BlasInt,1}, DCOEFF::Array{Float64,2},
    LDDCOE::Integer, UCOEFF::Array{Float64,3}, LDUCO1::Integer,
    LDUCO2::Integer, AT::Array{Float64,2}, N1::Integer,
    TAU::Array{Float64,1}, TOL1::FloatingPoint,
    TOL2::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb04ay_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &N, &MWORK, &PWORK, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, &NCONT, INDEXD, DCOEFF, &LDDCOE, UCOEFF,
            &LDUCO1, &LDUCO2, AT, &N1, TAU, &TOL1, &TOL2, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04AY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb04bd!(JOBD::Char, ORDER::Char, EQUIL::Char, N::Integer,
    M::Integer, P::Integer, MD::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, IGN::Array{BlasInt,2},
    LDIGN::Integer, IGD::Array{BlasInt,2}, LDIGD::Integer,
    GN::Array{Float64,1}, GD::Array{Float64,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb04bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &JOBD, &ORDER, &EQUIL, &N, &M, &P, &MD, A, &LDA, B,
            &LDB, C, &LDC, D, &LDD, IGN, &LDIGN, IGD, &LDIGD, GN,
            GD, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb04bv!(ORDER::Char, P::Integer, M::Integer, MD::Integer,
    IGN::Array{BlasInt,2}, LDIGN::Integer, IGD::Array{BlasInt,2},
    LDIGD::Integer, GN::Array{Float64,1},
    GD::Array{Float64,1}, D::Array{Float64,2},
    LDD::Integer, TOL::FloatingPoint)

    INFO = [0]

    ccall((:tb04bv_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &ORDER, &P, &M, &MD, IGN, &LDIGN, IGD, &LDIGD, GN, GD,
            D, &LDD, &TOL, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04BV: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb04bw!(ORDER::Char, P::Integer, M::Integer, MD::Integer,
    IGN::Array{BlasInt,2}, LDIGN::Integer, IGD::Array{BlasInt,2},
    LDIGD::Integer, GN::Array{Float64,1},
    GD::Array{Float64,1}, D::Array{Float64,2},
    LDD::Integer)

    INFO = [0]

    ccall((:tb04bw_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &ORDER, &P,
            &M, &MD, IGN, &LDIGN, IGD, &LDIGD, GN, GD, D, &LDD,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04BW: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb04bx!(IP::Integer, IZ::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,1},
    C::Array{Float64,1}, D::FloatingPoint,
    PR::Array{Float64,1}, PI::Array{Float64,1},
    ZR::Array{Float64,1}, ZI::Array{Float64,1},
    GAIN::FloatingPoint, IWORK::Array{BlasInt,1})

    INFO = [0]

    ccall((:tb04bx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &IP, &IZ, A,
            &LDA, B, C, &D, PR, PI, ZR, ZI, &GAIN, IWORK)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04BX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb04cd!(JOBD::Char, EQUIL::Char, N::Integer, M::Integer,
    P::Integer, NPZ::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, NZ::Array{BlasInt,2},
    LDNZ::Integer, NP::Array{BlasInt,2}, LDNP::Integer,
    ZEROSR::Array{Float64,1}, ZEROSI::Array{Float64,1},
    POLESR::Array{Float64,1}, POLESI::Array{Float64,1},
    GAINS::Array{Float64,2}, LDGAIN::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tb04cd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOBD,
            &EQUIL, &N, &M, &P, &NPZ, A, &LDA, B, &LDB, C, &LDC, D,
            &LDD, NZ, &LDNZ, NP, &LDNP, ZEROSR, ZEROSI, POLESR,
            POLESI, GAINS, &LDGAIN, &TOL, IWORK, DWORK, &LDWORK,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB04CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tb05ad!(BALEIG::Char, INITA::Char, N::Integer, M::Integer,
    P::Integer, FREQ::Complex, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, RCOND::FloatingPoint,
    G::Array{Complex,2}, LDG::Integer, EVRE::Array{Float64,1},
    EVIM::Array{Float64,1}, HINVB::Array{Complex,2},
    LDHINV::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer,
    ZWORK::Array{Complex,1}, LZWORK::Integer)

    INFO = [0]

    ccall((:tb05ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Complex128},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt}), &BALEIG,
            &INITA, &N, &M, &P, &FREQ, A, &LDA, B, &LDB, C, &LDC,
            &RCOND, G, &LDG, EVRE, EVIM, HINVB, &LDHINV, IWORK,
            DWORK, &LDWORK, ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TB05AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tc01od!(LERI::Char, M::Integer, P::Integer, INDLIM::Integer,
    PCOEFF::Array{Float64,3}, LDPCO1::Integer, LDPCO2::Integer,
    QCOEFF::Array{Float64,3}, LDQCO1::Integer, LDQCO2::Integer)

    INFO = [0]

    ccall((:tc01od_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}), &LERI, &M, &P, &INDLIM, PCOEFF, &LDPCO1,
            &LDPCO2, QCOEFF, &LDQCO1, &LDQCO2, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TC01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tc04ad!(LERI::Char, M::Integer, P::Integer,
    INDEX::Array{BlasInt,1}, PCOEFF::Array{Float64,3},
    LDPCO1::Integer, LDPCO2::Integer, QCOEFF::Array{Float64,3},
    LDQCO1::Integer, LDQCO2::Integer, N::Integer,
    RCOND::FloatingPoint, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tc04ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &LERI, &M, &P, INDEX,
            PCOEFF, &LDPCO1, &LDPCO2, QCOEFF, &LDQCO1, &LDQCO2, &N,
            &RCOND, A, &LDA, B, &LDB, C, &LDC, D, &LDD, IWORK,
            DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TC04AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tc05ad!(LERI::Char, M::Integer, P::Integer, SVAL::Complex,
    INDEX::Array{BlasInt,1}, PCOEFF::Array{Float64,3},
    LDPCO1::Integer, LDPCO2::Integer, QCOEFF::Array{Float64,3},
    LDQCO1::Integer, LDQCO2::Integer, RCOND::FloatingPoint,
    CFREQR::Array{Complex,2}, LDCFRE::Integer,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    ZWORK::Array{Complex,1})

    INFO = [0]

    ccall((:tc05ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Complex128}, Ptr{BlasInt}), &LERI, &M,
            &P, &SVAL, INDEX, PCOEFF, &LDPCO1, &LDPCO2, QCOEFF,
            &LDQCO1, &LDQCO2, &RCOND, CFREQR, &LDCFRE, IWORK, DWORK,
            ZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TC05AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function td03ad!(ROWCOL::Char, LERI::Char, EQUIL::Char, M::Integer,
    P::Integer, INDEXD::Array{BlasInt,1},
    DCOEFF::Array{Float64,2}, LDDCOE::Integer,
    UCOEFF::Array{Float64,3}, LDUCO1::Integer, LDUCO2::Integer,
    NR::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, INDEXP::Array{BlasInt,1},
    PCOEFF::Array{Float64,3}, LDPCO1::Integer, LDPCO2::Integer,
    QCOEFF::Array{Float64,3}, LDQCO1::Integer, LDQCO2::Integer,
    VCOEFF::Array{Float64,3}, LDVCO1::Integer, LDVCO2::Integer,
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:td03ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &ROWCOL, &LERI, &EQUIL, &M, &P, INDEXD,
            DCOEFF, &LDDCOE, UCOEFF, &LDUCO1, &LDUCO2, &NR, A, &LDA,
            B, &LDB, C, &LDC, D, &LDD, INDEXP, PCOEFF, &LDPCO1,
            &LDPCO2, QCOEFF, &LDQCO1, &LDQCO2, VCOEFF, &LDVCO1,
            &LDVCO2, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TD03AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function td03ay!(MWORK::Integer, PWORK::Integer,
    INDEX::Array{BlasInt,1}, DCOEFF::Array{Float64,2},
    LDDCOE::Integer, UCOEFF::Array{Float64,3}, LDUCO1::Integer,
    LDUCO2::Integer, N::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer)

    INFO = [0]

    ccall((:td03ay_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &MWORK, &PWORK, INDEX, DCOEFF, &LDDCOE, UCOEFF, &LDUCO1,
            &LDUCO2, &N, A, &LDA, B, &LDB, C, &LDC, D, &LDD, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TD03AY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function td04ad!(ROWCOL::Char, M::Integer, P::Integer,
    INDEX::Array{BlasInt,1}, DCOEFF::Array{Float64,2},
    LDDCOE::Integer, UCOEFF::Array{Float64,3}, LDUCO1::Integer,
    LDUCO2::Integer, NR::Integer, A::Array{Float64,2},
    LDA::Integer, B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:td04ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &ROWCOL, &M, &P, INDEX, DCOEFF, &LDDCOE,
            UCOEFF, &LDUCO1, &LDUCO2, &NR, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TD04AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function td05ad!(UNITF::Char, OUTPUT::Char, NP1::Integer,
    MP1::Integer, W::FloatingPoint, A::Array{Float64,1},
    B::Array{Float64,1}, VALR::FloatingPoint,
    VALI::FloatingPoint)

    INFO = [0]

    ccall((:td05ad_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &UNITF, &OUTPUT, &NP1, &MP1, &W, A, B, &VALR, &VALI,
            INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TD05AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01md!(N::Integer, M::Integer, P::Integer, NY::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    U::Array{Float64,2}, LDU::Integer,
    X::Array{Float64,1}, Y::Array{Float64,2},
    LDY::Integer, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tf01md_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &N, &M, &P, &NY, A, &LDA, B, &LDB, C,
            &LDC, D, &LDD, U, &LDU, X, Y, &LDY, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01MD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01mx!(N::Integer, M::Integer, P::Integer, NY::Integer,
    S::Array{Float64,2}, LDS::Integer,
    U::Array{Float64,2}, LDU::Integer,
    X::Array{Float64,1}, Y::Array{Float64,2},
    LDY::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tf01mx_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &N, &M, &P, &NY, S, &LDS, U, &LDU, X, Y, &LDY, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01MX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01my!(N::Integer, M::Integer, P::Integer, NY::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    U::Array{Float64,2}, LDU::Integer,
    X::Array{Float64,1}, Y::Array{Float64,2},
    LDY::Integer, DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tf01my_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &P, &NY, A, &LDA,
            B, &LDB, C, &LDC, D, &LDD, U, &LDU, X, Y, &LDY, DWORK,
            &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01MY: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01nd!(UPLO::Char, N::Integer, M::Integer, P::Integer,
    NY::Integer, A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    D::Array{Float64,2}, LDD::Integer,
    U::Array{Float64,2}, LDU::Integer,
    X::Array{Float64,1}, Y::Array{Float64,2},
    LDY::Integer, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tf01nd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}), &UPLO, &N, &M, &P, &NY, A,
            &LDA, B, &LDB, C, &LDC, D, &LDD, U, &LDU, X, Y, &LDY,
            DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01ND: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01od!(NH1::Integer, NH2::Integer, NR::Integer, NC::Integer,
    H::Array{Float64,2}, LDH::Integer,
    T::Array{Float64,2}, LDT::Integer)

    INFO = [0]

    ccall((:tf01od_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &NH1, &NH2,
            &NR, &NC, H, &LDH, T, &LDT, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01OD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01pd!(NH1::Integer, NH2::Integer, NR::Integer, NC::Integer,
    H::Array{Float64,2}, LDH::Integer,
    T::Array{Float64,2}, LDT::Integer)

    INFO = [0]

    ccall((:tf01pd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &NH1, &NH2,
            &NR, &NC, H, &LDH, T, &LDT, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01PD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01qd!(NC::Integer, NB::Integer, N::Integer,
    IORD::Array{BlasInt,1}, AR::Array{Float64,1},
    MA::Array{Float64,1}, H::Array{Float64,2},
    LDH::Integer)

    INFO = [0]

    ccall((:tf01qd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &NC, &NB, &N,
            IORD, AR, MA, H, &LDH, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01QD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tf01rd!(NA::Integer, NB::Integer, NC::Integer, N::Integer,
    A::Array{Float64,2}, LDA::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    H::Array{Float64,2}, LDH::Integer,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tf01rd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &NA, &NB, &NC, &N, A, &LDA, B, &LDB, C,
            &LDC, H, &LDH, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TF01RD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01ad!(JOB::Char, L::Integer, N::Integer, M::Integer,
    P::Integer, THRESH::FloatingPoint, A::Array{Float64,1},
    LDA::FloatingPoint, E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    LSCALE::Array{Float64,1}, RSCALE::Array{Float64,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tg01ad_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}),
            &JOB, &L, &N, &M, &P, &THRESH, A, &LDA, E, &LDE, B,
            &LDB, C, &LDC, LSCALE, RSCALE, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01AD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01az!(JOB::Char, L::Integer, N::Integer, M::Integer,
    P::Integer, THRESH::FloatingPoint, A::Array{Complex,1},
    LDA::Complex, E::Array{Complex,1}, LDE::Complex,
    B::Array{Complex,1}, LDB::Complex, C::Array{Complex,1},
    LDC::Complex, LSCALE::Array{Float64,1},
    RSCALE::Array{Float64,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tg01az_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}), &JOB, &L, &N,
            &M, &P, &THRESH, A, &LDA, E, &LDE, B, &LDB, C, &LDC,
            LSCALE, RSCALE, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01AZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01bd!(JOBE::Char, COMPQ::Char, COMPZ::Char, N::Integer,
    M::Integer, P::Integer, ILO::Integer, IHI::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tg01bd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOBE, &COMPQ, &COMPZ, &N, &M, &P, &ILO,
            &IHI, A, &LDA, E, &LDE, B, &LDB, C, &LDC, Q, &LDQ, Z,
            &LDZ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01BD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01cd!(COMPQ::Char, L::Integer, N::Integer, M::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tg01cd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &COMPQ, &L, &N, &M, A, &LDA, E, &LDE, B,
            &LDB, Q, &LDQ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01CD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01dd!(COMPZ::Char, L::Integer, N::Integer, P::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tg01dd_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &COMPZ, &L, &N, &P, A, &LDA, E, &LDE, C,
            &LDC, Z, &LDZ, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01DD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01ed!(JOBA::Char, L::Integer, N::Integer, M::Integer,
    P::Integer, A::Array{Float64,1}, LDA::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint, RANKE::Integer,
    RNKA22::Integer, TOL::FloatingPoint,
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tg01ed_, libslicot), Void, (Ptr{Char}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}), &JOBA, &L, &N, &M, &P, A, &LDA, E, &LDE,
            B, &LDB, C, &LDC, Q, &LDQ, Z, &LDZ, &RANKE, &RNKA22,
            &TOL, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01ED: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01fd!(COMPQ::Char, COMPZ::Char, JOBA::Char, L::Integer,
    N::Integer, M::Integer, P::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint, RANKE::Integer,
    RNKA22::Integer, TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, LDWORK::Integer)

    INFO = [0]

    ccall((:tg01fd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}),
            &COMPQ, &COMPZ, &JOBA, &L, &N, &M, &P, A, &LDA, E, &LDE,
            B, &LDB, C, &LDC, Q, &LDQ, Z, &LDZ, &RANKE, &RNKA22,
            &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01FD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01fz!(COMPQ::Char, COMPZ::Char, JOBA::Char, L::Integer,
    N::Integer, M::Integer, P::Integer, A::Array{Complex,1},
    LDA::Complex, E::Array{Complex,1}, LDE::Complex,
    B::Array{Complex,1}, LDB::Complex, C::Array{Complex,1},
    LDC::Complex, Q::Array{Complex,1}, LDQ::Complex,
    Z::Array{Complex,1}, LDZ::Complex, RANKE::Integer,
    RNKA22::Integer, TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1}, ZWORK::Array{Complex,1},
    LZWORK::Integer)

    INFO = [0]

    ccall((:tg01fz_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{Complex128}, Ptr{Complex128},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{Complex128}, Ptr{BlasInt}, Ptr{BlasInt}), &COMPQ,
            &COMPZ, &JOBA, &L, &N, &M, &P, A, &LDA, E, &LDE, B,
            &LDB, C, &LDC, Q, &LDQ, Z, &LDZ, &RANKE, &RNKA22, &TOL,
            IWORK, DWORK, ZWORK, &LZWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01FZ: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01hd!(JOBCON::Char, COMPQ::Char, COMPZ::Char, N::Integer,
    M::Integer, P::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint, NCONT::Integer,
    NIUCON::Integer, NRBLCK::Integer, RTAU::Array{BlasInt,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tg01hd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &JOBCON, &COMPQ, &COMPZ, &N, &M, &P, A, &LDA, E, &LDE,
            B, &LDB, C, &LDC, Q, &LDQ, Z, &LDZ, &NCONT, &NIUCON,
            &NRBLCK, RTAU, &TOL, IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01HD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01hx!(COMPQ::Char, COMPZ::Char, L::Integer, N::Integer,
    M::Integer, P::Integer, N1::Integer, LBE::Integer,
    A::Array{Float64,1}, LDA::FloatingPoint,
    E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint, NR::Integer,
    NRBLCK::Integer, RTAU::Array{BlasInt,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tg01hx_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}), &COMPQ, &COMPZ, &L, &N, &M, &P, &N1,
            &LBE, A, &LDA, E, &LDE, B, &LDB, C, &LDC, Q, &LDQ, Z,
            &LDZ, &NR, &NRBLCK, RTAU, &TOL, IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01HX: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01id!(JOBOBS::Char, COMPQ::Char, COMPZ::Char, N::Integer,
    M::Integer, P::Integer, A::Array{Float64,1},
    LDA::FloatingPoint, E::Array{Float64,1}, LDE::FloatingPoint,
    B::Array{Float64,1}, LDB::FloatingPoint,
    C::Array{Float64,1}, LDC::FloatingPoint,
    Q::Array{Float64,1}, LDQ::FloatingPoint,
    Z::Array{Float64,1}, LDZ::FloatingPoint, NOBSV::Integer,
    NIUOBS::Integer, NLBLCK::Integer, CTAU::Array{BlasInt,1},
    TOL::FloatingPoint, IWORK::Array{BlasInt,1},
    DWORK::Array{Float64,1})

    INFO = [0]

    ccall((:tg01id_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}),
            &JOBOBS, &COMPQ, &COMPZ, &N, &M, &P, A, &LDA, E, &LDE,
            B, &LDB, C, &LDC, Q, &LDQ, Z, &LDZ, &NOBSV, &NIUOBS,
            &NLBLCK, CTAU, &TOL, IWORK, DWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01ID: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01jd!(JOB::Char, SYSTYP::Char, EQUIL::Char, N::Integer,
    M::Integer, P::Integer, A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer, NR::Integer,
    INFRED::Array{BlasInt,1}, TOL::FloatingPoint,
    IWORK::Array{BlasInt,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:tg01jd_, libslicot), Void, (Ptr{Char}, Ptr{Char},
            Ptr{Char}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &JOB,
            &SYSTYP, &EQUIL, &N, &M, &P, A, &LDA, E, &LDE, B, &LDB,
            C, &LDC, &NR, INFRED, &TOL, IWORK, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01JD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end


function tg01wd!(N::Integer, M::Integer, P::Integer,
    A::Array{Float64,2}, LDA::Integer,
    E::Array{Float64,2}, LDE::Integer,
    B::Array{Float64,2}, LDB::Integer,
    C::Array{Float64,2}, LDC::Integer,
    Q::Array{Float64,2}, LDQ::Integer,
    Z::Array{Float64,2}, LDZ::Integer,
    ALPHAR::Array{Float64,1}, ALPHAI::Array{Float64,1},
    BETA::Array{Float64,1}, DWORK::Array{Float64,1},
    LDWORK::Integer)

    INFO = [0]

    ccall((:tg01wd_, libslicot), Void, (Ptr{BlasInt}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{BlasInt}, Ptr{Float64},
            Ptr{BlasInt}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
            Ptr{Float64}, Ptr{BlasInt}, Ptr{BlasInt}), &N, &M, &P,
            A, &LDA, E, &LDE, B, &LDB, C, &LDC, Q, &LDQ, Z, &LDZ,
            ALPHAR, ALPHAI, BETA, DWORK, &LDWORK, INFO)
    
    if INFO[1] < 0
        throw(SlicotException(INFO[1], @sprintf("SlicotError in TG01WD: the 
        %dth argument had an illegal value", -INFO[1])))
    end
end

end     #module
