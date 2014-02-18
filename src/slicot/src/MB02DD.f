      SUBROUTINE MB02DD( JOB, TYPET, K, M, N, TA, LDTA, T, LDT, G,
     $                   LDG, R, LDR, L, LDL, CS, LCS, DWORK, LDWORK,
     $                   INFO )
C
C     SLICOT RELEASE 5.0.
C
C     Copyright (c) 2002-2010 NICONET e.V.
C
C     This program is free software: you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation, either version 2 of
C     the License, or (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program.  If not, see
C     <http://www.gnu.org/licenses/>.
C
C     PURPOSE
C
C     To update the Cholesky factor and the generator and/or the
C     Cholesky factor of the inverse of a symmetric positive definite
C     (s.p.d.) block Toeplitz matrix T, given the information from
C     a previous factorization and additional blocks in TA of its first
C     block row, or its first block column, depending on the routine
C     parameter TYPET. Transformation information is stored.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the output of the routine, as follows:
C             = 'R':  updates the generator G of the inverse and
C                     computes the new columns / rows for the Cholesky
C                     factor R of T;
C             = 'A':  updates the generator G, computes the new
C                     columns / rows for the Cholesky factor R of T and
C                     the new rows / columns for the Cholesky factor L
C                     of the inverse;
C             = 'O':  only computes the new columns / rows for the
C                     Cholesky factor R of T.
C
C     TYPET   CHARACTER*1
C             Specifies the type of T, as follows:
C             = 'R':  the first block row of an s.p.d. block Toeplitz
C                     matrix was/is defined; if demanded, the Cholesky
C                     factors R and L are upper and lower triangular,
C                     respectively, and G contains the transposed
C                     generator of the inverse;
C             = 'C':  the first block column of an s.p.d. block Toeplitz
C                     matrix was/is defined; if demanded, the Cholesky
C                     factors R and L are lower and upper triangular,
C                     respectively, and G contains the generator of the
C                     inverse. This choice results in a column oriented
C                     algorithm which is usually faster.
C             Note:   in this routine, the notation x / y means that
C                     x corresponds to TYPET = 'R' and y corresponds to
C                     TYPET = 'C'.
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of rows / columns in T, which should be equal
C             to the blocksize.  K >= 0.
C
C     M       (input)  INTEGER
C             The number of blocks in TA.  M >= 0.
C
C     N       (input)  INTEGER
C             The number of blocks in T.  N >= 0.
C
C     TA      (input/output)  DOUBLE PRECISION array, dimension
C             (LDTA,M*K) / (LDTA,K)
C             On entry, the leading K-by-M*K / M*K-by-K part of this
C             array must contain the (N+1)-th to (N+M)-th blocks in the
C             first block row / column of an s.p.d. block Toeplitz
C             matrix.
C             On exit, if INFO = 0, the leading K-by-M*K / M*K-by-K part
C             of this array contains information on the Householder
C             transformations used, such that the array
C
C                        [ T  TA ]    /    [ T  ]
C                                          [ TA ]
C
C             serves as the new transformation matrix T for further
C             applications of this routine.
C
C     LDTA    INTEGER
C             The leading dimension of the array TA.
C             LDTA >= MAX(1,K),   if TYPET = 'R';
C             LDTA >= MAX(1,M*K), if TYPET = 'C'.
C
C     T       (input)  DOUBLE PRECISION array, dimension (LDT,N*K) /
C             (LDT,K)
C             The leading K-by-N*K / N*K-by-K part of this array must
C             contain transformation information generated by the SLICOT
C             Library routine MB02CD, i.e., in the first K-by-K block,
C             the upper / lower Cholesky factor of T(1:K,1:K), and in
C             the remaining part, the Householder transformations
C             applied during the initial factorization process.
C
C     LDT     INTEGER
C             The leading dimension of the array T.
C             LDT >= MAX(1,K),    if TYPET = 'R';
C             LDT >= MAX(1,N*K),  if TYPET = 'C'.
C
C     G       (input/output)  DOUBLE PRECISION array, dimension
C             (LDG,( N + M )*K) / (LDG,2*K)
C             On entry, if JOB = 'R', or 'A', then the leading
C             2*K-by-N*K / N*K-by-2*K part of this array must contain,
C             in the first K-by-K block of the second block row /
C             column, the lower right block of the Cholesky factor of
C             the inverse of T, and in the remaining part, the generator
C             of the inverse of T.
C             On exit, if INFO = 0 and JOB = 'R', or 'A', then the
C             leading 2*K-by-( N + M )*K / ( N + M )*K-by-2*K part of
C             this array contains the same information as on entry, now
C             for the updated Toeplitz matrix. Actually, to obtain a
C             generator of the inverse one has to set
C               G(K+1:2*K, 1:K) = 0,    if TYPET = 'R';
C               G(1:K, K+1:2*K) = 0,    if TYPET = 'C'.
C
C     LDG     INTEGER
C             The leading dimension of the array G.
C             LDG >= MAX(1,2*K),  if TYPET = 'R' and JOB = 'R', or 'A';
C             LDG >= MAX(1,( N + M )*K),
C                                 if TYPET = 'C' and JOB = 'R', or 'A';
C             LDG >= 1,           if JOB = 'O'.
C
C     R       (input/output)  DOUBLE PRECISION array, dimension
C             (LDR,M*K) / (LDR,( N + M )*K)
C             On input, the leading N*K-by-K part of R(K+1,1) /
C             K-by-N*K part of R(1,K+1) contains the last block column /
C             row of the previous Cholesky factor R.
C             On exit, if INFO = 0, then the leading
C             ( N + M )*K-by-M*K / M*K-by-( N + M )*K part of this
C             array contains the last M*K columns / rows of the upper /
C             lower Cholesky factor of T. The elements in the strictly
C             lower / upper triangular part are not referenced.
C
C     LDR     INTEGER
C             The leading dimension of the array R.
C             LDR >= MAX(1, ( N + M )*K), if TYPET = 'R';
C             LDR >= MAX(1, M*K),         if TYPET = 'C'.
C
C     L       (output)  DOUBLE PRECISION array, dimension
C             (LDL,( N + M )*K) / (LDL,M*K)
C             If INFO = 0 and JOB = 'A', then the leading
C             M*K-by-( N + M )*K / ( N + M )*K-by-M*K part of this
C             array contains the last M*K rows / columns of the lower /
C             upper Cholesky factor of the inverse of T. The elements
C             in the strictly upper / lower triangular part are not
C             referenced.
C
C     LDL     INTEGER
C             The leading dimension of the array L.
C             LDL >= MAX(1, M*K),         if TYPET = 'R' and JOB = 'A';
C             LDL >= MAX(1, ( N + M )*K), if TYPET = 'C' and JOB = 'A';
C             LDL >= 1,                   if JOB = 'R', or 'O'.
C
C     CS      (input/output)  DOUBLE PRECISION array, dimension (LCS)
C             On input, the leading 3*(N-1)*K part of this array must
C             contain the necessary information about the hyperbolic
C             rotations and Householder transformations applied
C             previously by SLICOT Library routine MB02CD.
C             On exit, if INFO = 0, then the leading 3*(N+M-1)*K part of
C             this array contains information about all the hyperbolic
C             rotations and Householder transformations applied during
C             the whole process.
C
C     LCS     INTEGER
C             The length of the array CS.  LCS >= 3*(N+M-1)*K.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0,  DWORK(1)  returns the optimal
C             value of LDWORK.
C             On exit, if  INFO = -19,  DWORK(1)  returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= MAX(1,(N+M-1)*K).
C             For optimum performance LDWORK should be larger.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the reduction algorithm failed. The block Toeplitz
C                   matrix associated with [ T  TA ] / [ T'  TA' ]' is
C                   not (numerically) positive definite.
C
C     METHOD
C
C     Householder transformations and modified hyperbolic rotations
C     are used in the Schur algorithm [1], [2].
C
C     REFERENCES
C
C     [1] Kailath, T. and Sayed, A.
C         Fast Reliable Algorithms for Matrices with Structure.
C         SIAM Publications, Philadelphia, 1999.
C
C     [2] Kressner, D. and Van Dooren, P.
C         Factorizations and linear system solvers for matrices with
C         Toeplitz structure.
C         SLICOT Working Note 2000-2, 2000.
C
C     NUMERICAL ASPECTS
C
C     The implemented method is numerically stable.
C                               3         2
C     The algorithm requires 0(K ( N M + M ) ) floating point
C     operations.
C
C     FURTHER COMMENTS
C
C     For min(K,N,M) = 0, the routine sets DWORK(1) = 1 and returns.
C     Although the calculations could still be performed when N = 0,
C     but min(K,M) > 0, this case is not considered as an "update".
C     SLICOT Library routine MB02CD should be called with the argument
C     M instead of N.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Chemnitz, Germany, December 2000.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2000,
C     Feb. 2004.
C
C     KEYWORDS
C
C     Elementary matrix operations, Householder transformation, matrix
C     operations, Toeplitz matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOB, TYPET
      INTEGER           INFO, K, LCS, LDG, LDL, LDR, LDT, LDTA, LDWORK,
     $                  M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  CS(*), DWORK(*), G(LDG, *), L(LDL,*), R(LDR,*),
     $                  T(LDT,*), TA(LDTA,*)
C     .. Local Scalars ..
      INTEGER           I, IERR, J, MAXWRK, STARTI, STARTR, STARTT
      LOGICAL           COMPG, COMPL, ISROW
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DLACPY, DLASET, DTRSM, MB02CX, MB02CY,
     $                  XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         INT, MAX, MIN
C
C     .. Executable Statements ..
C
C     Decode the scalar input parameters.
C
      INFO  = 0
      COMPL = LSAME( JOB, 'A' )
      COMPG = LSAME( JOB, 'R' ) .OR. COMPL
      ISROW = LSAME( TYPET, 'R' )
C
C     Check the scalar input parameters.
C
      IF ( .NOT.( COMPG .OR. LSAME( JOB, 'O' ) ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.( ISROW .OR. LSAME( TYPET, 'C' ) ) ) THEN
         INFO = -2
      ELSE IF ( K.LT.0 ) THEN
         INFO = -3
      ELSE IF ( M.LT.0 ) THEN
         INFO = -4
      ELSE IF ( N.LT.0 ) THEN
         INFO = -5
      ELSE IF ( LDTA.LT.1 .OR. ( ISROW .AND. LDTA.LT.K ) .OR.
     $                    ( .NOT.ISROW .AND. LDTA.LT.M*K ) ) THEN
         INFO = -7
      ELSE IF ( LDT.LT.1 .OR. ( ISROW .AND. LDT.LT.K ) .OR.
     $                   ( .NOT.ISROW .AND. LDT.LT.N*K ) ) THEN
         INFO = -9
      ELSE IF ( ( COMPG .AND. ( ( ISROW .AND. LDG.LT.2*K )
     $                .OR. ( .NOT.ISROW .AND. LDG.LT.( N + M )*K ) ) )
     $          .OR. LDG.LT.1 ) THEN
         INFO = -11
      ELSE IF ( ( (      ISROW .AND. LDR.LT.( N + M )*K ) .OR.
     $            ( .NOT.ISROW .AND. LDR.LT.M*K ) ) .OR.
     $          LDR.LT.1 ) THEN
         INFO = -13
      ELSE IF ( ( COMPL .AND. ( ( ISROW .AND. LDL.LT.M*K )
     $                .OR. ( .NOT.ISROW .AND. LDL.LT.( N + M )*K ) ) )
     $          .OR. LDL.LT.1 ) THEN
         INFO = -15
      ELSE IF ( LCS.LT.3*( N + M - 1 )*K ) THEN
         INFO = -17
      ELSE IF ( LDWORK.LT.MAX( 1, ( N + M - 1 )*K ) ) THEN
         DWORK(1) = MAX( 1, ( N + M - 1 )*K )
         INFO = -19
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB02DD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( MIN( K, N, M ).EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
      MAXWRK = 1
      IF ( ISROW ) THEN
C
C        Apply Cholesky factor of T(1:K, 1:K) on TA.
C
         CALL DTRSM( 'Left', 'Upper', 'Transpose', 'NonUnit', K, M*K,
     $               ONE, T, LDT, TA, LDTA )
C
C        Initialize the output matrices.
C
         IF ( COMPG ) THEN
            CALL DLASET( 'All', K, M*K, ZERO, ZERO, G(1,N*K+1), LDG )
            IF ( M.GE.N-1 .AND. N.GT.1 ) THEN
               CALL DLACPY( 'All', K, (N-1)*K, G(K+1,K+1), LDG,
     $                      G(K+1,K*(M+1)+1), LDG )
            ELSE
               DO 10  I = N*K, K + 1, -1
                  CALL DCOPY( K, G(K+1,I), 1, G(K+1,M*K+I), 1 )
   10          CONTINUE
            END IF
            CALL DLASET( 'All', K, M*K, ZERO, ZERO, G(K+1,K+1), LDG )
         END IF
C
         CALL DLACPY( 'All', K, M*K, TA, LDTA, R, LDR )
C
C        Apply the stored transformations on the new columns.
C
         DO 20  I = 2, N
C
C           Copy the last M-1 blocks of the positive generator together;
C           the last M blocks of the negative generator are contained
C           in TA.
C
            STARTR =   ( I - 1 )*K + 1
            STARTT = 3*( I - 2 )*K + 1
            CALL DLACPY( 'All', K, (M-1)*K, R(STARTR-K,1), LDR,
     $                   R(STARTR,K+1), LDR )
C
C           Apply the transformations stored in T on the generator.
C
            CALL MB02CY( 'Row', 'NoStructure', K, K, M*K, K,
     $                   R(STARTR,1), LDR, TA, LDTA, T(1,STARTR), LDT,
     $                   CS(STARTT), 3*K, DWORK, LDWORK, IERR )
            MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
   20    CONTINUE
C
C        Now, we have "normality" and can apply further M Schur steps.
C
         DO 30  I = 1, M
C
C           Copy the first M-I+1 blocks of the positive generator
C           together; the first M-I+1 blocks of the negative generator
C           are contained in TA.
C
            STARTT = 3*( N + I - 2 )*K + 1
            STARTI = ( M - I + 1 )*K + 1
            STARTR = ( N + I - 1 )*K + 1
            IF ( I.EQ.1 ) THEN
               CALL DLACPY( 'All', K, (M-1)*K, R(STARTR-K,1), LDR,
     $                      R(STARTR,K+1), LDR )
            ELSE
               CALL DLACPY( 'Upper', K, (M-I+1)*K,
     $                      R(STARTR-K,(I-2)*K+1), LDR,
     $                      R(STARTR,(I-1)*K+1), LDR )
            END IF
C
C           Reduce the generator to proper form.
C
            CALL MB02CX( 'Row', K, K, K, R(STARTR,(I-1)*K+1), LDR,
     $                   TA(1,(I-1)*K+1), LDTA, CS(STARTT), 3*K, DWORK,
     $                   LDWORK, IERR )
            IF ( IERR.NE.0 )  THEN
C
C              Error return:  The matrix is not positive definite.
C
               INFO = 1
               RETURN
            END IF
C
            MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
            IF ( M.GT.I ) THEN
               CALL MB02CY( 'Row', 'NoStructure', K, K, (M-I)*K, K,
     $                      R(STARTR,I*K+1), LDR, TA(1,I*K+1), LDTA,
     $                      TA(1,(I-1)*K+1), LDTA, CS(STARTT), 3*K,
     $                      DWORK, LDWORK, IERR )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
            END IF
C
            IF ( COMPG ) THEN
C
C              Transformations acting on the inverse generator:
C
               CALL MB02CY( 'Row', 'Triangular', K, K, K, K, G(K+1,1),
     $                      LDG, G(1,STARTR), LDG, TA(1,(I-1)*K+1),
     $                      LDTA, CS(STARTT), 3*K, DWORK, LDWORK, IERR )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
C
               CALL MB02CY( 'Row', 'NoStructure', K, K, (N+I-1)*K, K,
     $                      G(K+1,STARTI), LDG, G, LDG, TA(1,(I-1)*K+1),
     $                      LDTA, CS(STARTT), 3*K, DWORK, LDWORK, IERR )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
C
               IF ( COMPL ) THEN
                  CALL DLACPY( 'All', K, (N+I-1)*K, G(K+1,STARTI), LDG,
     $                         L((I-1)*K+1,1), LDL )
                  CALL DLACPY( 'Lower', K, K, G(K+1,1), LDG,
     $                         L((I-1)*K+1,STARTR), LDL )
               END IF
C
            END IF
   30    CONTINUE
C
      ELSE
C
C        Apply Cholesky factor of T(1:K, 1:K) on TA.
C
         CALL DTRSM( 'Right', 'Lower', 'Transpose', 'NonUnit', M*K, K,
     $               ONE, T, LDT, TA, LDTA )
C
C        Initialize the output matrices.
C
         IF ( COMPG ) THEN
            CALL DLASET( 'All', M*K, K, ZERO, ZERO, G(N*K+1,1), LDG )
            IF ( M.GE.N-1 .AND. N.GT.1 ) THEN
               CALL DLACPY( 'All', (N-1)*K, K, G(K+1,K+1), LDG,
     $                      G(K*(M+1)+1,K+1), LDG )
            ELSE
               DO 40  I = 1, K
                  DO 35  J = N*K, K + 1, -1
                     G(J+M*K,K+I) = G(J,K+I)
   35             CONTINUE
   40          CONTINUE
            END IF
            CALL DLASET( 'All', M*K, K, ZERO, ZERO, G(K+1,K+1), LDG )
         END IF
C
         CALL DLACPY( 'All', M*K, K, TA, LDTA, R, LDR )
C
C        Apply the stored transformations on the new rows.
C
         DO 50  I = 2, N
C
C           Copy the last M-1 blocks of the positive generator together;
C           the last M blocks of the negative generator are contained
C           in TA.
C
            STARTR =   ( I - 1 )*K + 1
            STARTT = 3*( I - 2 )*K + 1
            CALL DLACPY( 'All', (M-1)*K, K, R(1,STARTR-K), LDR,
     $                   R(K+1,STARTR), LDR )
C
C           Apply the transformations stored in T on the generator.
C
            CALL MB02CY( 'Column', 'NoStructure', K, K, M*K, K,
     $                   R(1,STARTR), LDR, TA, LDTA, T(STARTR,1), LDT,
     $                   CS(STARTT), 3*K, DWORK, LDWORK, IERR )
            MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
   50    CONTINUE
C
C        Now, we have "normality" and can apply further M Schur steps.
C
         DO 60  I = 1, M
C
C           Copy the first M-I+1 blocks of the positive generator
C           together; the first M-I+1 blocks of the negative generator
C           are contained in TA.
C
            STARTT = 3*( N + I - 2 )*K + 1
            STARTI = ( M - I + 1 )*K + 1
            STARTR = ( N + I - 1 )*K + 1
            IF ( I.EQ.1 ) THEN
               CALL DLACPY( 'All', (M-1)*K, K, R(1,STARTR-K), LDR,
     $                      R(K+1,STARTR), LDR )
            ELSE
               CALL DLACPY( 'Lower', (M-I+1)*K, K,
     $                      R((I-2)*K+1,STARTR-K), LDR,
     $                      R((I-1)*K+1,STARTR), LDR )
            END IF
C
C           Reduce the generator to proper form.
C
            CALL MB02CX( 'Column', K, K, K, R((I-1)*K+1,STARTR), LDR,
     $                   TA((I-1)*K+1,1), LDTA, CS(STARTT), 3*K, DWORK,
     $                   LDWORK, IERR )
            IF ( IERR.NE.0 )  THEN
C
C              Error return:  The matrix is not positive definite.
C
               INFO = 1
               RETURN
            END IF
C
            MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
            IF ( M.GT.I ) THEN
               CALL MB02CY( 'Column', 'NoStructure', K, K, (M-I)*K, K,
     $                      R(I*K+1,STARTR), LDR, TA(I*K+1,1), LDTA,
     $                      TA((I-1)*K+1,1), LDTA, CS(STARTT), 3*K,
     $                      DWORK, LDWORK, IERR )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
            END IF
C
            IF ( COMPG ) THEN
C
C              Transformations acting on the inverse generator:
C
               CALL MB02CY( 'Column', 'Triangular', K, K, K, K,
     $                      G(1,K+1), LDG, G(STARTR,1), LDG,
     $                      TA((I-1)*K+1,1), LDTA, CS(STARTT), 3*K,
     $                      DWORK, LDWORK, IERR )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
C
               CALL MB02CY( 'Column', 'NoStructure', K, K, (N+I-1)*K, K,
     $                      G(STARTI,K+1), LDG, G, LDG, TA((I-1)*K+1,1),
     $                      LDTA, CS(STARTT), 3*K, DWORK, LDWORK, IERR )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
C
               IF ( COMPL ) THEN
                  CALL DLACPY( 'All', (N+I-1)*K, K, G(STARTI,K+1), LDG,
     $                         L(1,(I-1)*K+1), LDL )
                  CALL DLACPY( 'Upper', K, K, G(1,K+1), LDG,
     $                         L(STARTR,(I-1)*K+1), LDL )
               END IF
C
            END IF
   60    CONTINUE
C
      END IF
C
      DWORK(1) = MAXWRK
C
      RETURN
C
C *** Last line of MB02DD ***
      END
