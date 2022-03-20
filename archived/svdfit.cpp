// /* Slightly modified versions of routines from
//  * Press, William H., Brian P. Flannery, Saul A Teukolsky and
//  *   William T. Vetterling, 1986, "Numerical Recipes: The Art of
//  *   Scientific Computing" (Fortran), Cambrigde University Press.
//  *
//  * svdfit  on p. 518.
//  * svbksb  on pp. 57-58.
//  * svdcmp  on pp. 60-64.
//  * svdvar  on p. 519.
//  */
// 
// #include <stdio.h>
// #include <math.h>
// 
// void funcs( double x, double *afunc, unsigned int ma );
// void svdfit( double *X, double *Y, double *Sig, unsigned int NData,
//     double *A, unsigned int MA,
//     double **U, double **V, double *W, unsigned int MP, unsigned int NP,
//     double *ChiSq, void funcs(double x, double *afunc, unsigned int ma) );
// void svdcmp( double **A, unsigned int M, unsigned int N,
//     unsigned int MP, unsigned int NP, double *W, double **V );
// void svbksb( double **U, double *W, double **V, unsigned int M,
//     unsigned int N, unsigned int MP, unsigned int NP,
//     double *B, double *X );
// void svdvar( double **V, unsigned int MA, unsigned int NP,
//     double *W, double **CVM, unsigned int NCVM );
// 
// # define true ((int)1)
// # define false ((int)0)
// # define nmax ((int)1000)
// # define mmax ((int)50)
// # define tol ((double)1.0e-15)
// 
// void svdfit( double *X, double *Y, double *Sig, unsigned int NData,
//     double *A, unsigned int MA, 
//     double **U, double **V, double *W, unsigned int MP, unsigned int NP,
//     double *ChiSq, void funcs(double x, double *afunc, unsigned int ma) )
// {
//     /*
//        Given a set of NData points X[], Y[] with individual standard
//        deviations of Sig[], use chi-square minimization to determine the
//        MA coefficients, A[], of the fitting function
//        y = sum over i Ai * funcsi(x).
//        Here we solve the fitting equation using singular value decomposition
//        of the NData by MA matrix. The arrays U, V and W provide workspace
//        on input. On output they define the singular value decomposition and
//        can be used to obtaint he covariance matrix. MP and NP are the
//        physical dimensions of the matrices U, V, and W as indicated below.
//        It is necessary that MP be greater than or equal to NData and that
//        NP be greather than or equal to MP. The program returns values for
//        the MA fit parameters A[] and the chi-square, ChiSq. The user
//        supplies a subroutine, funcs(), that returns the MA basis functions
//        evaluated at x in the array afunc[].
//     */
// 
//     int i;
//     int j;
//     double sum;
//     double thresh;
//     double tmp;
//     double wmax;
//     double wmin;
// 
//     double beta[nmax];
//     double afunc[mmax];
// 
//     /* Accumulate coefficients of the fitting matrix. */
//     for( i = 0; i < NData; ++i ) {
//         funcs( X[i], afunc, MA );
//         tmp = 1.0 / Sig[i];
//         for( j = 0; j < MA; ++j ) {
//             U[i][j] = afunc[j] * tmp;
//         }
//         beta[i] = Y[i] * tmp;
//     }
// 
//     /* Singular value decomposition. */
//     svdcmp( U, NData, MA, MP, NP, W, V );
// 
//     /* Edit the singular values, given tol from the parameter statement,
//        between here ... */
//     wmax = 0.0;
//     wmin = 1.0e99;
//     for( j = 0; j < MA; ++j ) {
//         if( W[j] > wmax )
//             wmax = W[j];
//         if( W[j] < wmin )
//             wmin = W[j];
//     }
// 
//     thresh = tol * wmax;
//     for( j = 0; j < MA; ++j ) {
//         if( W[j] < thresh ) {
//             W[j] = 0.0;
//         }
//     }
//     /* ... and here. */
// 
//     svbksb( U, W, V, NData, MA, MP, NP, beta, A );
// 
//     /* Evaluate chi-square. */
//     *ChiSq = 0.0;
//     for( i = 0; i < NData; ++i ) {
//         funcs( X[i], afunc, MA );
//         sum = 0.0;
//         for( j = 0; j < MA; ++j ) {
//             sum = sum + A[j] * afunc[j];
//         }
//         tmp = ((Y[i] - sum) / Sig[i]);
//         *ChiSq = *ChiSq + tmp*tmp;
//     }
// 
//     return;
// }
// 
// void svdvar( double **V, unsigned int MA, unsigned int NP,
//     double *W, double **CVM, unsigned int NCVM )
// {
//     /*
//        To evaluate the covariance matrix CVM of the fit for MA paramaters
//        obtained by svdfit, call this routine with matrix V and W as returned
//        from svdfit. NP, NCVM give the physical dimensions of V, W and CVM as
//        indicated below.
//     */
// 
//     int i;
//     int j;
//     int k;
//     double sum;
// 
//     double wti[mmax];
// 
//     for( i = 0; i < MA; ++i ) {
//         wti[i] = 0.0;
//         if( W[i] != 0.0 )
//             wti[i] = 1.0 / (W[i] * W[i]);
//     }
// 
//     for( i = 0; i < MA; ++i ) {
//         for( j = 0; j <= i; ++j ) {
//             sum = 0.0;
//             for( k = 0; k < MA; ++k ) {
//                 sum = sum + V[i][k] * V[j][k] * wti[k];
//             }
//             CVM[i][j] = sum;
//             CVM[j][i] = sum;
//         }
//     }
// 
//     return;
// }
// 
// void svbksb( double **U, double *W, double **V, unsigned int M,
//     unsigned int N, unsigned int MP, unsigned int NP,
//     double *B, double *X )
// {
//     /*
//        Solves A * X = B for a vector X where A is specified by the arrays
//        U, W and V as returned by svdcmp. M and N are the logical dimensions
//        of A and will be equal for a square matrices. MP and NP are the
//        physical dimensions of A. B is the input right-hand side. X is the
//        output solution vector. No input quantities are destroyed, so the
//        routine may be called sequentially with different B's. M must be
//        greater to N (see svdcmp).
//     */
// 
//     int i;
//     int j;
//     double S;
// 
//     double tmp[nmax];
// 
//     /* Calculate transpose U * B */
//     for( j = 0; j < N; ++j ) {
//         S = 0.0;
//         /* Nonzero result only if W[j] is nonzero. */
//         if( W[j] != 0.0 ) {
//             for( i = 0; i < M; ++i ) {
//                 S = S + U[i][j] * B[i];
//             }
//             S = S / W[j];
//         }
//         tmp[j] = S;
//     }
// 
//     /* Multiply by V to get answer. */
//     for( j = 0; j < N; ++j ) {
//         S = 0.0;
//         for( i = 0; i < N; ++i ) {
//             S = S + V[j][i] * tmp[i];
//         }
//         X[j] = S;
//     }
// 
//     return;
// }
// 
// void svdcmp( double **A, unsigned int M, unsigned int N,
//     unsigned int MP, unsigned int NP, double *W, double **V )
// {
//     /*
//        Give a matrix A, with logical dimensions M by N and physical
//        dimensions MP by NP, this routine computes its singular value
//        decomposition, A = U * W * transpose V. The matrix U replaces
//        A on output. The diagonal matrix of singular values, W, is output
//        as a vector W. The matrix V (not the transpose of V) is output as
//        V. M must be greater or equal to N. If it is smaller then A should
//        be filled up to square with zero rows.
//     */
// 
//     double rv1[nmax];
// 
//     /* Householder reduction to bidiagonal form. */
//     int NM;
//     double C;
//     double F;
//     double G = 0.0;
//     double H;
//     double S;
//     double X;
//     double Y;
//     double Z;
//     double Scale = 0.0;
//     double ANorm = 0.0;
//     double tmp;
//     int flag;
//     int i;
//     int its;
//     int j;
//     int jj;
//     int k;
//     int l;
// 
//     if( M < N ) {
//         fprintf( stderr, "You must augment A with extra zero rows.\n" );
//         return;
//     }
// 
//     for( i = 0; i < N; ++i ) {
//         l = i + 1;
//         rv1[i] = Scale * G;
//         G = 0.0;
//         S = 0.0;
//         Scale = 0.0;
//         if( i < M ) {
//             for( k = i; k < M; ++k ) {
//                 Scale = Scale + fabs( A[k][i] );
//             }
//             if( Scale != 0.0 ) {
//                 for( k = i; k < M; ++k ) {
//                     A[k][i] = A[k][i] / Scale;
//                     S = S + A[k][i] * A[k][i];
//                 }
//                 F = A[i][i];
//                 G = sqrt(S);
//                 if( F > 0.0 ) {
//                     G = -G;
//                 }
//                 H = F * G - S;
//                 A[i][i] = F - G;
//                 if( i != (N-1) ) {
//                     for( j = l; j < N; ++j ) {
//                         S = 0.0;
//                         for( k = i; k < M; ++k ) {
//                             S = S + A[k][i] * A[k][j];
//                         }
//                         F = S / H;
//                         for( k = i; k < M; ++k ) {
//                             A[k][j] = A[k][j] + F * A[k][i];
//                         }
//                     }
//                 }
//                 for( k = i; k < M; ++k ) {
//                     A[k][i] = Scale * A[k][i];
//                 }
//             }
//         }
// 
//         W[i] = Scale * G;
//         G = 0.0;
//         S = 0.0;
//         Scale = 0.0;
//         if( (i < M) && (i != (N-1)) ) {
//             for( k = l; k < N; ++k ) {
//                 Scale = Scale + fabs( A[i][k] );
//             }
//             if( Scale != 0.0 ) {
//                 for( k = l; k < N; ++k ) {
//                     A[i][k] = A[i][k] / Scale;
//                     S = S + A[i][k] * A[i][k];
//                 }
//                 F = A[i][l];
//                 G = sqrt(S);
//                 if( F > 0.0 ) {
//                     G = -G;
//                 }
//                 H = F * G - S;
//                 A[i][l] = F - G;
//                 for( k = l; k < N; ++k ) {
//                     rv1[k] = A[i][k] / H;
//                 }
//                 if( i != (M-1) ) {
//                     for( j = l; j < M; ++j ) {
//                         S = 0.0;
//                         for( k = l; k < N; ++k ) {
//                             S = S + A[j][k] * A[i][k];
//                         }
//                         for( k = l; k < N; ++k ) {
//                             A[j][k] = A[j][k] + S * rv1[k];
//                         }
//                     }
//                 }
//                 for( k = l; k < N; ++k ) {
//                     A[i][k] = Scale * A[i][k];
//                 }
//             }
//         }
//         tmp = fabs( W[i] ) + fabs( rv1[i] );
//         if( tmp > ANorm )
//             ANorm = tmp;
//     }
// 
//     /* Accumulation of right-hand transformations. */
//     for( i = N-1; i >= 0; --i ) {
//         if( i < (N-1) ) {
//             if( G != 0.0 ) {
//                 for( j = l; j < N; ++j ) {
//                     V[j][i] = (A[i][j] / A[i][l]) / G;
//                 }
//                 for( j = l; j < N; ++j ) {
//                     S = 0.0;
//                     for( k = l; k < N; ++k ) {
//                         S = S + A[i][k] * V[k][j];
//                     }
//                     for( k = l; k < N; ++k ) {
//                         V[k][j] = V[k][j] + S * V[k][i];
//                     }
//                 }
//             }
//             for( j = l; j < N; ++j ) {
//                 V[i][j] = 0.0;
//                 V[j][i] = 0.0;
//             }
//         }
//         V[i][i] = 1.0;
//         G = rv1[i];
//         l = i;
//     }
// 
//     /* Accumulation of left-hand transformations. */
//     for( i = N-1; i >= 0; --i ) {
//         l = i + 1;
//         G = W[i];
//         if( i < (N-1) ) {
//             for( j = l; j < N; ++j ) {
//                 A[i][j] = 0.0;
//             }
//         }
//         if( G != 0.0 ) {
//             G = 1.0 / G;
//             if( i != (N-1) ) {
//                 for( j = l; j < N; ++j ) {
//                     S = 0.0;
//                     for( k = l; k < M; ++k ) {
//                         S = S + A[k][i] * A[k][j];
//                     }
//                     F = (S / A[i][i]) * G;
//                     for( k = i; k < M; ++k ) {
//                         A[k][j] = A[k][j] + F * A[k][i];
//                     }
//                 }
//             }
//             for( j = i; j < M; ++j ) {
//                 A[j][i] = A[j][i] * G;
//             }
//         } else {
//             for( j = i; j < M; ++j ) {
//                 A[j][i] = 0.0;
//             }
//         }
//         A[i][i] = A[i][i] + 1.0;
//     }
// 
//     /* Diagonalization of the bidiagonal form.
//        Loop over singular values. */
//     for( k = (N-1); k >= 0; --k ) {
//         /* Loop over allowed iterations. */
//         for( its = 1; its <= 30; ++its ) {
//             /* Test for splitting.
//                Note that rv1[0] is always zero. */
//             flag = true;
//             for( l = k; l >= 0; --l ) {
//                 NM = l - 1;
//                 if( (fabs(rv1[l]) + ANorm) == ANorm ) {
//                     flag = false;
//                     break;
//                 } else if( (fabs(W[NM]) + ANorm) == ANorm ) {
//                     break;
//                 }
//             }
// 
//             /* Cancellation of rv1[l], if l > 0; */
//             if( flag ) {
//                 C = 0.0;
//                 S = 1.0;
//                 for( i = l; i <= k; ++i ) {
//                     F = S * rv1[i];
//                     if( (fabs(F) + ANorm) != ANorm ) {
//                         G = W[i];
//                         H = sqrt( F * F + G * G );
//                         W[i] = H;
//                         H = 1.0 / H;
//                         C = ( G * H );
//                         S = -( F * H );
//                         for( j = 0; j < M; ++j ) {
//                             Y = A[j][NM];
//                             Z = A[j][i];
//                             A[j][NM] = (Y * C) + (Z * S);
//                             A[j][i] = -(Y * S) + (Z * C);
//                         }
//                     }
//                 }
//             }
//             Z = W[k];
//             /* Convergence. */
//             if( l == k ) {
//                 /* Singular value is made nonnegative. */
//                 if( Z < 0.0 ) {
//                     W[k] = -Z;
//                     for( j = 0; j < N; ++j ) {
//                         V[j][k] = -V[j][k];
//                     }
//                 }
//                 break;
//             }
// 
//             if( its >= 30 ) {
//                 fprintf( stderr, "No convergence in 30 iterations.\n" );
//                 return;
//             }
// 
//             X = W[l];
//             NM = k - 1;
//             Y = W[NM];
//             G = rv1[NM];
//             H = rv1[k];
//             F = ((Y-Z)*(Y+Z) + (G-H)*(G+H)) / (2.0*H*Y);
//             G = sqrt( F * F + 1.0 );
//             tmp = G;
//             if( F < 0.0 )
//                 tmp = -tmp;
//             F = ((X-Z)*(X+Z) + H*((Y/(F+tmp))-H)) / X;
// 
//             /* Next QR transformation. */
//             C = 1.0;
//             S = 1.0;
//             for( j = l; j <= NM; ++j ) {
//                 i = j + 1;
//                 G = rv1[i];
//                 Y = W[i];
//                 H = S * G;
//                 G = C * G;
//                 Z = sqrt( F * F + H * H );
//                 rv1[j] = Z;
//                 C = F / Z;
//                 S = H / Z;
//                 F = (X * C) + (G * S);
//                 G = -(X * S) + (G * C);
//                 H = Y * S;
//                 Y = Y * C;
//                 for( jj = 0; jj < N; ++jj ) {
//                     X = V[jj][j];
//                     Z = V[jj][i];
//                     V[jj][j] = (X * C) + (Z * S);
//                     V[jj][i] = -(X * S) + (Z * C);
//                 }
//                 Z = sqrt( F * F + H * H );
//                 W[j] = Z;
// 
//                 /* Rotation can be arbitrary if Z = 0. */
//                 if( Z != 0.0 ) {
//                     Z = 1.0 / Z;
//                     C = F * Z;
//                     S = H * Z;
//                 }
//                 F = (C * G) + (S * Y);
//                 X = -(S * G) + (C * Y);
//                 for( jj = 0; jj < M; ++jj ) {
//                     Y = A[jj][j];
//                     Z = A[jj][i];
//                     A[jj][j] = (Y * C) + (Z * S);
//                     A[jj][i] = -(Y * S) + (Z * C);
//                 }
//             }
//             rv1[l] = 0.0;
//             rv1[k] = F;
//             W[k] = X;
//         }
//     }
// 
//     return;
// }
