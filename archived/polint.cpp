// /* Slightly modified versions of the "polint" routine from
//  * Press, William H., Brian P. Flannery, Saul A. Teukolsky and
//  *   William T. Vetterling, 1986, "Numerical Recipes: The Art of
//  *   Scientific Computing" (Fortran), Cambrigde University Press,
//  *   pp. 80-82.
//  */
// 
// #include <stdio.h>
// #include <malloc.h>
// #include <math.h>
// 
// void polint( double *xa, double *ya, int n, double x, double *y, double *dy );
// 
// void polint( double *xa, double *ya, int n, double x, double *y, double *dy )
// {
//     double *c = NULL;
//     double *d = NULL;
//     double den;
//     double dif;
//     double dift;
//     double ho;
//     double hp;
//     double w;
// 
//     int i;
//     int m;
//     int ns;
// 
//     if( (c = (double *)malloc( n * sizeof( double ) )) == NULL ||
//         (d = (double *)malloc( n * sizeof( double ) )) == NULL ) {
//         fprintf( stderr, "polint error: allocating workspace\n" );
//         fprintf( stderr, "polint error: setting y = 0 and dy = 1e9\n" );
//         *y = 0.0;
//         *dy = 1.e9;
// 
//         if( c != NULL )
//             free( c );
//         if( d != NULL )
//             free( d );
//         return;
//     }
// 
//     ns = 0;
//     dif = fabs(x-xa[0]);
//     for( i = 0; i < n; ++i ) {
//         dift = fabs( x-xa[i] );
//         if( dift < dif ) {
//             ns = i;
//             dif = dift;
//         }
//         c[i] = ya[i];
//         d[i] = ya[i];
//     }
//     *y = ya[ns];
//     ns = ns-1;
//     for( m = 0; m < n-1; ++m ) {
//         for( i = 0; i < n-m-1; ++i ) {
//             ho = xa[i]-x;
//             hp = xa[i+m+1]-x;
//             w = c[i+1]-d[i];
//             den = ho-hp;
//             if( den == 0 ) {
//                 fprintf( stderr, "polint error: den = 0\n" );
//                 fprintf( stderr, "polint error: setting y = 0 and dy = 1e9\n" );
//                 *y = 0.0;
//                 *dy = 1.e9;
// 
//                 if( c != NULL )
//                     free( c );
//                 if( d != NULL )
//                     free( d );
//                 return;
//             }
//             den = w/den;
//             d[i] = hp*den;
//             c[i] = ho*den;
//         }
//         if( 2*(ns+1) < n-m-1 ) {
//             *dy = c[ns+1];
//         } else {
//             *dy = d[ns];
//             ns = ns-1;
//         }
//         *y = (*y)+(*dy);
//     }
// 
// 
//     if( c != NULL )
//         free( c );
//     if( d != NULL )
//         free( d );
//     return;
// }
