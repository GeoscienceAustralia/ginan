// #include <math.h>
// /*
//  *  Trigometric expansion basis functions.
//  */
// void TrigFunc( double x, double *afunc, unsigned int nfunc )
// {
//     double pi = 3.14159265358979;
//     double two_pi = 2.0 * pi;
// 
//     /*
//      *  Sidereal day taken from the Astronomical Almanac 1995 (USNO)
//      */
//     double sidereal_day = 0.99726956634;
// 
//     double period = sidereal_day / 2.0;
//     double P0 = two_pi / period;
// 
//     /*
//      *  Reference to a specific epoch to help minimize roundoff problems.
//      */
//     double t0 = x;
// 
//     double P;
//     int factor = 1;
//     unsigned int i = 0;
// 
//     afunc[i] = 1.0; ++i;
//     while( i < nfunc ) {
//         P = (double)factor * P0;
//         afunc[i] = sin( t0 * P ); ++i;
//         afunc[i] = cos( t0 * P ); ++i;
//         ++factor;
//     }
// 
//     return;
// }
