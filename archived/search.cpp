// /* Slightly modified version of the "hunt" routine from
//  *
//  * Press, William H., Brian P. Flannery, Saul A Teukolsky and
//  *   William T. Vetterling, 1986, "Numerical Recipes: The Art of
//  *   Scientific Computing" (Fortran), Cambrigde University Press, pp. 91-92.
//  */
// 
// #include "atest.hpp"
// 
// int search( EpochDatum **data, int count_data, double x, int lower_index)
// {
//     int index;
//     int upper_index = count_data - 1;
//     int inc;
//     int test1 = (data[upper_index]->epoch > data[0]->epoch);
//     int test2;
// #   define true ((int)1)
// 
//     if( lower_index >= 0 && lower_index < count_data ) {
//         inc = 1;
//         test2 = (x >= data[lower_index]->epoch);
//         if( (test1 && test2) || ! (test1 || test2) ) {
//             while( true ) {
//                 upper_index = lower_index + inc;
//                 if( upper_index >= count_data ) {
//                     upper_index = count_data - 1;
//                     break;
//                 }
//                 test2 = (x >= data[upper_index]->epoch);
//                 if( (test1 && test2) || ! (test1 || test2) ) {
//                     lower_index = upper_index;
//                     inc = inc + inc;
//                 } else {
//                     break;
//                 }
//             }
//         } else {
//             upper_index = lower_index;
//             while( true ) {
//                 lower_index = upper_index - inc;
//                 if( lower_index < 0 ) {
//                     lower_index = -1;
//                     break;
//                 }
//                 test2 = (x < data[lower_index]->epoch);
//                 if( (test1 && test2) || ! (test1 || test2) ) {
//                     upper_index = lower_index;
//                     inc = inc + inc;
//                 } else {
//                     break;
//                 }
//             }
//         }
//                 
//     } else {
//         lower_index = 0;
//         upper_index = count_data - 1;
//     }
// 
//     while( (upper_index - lower_index) > 1 ) {
//         index = (upper_index + lower_index) / 2;
//         test2 = (x > data[index]->epoch);
//         if( (test1 && test2) || ! (test1 || test2) ) {
//             lower_index = index;
//         } else {
//             upper_index = index;
//         }
//     }
// 
//     return lower_index;
// }
