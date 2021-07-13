/*!
 * Definition of the boost multi array types.
 *
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */

#ifndef PEA_BOOST_MA_TYPE_H
#define PEA_BOOST_MA_TYPE_H

// #include <complex>


typedef boost::multi_array<double, 3> MA3d;
typedef boost::multi_array<double, 2> MA2d;
typedef boost::multi_array<double, 1> MA1d;

typedef boost::multi_array<float, 3> MA3f;
typedef boost::multi_array<float, 2> MA2f;
typedef boost::multi_array<float, 1> MA1f;

typedef boost::multi_array< std::complex<float>, 3> MA3cf;
typedef boost::multi_array< std::complex<float>, 2> MA2cf;
typedef boost::multi_array< std::complex<float>, 1> MA1cf;

#endif //PEA_BOOST_MA_TYPE_H
