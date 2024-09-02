
// #pragma GCC optimize ("O0")

#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include "acceleration.hpp"
#include "coordinates.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "planets.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "sofa.h"

HfOceanEop hfEop;

const GTime time2010	= GEpoch{2010, E_Month::JAN, 1,		0,	0,	0};

void IERS2010::PMGravi(
	GTime	time, 		///< Time
	double	ut1_utc,	///< Input ut1_utc value
	double&	x,			///< Polar motion in the x direction (micro arc seconds)
	double&	y,			///< Polar mothin in the y direction (micro arc seconds)
	double&	ut1,		///< ut1 variation (micro seconds)
	double&	lod)		///< lod variation (micro seconds)
{
	Eigen::Array<double,10,10> pmLibration; /** Tab5.1a IERS 2010, keeping only short periods */
	pmLibration <<

	1, -1, 0, -2,  0, -1,  -0.4,   0.3,   -0.3,   -0.4,
	1, -1, 0, -2,  0, -2,  -2.3,   1.3,   -1.3,   -2.3,
	1,  1, 0, -2, -2, -2,  -0.4,   0.3,   -0.3,   -0.4,
	1,  0, 0, -2,  0, -1,  -2.1,   1.2,   -1.2,   -2.1,
	1,  0, 0, -2,  0, -2, -11.4,   6.5,   -6.5,  -11.4,
	1, -1, 0,  0,  0,  0,   0.8,  -0.5,    0.5,    0.8,
	1,  0, 0, -2,  2, -2,  -4.8,   2.7,   -2.7,   -4.8,
	1,  0, 0,  0,  0,  0,  14.3,  -8.2,    8.2,   14.3,
	1,  0, 0,  0,  0, -1,   1.9,  -1.1,    1.1,    1.9,
	1,  1, 0,  0,  0,  0,   0.8,  -0.4,    0.4,    0.8;

	Eigen::Array<double,11,10> utLibration; /** Tab5.1b IERS 2010 */
	utLibration <<
		2,	-2,	0,	-2,	 0,	-2,  0.05, -0.03, -0.3, -0.6,
		2,	 0,	0,	-2,	-2,	-2,  0.06, -0.03, -0.4, -0.7,
		2,	-1,	0,	-2,	 0, -2,  0.35, -0.20, -2.4, -4.2,
		2,	1,	0,	-2, -2, -2,	0.07, -0.04, -0.5, -0.8,
		2,	0,	 0, -2, 0, -1,	-0.07, 0.04, 0.5, 0.8,
		2,	0,	0, 	-2, 0, -2,  1.75, -1.01, -12.2, -21.3,
		2,	1,	0, 	-2, 0, -2,  -0.05, 0.03, 0.3,	0.6,
		2,	0,	-1, -2, 2, -2,  0.05, -0.03, -0.3, -0.6,
		2,	0,	0,	-2, 2, -2,  0.76, -0.44, -5.5, -9.5,
		2,	0,	0,	 0, 0,  0,  0.21, -0.12, -1.5, -2.6,
		2,	0,	 0,	0,  0, -1,  0.06, -0.04, -0.4, -0.8;


	FundamentalArgs fundArgs(time, ut1_utc);

	x = 0;
	y = 0;
	ut1 = 0;
	lod = 0;
	for (auto pnlib : pmLibration.rowwise())
	{
		double arg = (pnlib.segment(0,6) * fundArgs).sum();

		x += sin(arg) * pnlib(6) + cos(arg)* pnlib(7);
		y += sin(arg) * pnlib(8) + cos(arg)* pnlib(9);
	}

	for (auto utlib : utLibration.rowwise())
	{
		double arg =  (utlib.segment(0,6) * fundArgs).sum() ;
		ut1 += sin(arg) * utlib(6) + cos(arg)* utlib(7);
		lod += sin(arg) * utlib(8) + cos(arg)* utlib(9);

	}
}

void IERS2010::PMUTOcean(
	GTime	time,
	double	ut1_utc,
	double&	x,
	double&	y,
	double&	ut1)
{
	Eigen::Array<double,71,12> data;
	data <<
	1,-1, 0,-2,-2,-2,  -0.05,   0.94,  -0.94,  -0.05,  0.396, -0.078,
	1,-2, 0,-2, 0,-1,   0.06,   0.64,  -0.64,   0.06,  0.195, -0.059,
	1,-2, 0,-2, 0,-2,   0.30,   3.42,  -3.42,   0.30,  1.034, -0.314,
	1, 0, 0,-2,-2,-1,   0.08,   0.78,  -0.78,   0.08,  0.224, -0.073,
	1, 0, 0,-2,-2,-2,   0.46,   4.15,  -4.15,   0.45,  1.187, -0.387,
	1,-1, 0,-2, 0,-1,   1.19,   4.96,  -4.96,   1.19,  0.966, -0.474,
	1,-1, 0,-2, 0,-2,   6.24,  26.31, -26.31,   6.23,  5.118, -2.499,
	1, 1, 0,-2,-2,-1,   0.24,   0.94,  -0.94,   0.24,  0.172, -0.090,
	1, 1, 0,-2,-2,-2,   1.28,   4.99,  -4.99,   1.28,  0.911, -0.475,
	1, 0, 0,-2, 0, 0,  -0.28,  -0.77,   0.77,  -0.28, -0.093,  0.070,
	1, 0, 0,-2, 0,-1,   9.22,  25.06, -25.06,   9.22,  3.025, -2.280,
	1, 0, 0,-2, 0,-2,  48.82, 132.91,-132.90,  48.82, 16.020,-12.069,
	1,-2, 0, 0, 0, 0,  -0.32,  -0.86,   0.86,  -0.32, -0.103,  0.078,
	1, 0, 0, 0,-2, 0,  -0.66,  -1.72,   1.72,  -0.66, -0.194,  0.154,
	1,-1, 0,-2, 2,-2,  -0.42,  -0.92,   0.92,  -0.42, -0.083,  0.074,
	1, 1, 0,-2, 0,-1,  -0.30,  -0.64,   0.64,  -0.30, -0.057,  0.050,
	1, 1, 0,-2, 0,-2,  -1.61,  -3.46,   3.46,  -1.61, -0.308,  0.271,
	1,-1, 0, 0, 0, 0,  -4.48,  -9.61,   9.61,  -4.48, -0.856,  0.751,
	1,-1, 0, 0, 0,-1,  -0.90,  -1.93,   1.93,  -0.90, -0.172,  0.151,
	1, 1, 0, 0,-2, 0,  -0.86,  -1.81,   1.81,  -0.86, -0.161,  0.137,
	1, 0,-1,-2, 2,-2,   1.54,   3.03,  -3.03,   1.54,  0.315, -0.189,
	1, 0, 0,-2, 2,-1,  -0.29,  -0.58,   0.58,  -0.29, -0.062,  0.035,
	1, 0, 0,-2, 2,-2,  26.13,  51.25, -51.25,  26.13,  5.512, -3.095,
	1, 0, 1,-2, 2,-2,  -0.22,  -0.42,   0.42,  -0.22, -0.047,  0.025,
	1, 0,-1, 0, 0, 0,  -0.61,  -1.20,   1.20,  -0.61, -0.134,  0.070,
	1, 0, 0, 0, 0, 1,   1.54,   3.00,  -3.00,   1.54,  0.348, -0.171,
	1, 0, 0, 0, 0, 0, -77.48,-151.74, 151.74, -77.48,-17.620,  8.548,
	1, 0, 0, 0, 0,-1, -10.52, -20.56,  20.56, -10.52, -2.392,  1.159,
	1, 0, 0, 0, 0,-2,   0.23,   0.44,  -0.44,   0.23,  0.052, -0.025,
	1, 0, 1, 0, 0, 0,  -0.61,  -1.19,   1.19,  -0.61, -0.144,  0.065,
	1, 0, 0, 2,-2, 2,  -1.09,  -2.11,   2.11,  -1.09, -0.267,  0.111,
	1,-1, 0, 0, 2, 0,  -0.69,  -1.43,   1.43,  -0.69, -0.288,  0.043,
	1, 1, 0, 0, 0, 0,  -3.46,  -7.28,   7.28,  -3.46, -1.610,  0.187,
	1, 1, 0, 0, 0,-1,  -0.69,  -1.44,   1.44,  -0.69, -0.320,  0.037,
	1, 0, 0, 0, 2, 0,  -0.37,  -1.06,   1.06,  -0.37, -0.407, -0.005,
	1, 2, 0, 0, 0, 0,  -0.17,  -0.51,   0.51,  -0.17, -0.213, -0.005,
	1, 0, 0, 2, 0, 2,  -1.10,  -3.42,   3.42,  -1.09, -1.436, -0.037,
	1, 0, 0, 2, 0, 1,  -0.70,  -2.19,   2.19,  -0.70, -0.921, -0.023,
	1, 0, 0, 2, 0, 0,  -0.15,  -0.46,   0.46,  -0.15, -0.193, -0.005,
	1, 1, 0, 2, 0, 2,  -0.03,  -0.59,   0.59,  -0.03, -0.396, -0.024,
	1, 1, 0, 2, 0, 1,  -0.02,  -0.38,   0.38,  -0.02, -0.253, -0.015,
	2,-3, 0,-2, 0,-2,  -0.49,  -0.04,   0.63,   0.24, -0.089, -0.011,
	2,-1, 0,-2,-2,-2,  -1.33,  -0.17,   1.53,   0.68, -0.224, -0.032,
	2,-2, 0,-2, 0,-2,  -6.08,  -1.61,   3.13,   3.35, -0.637, -0.177,
	2, 0, 0,-2,-2,-2,  -7.59,  -2.05,   3.44,   4.23, -0.745, -0.222,
	2, 0, 1,-2,-2,-2,  -0.52,  -0.14,   0.22,   0.29, -0.049, -0.015,
	2,-1,-1,-2, 0,-2,   0.47,   0.11,  -0.10,  -0.27,  0.033,  0.013,
	2,-1, 0,-2, 0,-1,   2.12,   0.49,  -0.41,  -1.23,  0.141,  0.058,
	2,-1, 0,-2, 0,-2, -56.87, -12.93,  11.15,  32.88, -3.795, -1.556,
	2,-1, 1,-2, 0,-2,  -0.54,  -0.12,   0.10,   0.31, -0.035, -0.015,
	2, 1, 0,-2,-2,-2, -11.01,  -2.40,   1.89,   6.41, -0.698, -0.298,
	2, 1, 1,-2,-2,-2,  -0.51,  -0.11,   0.08,   0.30, -0.032, -0.014,
	2,-2, 0,-2, 2,-2,   0.98,   0.11,  -0.11,  -0.58,  0.050,  0.022,
	2, 0,-1,-2, 0,-2,   1.13,   0.11,  -0.13,  -0.67,  0.056,  0.025,
	2, 0, 0,-2, 0,-1,  12.32,   1.00,  -1.41,  -7.31,  0.605,  0.266,
	2, 0, 0,-2, 0,-2,-330.15, -26.96,  37.58, 195.92,-16.195, -7.140,
	2, 0, 1,-2, 0,-2,  -1.01,  -0.07,   0.11,   0.60, -0.049, -0.021,
	2,-1, 0,-2, 2,-2,   2.47,  -0.28,  -0.44,  -1.48,  0.111,  0.034,
	2, 1, 0,-2, 0,-2,   9.40,  -1.44,  -1.88,  -5.65,  0.425,  0.117,
	2,-1, 0, 0, 0, 0,  -2.35,   0.37,   0.47,   1.41, -0.106, -0.029,
	2,-1, 0, 0, 0,-1,  -1.04,   0.17,   0.21,   0.62, -0.047, -0.013,
	2, 0,-1,-2, 2,-2,  -8.51,   3.50,   3.29,   5.11, -0.437, -0.019,
	2, 0, 0,-2, 2,-2,-144.13,  63.56,  59.23,  86.56, -7.547, -0.159,
	2, 0, 1,-2, 2,-2,   1.19,  -0.56,  -0.52,  -0.72,  0.064,  0.000,
	2, 0, 0, 0, 0, 1,   0.49,  -0.25,  -0.23,  -0.29,  0.027, -0.001,
	2, 0, 0, 0, 0, 0, -38.48,  19.14,  17.72,  23.11, -2.104,  0.041,
	2, 0, 0, 0, 0,-1, -11.44,   5.75,   5.32,   6.87, -0.627,  0.015,
	2, 0, 0, 0, 0,-2,  -1.24,   0.63,   0.58,   0.75, -0.068,  0.002,
	2, 1, 0, 0, 0, 0,  -1.77,   1.79,   1.71,   1.04, -0.146,  0.037,
	2, 1, 0, 0, 0,-1,  -0.77,   0.78,   0.75,   0.45, -0.064,  0.017,
	2, 0, 0, 2, 0, 2,  -0.33,   0.62,   0.65,   0.19, -0.049,  0.018;

	x	= 0;
	y	= 0;
	ut1	= 0;

	FundamentalArgs fundArgs(time, ut1_utc);
	for (auto pnlib : data.rowwise())
	{
		double arg =  (pnlib.segment(0, 6) * fundArgs).sum();
		x	+= sin(arg) * pnlib(6)	+ cos(arg) * pnlib(7);
		y	+= sin(arg) * pnlib(8)	+ cos(arg) * pnlib(9);
		ut1 += sin(arg) * pnlib(10)	+ cos(arg) * pnlib(11);
	}
}


FundamentalArgs::FundamentalArgs(
	GTime	time,
	double	ut1_utc)
:	gmst	{(*this)[0]},
	l		{(*this)[1]},
	l_prime	{(*this)[2]},
	f		{(*this)[3]},
	d		{(*this)[4]},
	omega	{(*this)[5]}
{
	(*this)[0] = Sofa::iauGmst	(MjDateUt1(time, ut1_utc), time) + PI;
	(*this)[1] = Sofa::iauFal	(time);
	(*this)[2] = Sofa::iauFalp	(time);
	(*this)[3] = Sofa::iauFaf	(time);
	(*this)[4] = Sofa::iauFad	(time);
	(*this)[5] = Sofa::iauFaom	(time);
}


Array6d IERS2010::doodson(
	GTime	time,
	double	ut1_utc)
{
	FundamentalArgs fundArgs(time, ut1_utc);

	Array6d Doodson;
	Doodson(4) = -1 * fundArgs(5);	//todo aaron, change to use named parameters, remove setBeta function
	Doodson(1) = fundArgs(3) + fundArgs(5);
	Doodson(0) = fundArgs(0) - Doodson(1);
	Doodson(2) = Doodson(1) - fundArgs(4);
	Doodson(3) = Doodson(1) - fundArgs(1);
	Doodson(5) = Doodson(1) - fundArgs(4) - fundArgs(2);

	return Doodson;
}

/** Implementation of the first part of the solidEarth tides (Eq 6.6 and 6.7 IERS2010)
*/
void IERS2010::solidEarthTide1(
	const Vector3d& ITRFSun,	///< Position of the Sun in ITRF / ECEF
	const Vector3d& ITRFMoon,	///< Position of the Moon in ITRF/ ECEF
	MatrixXd& Cnm,				///< Modification of the C coefficient
	MatrixXd& Snm)				///< Modification of the S coefficient
{
	Matrix <double, 5, 5> elasticLove;

	elasticLove <<  0,			0,			0,			0,			0,
					0,			0,			0,			0,			0,
					0.29525,	0.29470,	0.29801,	0,			0,
					0.093,		0.093,		0.093,		0.094,		0,
					-0.00087,	-0.00079,	-0.00057,	0,			0;

	double GMe = GM_values[E_ThirdBody::EARTH];

	for (auto body: {E_ThirdBody::SUN, E_ThirdBody::MOON})
	{
		Vector3d	position	= Vector3d::Zero();
		double		GM			= 0;
		if		(body == E_ThirdBody::SUN)		{		position = ITRFSun;			GM = GM_values[body];		}
		else if (body == E_ThirdBody::MOON)		{		position = ITRFMoon;		GM = GM_values[body];		}

		// BOOST_LOG_TRIVIAL(debug) << position << " " << GM ;
		Legendre leg(3);
		leg.calculate(position.z()/position.norm());
		double phi = atan2(position.y(), position.x());

		// Do deg 2 and 3
		for (int ideg = 2; ideg <= 3;		ideg++)
		for (int iord = 0; iord <= ideg;	iord++)
		{
			// BOOST_LOG_TRIVIAL(debug) << elasticLove(ideg,iord) << ideg << " " <<iord << "\n";
			double cst = elasticLove(ideg,iord) / (2*(double)ideg +1);
			Cnm(ideg, iord) += cst * GM/GMe * pow(RE_WGS84/position.norm(), ideg+1) * leg.Pnm(ideg, iord) * cos(iord * phi);
			Snm(ideg, iord) += cst * GM/GMe * pow(RE_WGS84/position.norm(), ideg+1) * leg.Pnm(ideg, iord) * sin(iord * phi);
		}

		// Special case deg4
		for (int iord = 0; iord <=2; iord ++)
		{
			double cst = elasticLove(4,iord) / 5;
			Cnm(4, iord) += cst * GM/GMe * pow(RE_WGS84/position.norm(), 3) * leg.Pnm(2, iord) * cos(iord * phi);
			Snm(4, iord) += cst * GM/GMe * pow(RE_WGS84/position.norm(), 3) * leg.Pnm(2, iord) * sin(iord * phi);
		}
	}
}


void IERS2010::solidEarthTide2(
	GTime		time,			///< Time
	double		ut1_utc,		///< Input ut1_utc value
	MatrixXd&	Cnm,			///< Effect of solidEarth Tides on C coefficient
	MatrixXd&	Snm)			///< Effect of solidEarth Tides on S coefficient
{
	Eigen::Array< double, 48, 8> tab65a;
	Eigen::Array< double, 21, 8> tab65b;
	Eigen::Array< double,  2, 7> tab65c;
	tab65a <<
		1.0e0, -3.0e0,  0.0e0,  2.0e0,  0.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0, -3.0e0,  2.0e0,  0.0e0,  0.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0, -2.0e0,  0.0e0,  1.0e0, -1.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0, -2.0e0,  0.0e0,  1.0e0,  0.0e0,  0.0e0,   -0.7e0,    0.1e0,
		1.0e0, -2.0e0,  2.0e0, -1.0e0,  0.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0, -1.0e0,  0.0e0,  0.0e0, -1.0e0,  0.0e0,   -1.3e0,    0.1e0,
		1.0e0, -1.0e0,  0.0e0,  0.0e0,  0.0e0,  0.0e0,   -6.8e0,    0.6e0,
		1.0e0, -1.0e0,  2.0e0,  0.0e0,  0.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  0.0e0, -2.0e0,  1.0e0,  0.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  0.0e0,  0.0e0, -1.0e0, -1.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  0.0e0,  0.0e0, -1.0e0,  0.0e0,  0.0e0,    0.4e0,    0.0e0,
		1.0e0,  0.0e0,  0.0e0,  1.0e0,  0.0e0,  0.0e0,    1.3e0,   -0.1e0,
		1.0e0,  0.0e0,  0.0e0,  1.0e0,  1.0e0,  0.0e0,    0.3e0,    0.0e0,
		1.0e0,  0.0e0,  2.0e0, -1.0e0,  0.0e0,  0.0e0,    0.3e0,    0.0e0,
		1.0e0,  0.0e0,  2.0e0, -1.0e0,  1.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  1.0e0, -3.0e0,  0.0e0,  0.0e0,  1.0e0,   -1.9e0,    0.1e0,
		1.0e0,  1.0e0, -2.0e0,  0.0e0, -1.0e0,  0.0e0,    0.5e0,    0.0e0,
		1.0e0,  1.0e0, -2.0e0,  0.0e0,  0.0e0,  0.0e0,  -43.4e0,    2.9e0,
		1.0e0,  1.0e0, -1.0e0,  0.0e0,  0.0e0, -1.0e0,    0.6e0,    0.0e0,
		1.0e0,  1.0e0, -1.0e0,  0.0e0,  0.0e0,  1.0e0,    1.6e0,   -0.1e0,
		1.0e0,  1.0e0,  0.0e0, -2.0e0, -1.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  1.0e0,  0.0e0,  0.0e0, -2.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  1.0e0,  0.0e0,  0.0e0, -1.0e0,  0.0e0,   -8.8e0,    0.5e0,
		1.0e0,  1.0e0,  0.0e0,  0.0e0,  0.0e0,  0.0e0,  470.9e0,  -30.2e0,
		1.0e0,  1.0e0,  0.0e0,  0.0e0,  1.0e0,  0.0e0,   68.1e0,   -4.6e0,
		1.0e0,  1.0e0,  0.0e0,  0.0e0,  2.0e0,  0.0e0,   -1.6e0,    0.1e0,
		1.0e0,  1.0e0,  1.0e0, -1.0e0,  0.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  1.0e0,  1.0e0,  0.0e0, -1.0e0, -1.0e0,   -0.1e0,    0.0e0,
		1.0e0,  1.0e0,  1.0e0,  0.0e0,  0.0e0, -1.0e0,  -20.6e0,   -0.3e0,
		1.0e0,  1.0e0,  1.0e0,  0.0e0,  0.0e0,  1.0e0,    0.3e0,    0.0e0,
		1.0e0,  1.0e0,  1.0e0,  0.0e0,  1.0e0, -1.0e0,   -0.3e0,    0.0e0,
		1.0e0,  1.0e0,  2.0e0, -2.0e0,  0.0e0,  0.0e0,   -0.2e0,    0.0e0,
		1.0e0,  1.0e0,  2.0e0, -2.0e0,  1.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0,  1.0e0,  2.0e0,  0.0e0,  0.0e0,  0.0e0,   -5.0e0,    0.3e0,
		1.0e0,  1.0e0,  2.0e0,  0.0e0,  1.0e0,  0.0e0,    0.2e0,    0.0e0,
		1.0e0,  1.0e0,  3.0e0,  0.0e0,  0.0e0, -1.0e0,   -0.2e0,    0.0e0,
		1.0e0,  2.0e0, -2.0e0,  1.0e0,  0.0e0,  0.0e0,   -0.5e0,    0.0e0,
		1.0e0,  2.0e0, -2.0e0,  1.0e0,  1.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0,  2.0e0,  0.0e0, -1.0e0, -1.0e0,  0.0e0,    0.1e0,    0.0e0,
		1.0e0,  2.0e0,  0.0e0, -1.0e0,  0.0e0,  0.0e0,   -2.1e0,    0.1e0,
		1.0e0,  2.0e0,  0.0e0, -1.0e0,  1.0e0,  0.0e0,   -0.4e0,    0.0e0,
		1.0e0,  3.0e0, -2.0e0,  0.0e0,  0.0e0,  0.0e0,   -0.2e0,    0.0e0,
		1.0e0,  3.0e0,  0.0e0, -2.0e0,  0.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0,  3.0e0,  0.0e0,  0.0e0,  0.0e0,  0.0e0,   -0.6e0,    0.0e0,
		1.0e0,  3.0e0,  0.0e0,  0.0e0,  1.0e0,  0.0e0,   -0.4e0,    0.0e0,
		1.0e0,  3.0e0,  0.0e0,  0.0e0,  2.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0,  4.0e0,  0.0e0, -1.0e0,  0.0e0,  0.0e0,   -0.1e0,    0.0e0,
		1.0e0,  4.0e0,  0.0e0, -1.0e0,  1.0e0,  0.0e0,   -0.1e0,    0.0e0;

	tab65b <<
		0.0e0,  0.0e0,  0.0e0,  0.0e0,  1.0e0,  0.0e0,  16.6e0,  -6.7e0,
		0.0e0,  0.0e0,  0.0e0,  0.0e0,  2.0e0,  0.0e0,  -0.1e0,   0.1e0,
		0.0e0,  0.0e0,  1.0e0,  0.0e0,  0.0e0, -1.0e0,  -1.2e0,   0.8e0,
		0.0e0,  0.0e0,  2.0e0,  0.0e0,  0.0e0,  0.0e0,  -5.5e0,   4.3e0,
		0.0e0,  0.0e0,  2.0e0,  0.0e0,  1.0e0,  0.0e0,   0.1e0,  -0.1e0,
		0.0e0,  0.0e0,  3.0e0,  0.0e0,  0.0e0, -1.0e0,  -0.3e0,   0.2e0,
		0.0e0,  1.0e0, -2.0e0,  1.0e0,  0.0e0,  0.0e0,  -0.3e0,   0.7e0,
		0.0e0,  1.0e0,  0.0e0, -1.0e0, -1.0e0,  0.0e0,   0.1e0,  -0.2e0,
		0.0e0,  1.0e0,  0.0e0, -1.0e0,  0.0e0,  0.0e0,  -1.2e0,   3.7e0,
		0.0e0,  1.0e0,  0.0e0, -1.0e0,  1.0e0,  0.0e0,   0.1e0,  -0.2e0,
		0.0e0,  1.0e0,  0.0e0,  1.0e0,  0.0e0,  0.0e0,   0.1e0,  -0.2e0,
		0.0e0,  2.0e0, -2.0e0,  0.0e0,  0.0e0,  0.0e0,   0.0e0,   0.6e0,
		0.0e0,  2.0e0,  0.0e0, -2.0e0,  0.0e0,  0.0e0,   0.0e0,   0.3e0,
		0.0e0,  2.0e0,  0.0e0,  0.0e0,  0.0e0,  0.0e0,   0.6e0,   6.3e0,
		0.0e0,  2.0e0,  0.0e0,  0.0e0,  1.0e0,  0.0e0,   0.2e0,   2.6e0,
		0.0e0,  2.0e0,  0.0e0,  0.0e0,  2.0e0,  0.0e0,   0.0e0,   0.2e0,
		0.0e0,  3.0e0, -2.0e0,  1.0e0,  0.0e0,  0.0e0,   0.1e0,   0.2e0,
		0.0e0,  3.0e0,  0.0e0, -1.0e0,  0.0e0,  0.0e0,   0.4e0,   1.1e0,
		0.0e0,  3.0e0,  0.0e0, -1.0e0,  1.0e0,  0.0e0,   0.2e0,   0.5e0,
		0.0e0,  4.0e0, -2.0e0,  0.0e0,  0.0e0,  0.0e0,   0.1e0,   0.2e0,
		0.0e0,  4.0e0,  0.0e0, -2.0e0,  0.0e0,  0.0e0,   0.1e0,   0.1e0;

	tab65c <<
		2.0e0, -1.0e0,  0.0e0,  1.0e0,  0.0e0,  0.0e0,   -0.3e0,
		2.0e0,  0.0e0,  0.0e0,  0.0e0,  0.0e0,  0.0e0,   -1.2e0;

	auto dood_arr = IERS2010::doodson(time, ut1_utc);

	/**
	* Effect on C20
	*/
	for (auto line : tab65b.rowwise())
	{
		double thetaf = (line.segment(0, 6) * dood_arr).sum();
		Cnm(2, 0) += line(6) * 1e-12 * cos(thetaf) - line(7) * 1e-12 * sin(thetaf);
	}

	/**
	*  Effect on C21/S21
	*/
	for (auto line : tab65a.rowwise())
	{
		double thetaf = (line.segment(0, 6) * dood_arr).sum();
		Cnm(2, 1) += line(6) * 1e-12 * sin(thetaf) + line(7) * 1e-12 * cos(thetaf);
		Snm(2, 1) += line(6) * 1e-12 * cos(thetaf) - line(7) * 1e-12 * sin(thetaf);
	}

	/**
	*  Effect on C22/S22
	*/
	for (auto line : tab65c.rowwise())
	{
		double thetaf = (line.segment(0, 6) * dood_arr).sum();
		Cnm(2, 2) +=      line(6) * 1e-12 * cos(thetaf);
		Snm(2, 2) += -1 * line(6) * 1e-12 * sin(thetaf);
	}
}


void IERS2010::poleSolidEarthTide(
	MjDateTT		mjd,
	const double	xp,
	const double	yp,
	MatrixXd&		Cnm,
	MatrixXd&		Snm)
{
	double xpv;
	double ypv;
	meanPole(mjd, xpv, ypv);

	double m1 = +(xp / AS2R - xpv / 1000);
	double m2 = -(yp / AS2R - ypv / 1000);
	Cnm(2, 1) += -1.333e-9 * (m1 + 0.0115 * m2);
	Snm(2, 1) += -1.333e-9 * (m2 - 0.0115 * m1);
}


void IERS2010::poleOceanTide(
	MjDateTT		mjd,
	const double	xp,
	const double	yp,
	MatrixXd&		Cnm,
	MatrixXd&		Snm)
{
	double xpv;
	double ypv;
	meanPole(mjd, xpv, ypv);

	double m1 = +(xp / AS2R - xpv / 1000);
	double m2 = -(yp / AS2R - ypv / 1000);
	Cnm(2, 1) += -2.1778e-10 * (m1 - 0.01724 * m2);
	Snm(2, 1) += -1.7232e-10 * (m2 - 0.03365 * m1);
}


void IERS2010::meanPole(
	const MjDateTT&	mjd,
	double&			xpv,
	double&			ypv)
{
	double t = mjd.to_j2000() / 365.25;
	xpv = 55.0	+ 1.677 * t;
	ypv = 320.5	+ 3.460 * t;
}


Vector3d IERS2010::relativity(
	const	Vector3d&	posSat,
	const	Vector3d&	velSat,
	const	Vector3d&	posSun,
	const	Vector3d&	velSun,
	const	Matrix3d&	U,
	const	Matrix3d&	dU)
{
	double GMe = GM_values[E_ThirdBody::EARTH];
	double GMs = GM_values[E_ThirdBody::SUN];

	// required pos and vel of Earth wrt sun, we have sun wrt earth => so mult by -1
	Vector3d posEarth = -posSun;
	Vector3d velEarth = -velSun;

	double beta  = 1;
	double gamma = 1;

	double rsat = posSat.norm();
	double rsun = posEarth.norm();

	Matrix3d S = dU.transpose() * U;

	Vector3d anglevel;
	anglevel(0) = S(2, 1);
	anglevel(1) = S(0, 2);
	anglevel(2) = S(1, 0);

	Vector3d J = anglevel * 8.0365e37 / 5.9736e24;

	// 1st term (Schwarzchild)
	Vector3d acc1 = GMe / (SQR(CLIGHT)  * pow(rsat, 3)) * ((2 * (beta + gamma) * GMe / rsat - gamma * velSat.dot(velSat)) * posSat +	2 * (1 + gamma) * posSat.dot(velSat) * velSat);

	// 2nt term (Lense-Thirring)
	Vector3d acc2 = (1 + gamma) * GMe / (SQR(CLIGHT) * pow(rsat, 3)) *	(	3 / SQR(rsat) * posSat.cross(velSat) * posSat.dot(J) + velSat.cross(J)	) ;

	//3rd (de Sitter .aka. geodesic preciession)
	Vector3d acc3 = (1 + 2 * gamma) * (velEarth.cross((-1 * GMs * posEarth) / (SQR(CLIGHT) * pow(rsun, 3)))).cross(velSat);

	return acc1 + acc2 + acc3;
}


void HfOceanEop::read(
	const string& filename)
{
	std::ifstream file(filename);

	if (!file)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "HF Ocean eop file open error " << filename;

		return;
	}

	string line;

	while (std::getline(file, line))
	{
		if (line[0] == '#')
		{
			continue;
		}

		std::istringstream iss(line);
		HfOceanEOPData data;
		iss >> data.name;
		for (int i = 0; i < 6; i++)
		{
			iss >> data.mFundamentalArgs[i];
		}

		iss >> data.doodson;
		iss >> data.period;
		iss >> data.xSin;
		iss >> data.xCos;
		iss >> data.ySin;
		iss >> data.yCos;
		iss >> data.ut1Sin;
		iss >> data.ut1Cos;
		iss >> data.lodSin;
		iss >> data.lodCos;

		hfOceanDataVec.push_back(data);
	}

	initialized = true;
}

void HfOceanEop::compute(
	Array6d&	fundamentalArgs,
	double&		x,
	double&		y,
	double&		ut1,
	double&		lod)
{
	x	= 0;
	y	= 0;
	ut1	= 0;
	lod	= 0;

	for (auto& hfdata : hfOceanDataVec)
	{
		double theta = (fundamentalArgs * hfdata.mFundamentalArgs).sum();

		x		+= hfdata.xCos		* cos(theta) + hfdata.xSin		* sin(theta);
		y		+= hfdata.yCos		* cos(theta) + hfdata.ySin		* sin(theta);
		ut1		+= hfdata.ut1Cos	* cos(theta) + hfdata.ut1Sin	* sin(theta);
		lod		+= hfdata.lodCos	* cos(theta) + hfdata.lodSin	* sin(theta);
	}
}
