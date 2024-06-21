
// #pragma GCC optimize ("O0")

#include <stdarg.h>
#include <ctype.h>

#include <algorithm>
#include <string>

#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "enums.h"

void updateLamMap(
	const	GTime&	time,
			SatPos&	satPos)
{
	E_Sys sys = satPos.Sat.sys;
	if (satPos.satNav_ptr == nullptr)
	{
		return;
	}

	auto& lamMap = satPos.satNav_ptr->lamMap;

	if (sys == +E_Sys::GLO)
	{
		auto* eph_ptr = seleph<Geph>(nullStream, time, satPos.Sat, acsConfig.used_nav_types[sys], ANY_IODE, nav);

		if (eph_ptr)
		{
			auto& geph = *static_cast<Geph*>(eph_ptr);

			lamMap[G1]	= CLIGHT / (FREQ1_GLO + DFRQ1_GLO * geph.frq);
			lamMap[G2]	= CLIGHT / (FREQ2_GLO + DFRQ2_GLO * geph.frq);
			lamMap[G3]	= CLIGHT / (FREQ3_GLO);
			lamMap[G4]	= CLIGHT / (FREQ4_GLO);
			lamMap[G6]	= CLIGHT / (FREQ6_GLO);
		}
	}
	else
	{
		lamMap[F1]	= CLIGHT / FREQ1; /* L1/E1/B1 */
		lamMap[F2]	= CLIGHT / FREQ2; /* L2 */
		lamMap[F5]	= CLIGHT / FREQ5; /* L5/E5a/B2a */
		lamMap[F6]	= CLIGHT / FREQ6; /* E6/L6 */
		lamMap[F7]	= CLIGHT / FREQ7; /* E5b/B2/B2b */
		lamMap[F8]	= CLIGHT / FREQ8; /* E5a+b/B2a+b */

		if	(sys == +E_Sys::BDS)
		{
			lamMap[B1]	= CLIGHT / FREQ1_CMP; /* B2-1 */
			lamMap[B3]	= CLIGHT / FREQ3_CMP; /* B3 */
		}
	}
}

/** geometric distance
* compute geometric distance and receiver-to-satellite unit vector
* notes  : distance includes sagnac effect correction
*/
double geodist(
	Vector3d& rs,	///< satellilte position (ecef at transmission) (m)
	Vector3d& rr,	///< receiver position (ecef at reception) (m)
	Vector3d& e)	///< line-of-sight vector (ecef)
{
	if (rs.norm() < RE_WGS84 * 0.9)
		return -1;

	e = rs - rr;
	double r = e.norm();
	e.normalize();
	return r + sagnac(rs, rr);
}

double sagnac(
	Vector3d&	rSource,	//inertial position of source
	Vector3d&	rDest,		//inertial position of destination
	Vector3d	vel)		//inertial velocity
{
	if (vel.isZero())
	{
		Vector3d omega = Vector3d::Zero();

		omega.z() = OMGE;

		vel = omega.cross(rDest);
	}

	//todo aaron, check which vel is required for slr things, still dest on outward journey?
	return (rDest - rSource).dot(vel) / CLIGHT;
}

/** satellite azimuth/elevation angle
 */
void satazel(
	const	VectorPos&	pos,	///< geodetic position
	const	VectorEcef&	e,		///< receiver-to-satellilte unit vector
			AzEl&		azel)	///< azimuth/elevation {az,el} (rad)
{
	azel.az = 0;
	azel.el = PI/2;

	//printf("pos %f %f %f e: %f enu: %f %f %f\n",pos[0],pos[1],pos[2],e,enu[0],enu[1],enu[2]);
	if (pos.hgt() > -RE_WGS84)
	{
		VectorEnu enu = ecef2enu(pos, e);

		azel.az = dot(enu.data(), enu.data(), 2) < 1E-12 ? 0.0 : atan2(enu.e(), enu.n());

		if (azel.az < 0)
			azel.az += 2 * PI;

		azel.el = asin(enu.u());
	}
}

/** compute DOP (dilution of precision)
*/
Dops dopCalc(
	const vector<AzEl>&	azels)		///< satellite azimuth/elevation angles
{
	vector<double> H;
	H.reserve(64);
	int n = 0;

	for (auto& azel : azels)
	{
		double cosel = cos(azel.el);
		double sinel = sin(azel.el);

		H.push_back(cosel * sin(azel.az));
		H.push_back(cosel * cos(azel.az));
		H.push_back(sinel);
		H.push_back(1);
		n++;
	}

	if (n < 4)
	{
		fprintf(stderr, "%s: Can not calculate the dops, less than 4 sats\n", __FUNCTION__);
		return Dops();
	}

	auto H_mat = MatrixXd::Map(H.data(), 4, n).transpose();
	auto Q_mat = H_mat.transpose() * H_mat;

	auto Q_inv = Q_mat.inverse();

	Dops dops;
	dops.gdop = SQRT(Q_inv(0,0) + Q_inv(1,1) +	Q_inv(2,2) + Q_inv(3,3)	);
	dops.pdop = SQRT(Q_inv(0,0) + Q_inv(1,1) +	Q_inv(2,2)				);
	dops.hdop = SQRT(Q_inv(0,0) + Q_inv(1,1)							);
	dops.vdop = SQRT(							Q_inv(2,2)				);

	return dops;
}

/** Low pass filter values
*/
void lowPassFilter(
	Average&	avg,
	double		meas,
	double		procNoise,
	double		measVar)
{
	if (avg.var == 0)
	{
		avg.mean	= meas;
		avg.var		= measVar;
		return;
	}

	avg.var += SQR(procNoise);

	double delta	= meas - avg.mean;
	double varFrac	= 1 / (measVar + avg.var);

	avg.mean += delta * varFrac;

	avg.var = 1 / (1 / measVar + 1 / avg.var);
}

