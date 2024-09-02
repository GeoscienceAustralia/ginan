
// #pragma GCC optimize ("O0")

/** \file
* ###References:
*
* 1.  D.D.McCarthy, IERS Technical Note 21, IERS Conventions 1996, July 1996
* 2.  D.D.McCarthy and G.Petit, IERS Technical Note 32, IERS Conventions 2003, November 2003
* 3.  D.A.Vallado, Fundamentals of Astrodynamics and Applications 2nd ed, Space Technology Library, 2004
* 4.  J.Kouba, A Guide to using International GNSS Service (IGS) products, May 2009
* 5.  RTCM Paper, April 12, 2010, Proposed SSR Messages for SV Orbit Clock, Code Biases, URA
* 6.  MacMillan et al., Atmospheric gradients and the VLBI terrestrial and celestial reference frames, Geophys. Res. Let., 1997
* 7.  G.Petit and B.Luzum (eds), IERS Technical Note No. 36, IERS Conventions (2010), 2010
* 8.  J.Kouba, A simplified yaw-attitude model for eclipsing GPS satellites, GPS Solutions, 13:1-12, 2009
* 9.  F.Dilssner, GPS IIF-1 satellite antenna phase center and attitude modeling, InsideGNSS, September, 2010
* 10. F.Dilssner, The GLONASS-M satellite yaw-attitude model, Advances in Space Research, 2010
* 11. IGS MGEX (http://igs.org/mgex)
*/


#include <boost/log/trivial.hpp>

#include <vector>

using std::vector;


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "receiver.hpp"
#include "planets.hpp"
#include "satStat.hpp"
#include "antenna.hpp"
#include "common.hpp"
#include "sinex.hpp"
#include "trace.hpp"
#include "enums.h"
#include "ppp.hpp"


/** exclude meas of eclipsing satellite (block IIA)
*/
void testEclipse(
	ObsList&	obsList)
{
	/* unit vector of sun direction (ecef) */
	VectorEcef rsun;
	planetPosEcef(obsList.front()->time, E_ThirdBody::SUN, rsun);

	Vector3d esun = rsun.normalized();

	for (auto& obs : only<GObs>(obsList))
	{
		if (acsConfig.reject_eclipse[obs.Sat.sys] == false)
			continue;

		if (obs.exclude)
		{
			continue;
		}

		double r = obs.rSatCom.norm();
		if (r <= 0)
			continue;

		/* only block IIA */
// 		if (obs.satNav_ptr->pcv.type == "BLOCK IIA")		//todo take from the satSys object
// 			continue;

		/* sun-earth-satellite angle */
		double cosa = obs.rSatCom.dot(esun) / r;

		if (cosa < -1)		cosa = -1;
		if (cosa > +1)		cosa = +1;

		double ang = acos(cosa);

		/* test eclipse */
		if	( ang < PI / 2
			|| r * sin(ang) > RE_WGS84)
			continue;

// 		trace(3, "eclipsing sat excluded %s sat=%s\n", obs.time.to_string(0).c_str(), obs.Sat.id().c_str());

		obs.excludeEclipse = true;
	}
}


struct SatGeom
{
	SatSys		Sat;				///< Satellite
	VectorEcef	rSat;				///< Satellite position (ECEF)
	VectorEcef	vSat;				///< Satellite velocity (ECEF)
	VectorEcef	vSatPrime;			///< Satellite velocity (ECEF + Earth rotation component)
	VectorEcef	rSun;				///< Sun position (ECEF)
	VectorEcef	rMoon;				///< Moon position (ECEF)
	VectorEcef	eNorm;				///< Normalised orbit normal vector (ECEF)
	double		beta		= 0;	///< Sun elevation angle with respect to the orbital plane
	double		betaRate	= 0;	///< dBeta/dt
	double		mu			= 0;	///< Angle of sat from 'midnight' (when sat is at the furthest point from Sun in its orbit)
	double		muRate		= 0;	///< dMu/dt
};

/** Calculates satellite orbit geometry - for use in calculating modelled yaw
 */
SatGeom satOrbitGeometry(
	SatPos&		satPos)			///< Observation
{
	SatGeom satGeom;
	auto& rSat		= satGeom.rSat;
	auto& vSat		= satGeom.vSat;
	auto& vSatPrime	= satGeom.vSatPrime;
	auto& rSun		= satGeom.rSun;
	auto& rMoon		= satGeom.rMoon;
	auto& eNorm		= satGeom.eNorm;
	auto& time		= satPos.posTime;

	rSat			= satPos.rSatCom;
	vSat			= satPos.satVel;

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	VectorEci vSatEci;
	VectorEci rSatEci = frameSwapper(rSat, &vSat, &vSatEci);

	double	beta[2];
	double	mu	[2];
	int		DT = 1;

	//do dt = 1, 0, so that the final results end up in the right place
	for (int dt : {DT, 0})
	{
		SatPos satPosDt;
		satPosDt.Sat = satPos.Sat;

		propagateEllipse(nullStream, time, dt, rSatEci, vSatEci, satPosDt);

		rSat = satPosDt.rSatCom;
		vSat = satPosDt.satVel;

		VectorEcef vSatPrime = vSat;
		vSatPrime[0]		-= OMGE * rSat[1];
		vSatPrime[1]		+= OMGE * rSat[0];

		planetPosEcef(time + dt, E_ThirdBody::MOON,	rMoon);
		planetPosEcef(time + dt, E_ThirdBody::SUN,	rSun);

		Vector3d n		= rSat.cross(vSatPrime);				//orbit-axis
		Vector3d p		= rSun.cross(n);						//ascension?

		Vector3d	eSat	= rSat.	normalized();
		Vector3d	eSun	= rSun.	normalized();
					eNorm	= n.	normalized();				//orbit-axis
		Vector3d	ep		= p.	normalized();				//ascension?

		beta[dt]	= asin(eNorm.dot(eSun));					//angle between sun and orbital plane

		double E	= acos(eSat	.dot(ep));						//angle between sat and ascension node?

		if (eSat.dot(eSun) <= 0)	mu[dt] = PI / 2 - E;		//sat on dark side
		else						mu[dt] = PI / 2 + E;		//sat on noon side

		wrapPlusMinusPi(beta[dt]);
		wrapPlusMinusPi(mu	[dt]);
	}

	double dBeta	= beta	[1] - beta	[0];
	double dMu		= mu	[1] - mu	[0];

	wrapPlusMinusPi(dBeta);
	wrapPlusMinusPi(dMu);

	satGeom.beta		= beta	[0];
	satGeom.mu			= mu	[0];
	satGeom.betaRate	= dBeta	/ DT;
	satGeom.muRate		= dMu	/ DT;
	satGeom.Sat			= satPos.Sat;

	return satGeom;
}

/** Calculate nominal (ideal) sat yaw for GPS sats
 * Returns result between (-PI, PI]
*/
double nominalYawGps(
	double	beta,		///< Sun elevation angle with respect to the orbital plane
	double	mu)			///< Angle of sat from 'midnight' (when sat is at the furthest point from Sun in its orbit)
{
	if	( abs(beta)	< 1E-12
		&&abs(mu)	< 1E-12)
		return PI;

	double yaw = atan2(-tan(beta), sin(mu)) + PI;
	wrapPlusMinusPi(yaw);
	return yaw;
}

/** Calculate nominal sat yaw for GPS sats at a given time
 * Returns result between (-PI, PI]
*/
double nominalYawGpsAtTime(
	SatGeom&	satGeom,	///< Satellite geometry
	GTime		time,		///< Time of satGeom
	GTime		reqTime)	///< Requested time
{
	double dt = (reqTime - time).to_double();
	return nominalYawGps(satGeom.beta + satGeom.betaRate * dt,	satGeom.mu + satGeom.muRate * dt);
}

/** Calculates nominal yaw rate
*/
double nominalYawRate(
	SatGeom&	satGeom)	///< Satellite geometry
{
	double nominalYaw	= nominalYawGps(satGeom.beta, satGeom.mu);
	double dt = 1E-3;
	GTime time;
	time.bigTime = 100; // Any positive value
	double nominalYaw2	= nominalYawGpsAtTime(satGeom, time, time + dt);
	double dYaw = nominalYaw2 - nominalYaw;
	wrapPlusMinusPi(dYaw);
	return dYaw / dt;
}

/** Finds time when max yaw-rate catch-up started, previous to this point in time
*/
bool findCatchupStart(
	GTime		earliestTime,		///< Search backwards until this time
	GTime		time,				///< Solution time
	SatGeom		satGeom,			///< Satellite geometry (copy)
	double		maxYawRate,			///< Maximum yaw rate (rad/s)
	GTime&		catchupTime,		///< Time of catchup start
	double		dt = -1)			///< Time step to search backwards by (sec)
{
	bool catchupExists = false;
	while (time > earliestTime)
	{
		if (abs(nominalYawRate(satGeom)) > maxYawRate)
			catchupExists = true;
		else if (catchupExists)
		{
			catchupTime = time;
			break;
		}

		time			+= dt;
		satGeom.beta	+= satGeom.betaRate	* dt;
		satGeom.mu		+= satGeom.muRate	* dt;
	}
	return catchupExists;
}

/** Calculates proportion of circle 1 visible with overlapping circle 2
 */
double circleAreaVisible(
	double r1,	///< radius of circle 1
	double r2,	///< radius of circle 2
	double d)	///< distance between circle centres
{
	double intersection = 0;
	if (d >= r1 + r2) // no overlap
	{
		intersection = 0;
	}
	else if (abs(r1 - r2) >= d) // full overlap
	{
		double smallerR = std::min(r1, r2);
        intersection = PI * SQR(smallerR);
	}
	else // partial overlap
	{
		double area1 = PI * SQR(r1);
		double area2 = PI * SQR(r2);

		double d1 = (SQR(r1) - SQR(r2) + SQR(d)) / (2 * d);
		double d2 = (SQR(r2) - SQR(r1) + SQR(d)) / (2 * d);
		intersection	= SQR(r1) * acos(d1 / r1) - d1 * sqrt(SQR(r1) - SQR(d1))
						+ SQR(r2) * acos(d2 / r2) - d2 * sqrt(SQR(r2) - SQR(d2)); // Ref: https://diego.assencio.com/?index=8d6ca3d82151bad815f78addf9b5c1c6
	}

	double area1 = PI * SQR(r1);
	return (area1 - intersection) / area1;
}

/** Calculates fraction of Sun's disk visible by spacecraft
 * 0 = full eclipse (umbra)
 * 0 < fraction < 1 = partial eclipse (within penumbra)
 * 1 = no eclipse (outside penumbra)
 */
double sunVisibility(
	Vector3d&	rSat,		///< Satellite position (ECEF)
	Vector3d&	rSun,		///< Sun position (ECEF)
	Vector3d&	rMoon)		///< Moon position (ECEF)
{
	struct PosRadius
	{
		Vector3d	pos		= Vector3d::Zero();
		double		radius	= 0;
	};
	Vector3d rEarth = Vector3d::Zero();

	PosRadius earthPosRadius	= {rEarth,	RE_WGS84};
	PosRadius moonPosRadius		= {rMoon,	MoonRadius};

	for (auto& bodyPosRadius : {earthPosRadius, moonPosRadius})
	{
		Vector3d rBody		= bodyPosRadius.pos;
		double bodyRadius	= bodyPosRadius.radius;

		Vector3d sunToSat	= -rSun		+ rSat;
		Vector3d sunToBody	= -rSun		+ rBody;
		Vector3d bodyToSat	= -rBody	+ rSat;

		if (sunToSat.norm() < sunToBody.norm())
			continue;

		Vector3d	unitSunToSat		= sunToSat.normalized();
		double		bodySatAlongSunSat	= bodyToSat.dot(	unitSunToSat);
		Vector3d	centreSeparation	= bodyToSat.cross(	unitSunToSat);

		double sunRadiusAngularSize		= SunRadius					/ sunToSat.norm();
		double bodyRadiusAngularSize	= bodyRadius				/ bodySatAlongSunSat;
		double separationAngularSize	= centreSeparation.norm()	/ bodySatAlongSunSat;

		double fraction = circleAreaVisible(sunRadiusAngularSize, bodyRadiusAngularSize, separationAngularSize);
		if (fraction < 1)
			return fraction;
	}
	return 1; // no eclipse from Earth or Moon
}

/** Returns true if satellite is in eclipse (shadow umbra); else false
*/
bool inEclipse(
	Vector3d&	rSat,		///< Satellite position (ECEF)
	Vector3d&	rSun,		///< Sun position (ECEF)
	Vector3d&	rMoon)		///< Moon position (ECEF)
{
	double visibility = sunVisibility(rSat, rSun, rMoon);

	if (visibility == 0)	return true;
	else					return false;
}

void sunMoonPos(
	GTime		time,		///< Time of
	VectorEcef&	rSun,		///< Sun position (ECEF)
	VectorEcef&	rMoon)		///< Moon position (ECEF)
{
	planetPosEcef(time, E_ThirdBody::SUN,	rSun);
	planetPosEcef(time, E_ThirdBody::MOON,	rMoon);
}

/** Finds time when eclipse started, prior (default) to this point in time
 * For finding eclipses forward in time, set dt to some positive value
*/
GTime findEclipseBoundaries(
	GTime		time,					///< Solution time
	SatGeom&	satGeom,				///< Satellite geometry
	bool		searchForward)			///< Search forward in time
{
	double precision = 1;

	VectorEcef rSat		= satGeom.rSat;
	VectorEcef vSat		= satGeom.vSat;
	VectorEcef rSun		= satGeom.rSun;
	VectorEcef rMoon	= satGeom.rMoon;

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	VectorEci rSatEci0;
	VectorEci vSatEci0;
	rSatEci0	= frameSwapper(rSat, &vSat, &vSatEci0);

	double dt = 0;
	if (searchForward)
	{
		double start	= 0;
		double interval = 0.5 * 60 * 60; // Eclipses are ~0.8-1.5hrs in length
		double end		= interval;

		// Find rough 1/2hr interval that eclipse falls within
		while (inEclipse(rSat, rSun, rMoon))
		{
			start = end;
			end += interval;

			SatPos satPos;
			satPos.Sat = satGeom.Sat;

			propagateEllipse(nullStream, time, end, rSatEci0, vSatEci0, satPos);

			rSat = satPos.rSatCom;

			sunMoonPos(time + end, rSun, rMoon);
		}

		// Binary search to nearest second
		while (start <= end)
		{
			double mid = start + (end - start) / 2;

			SatPos satPos;
			satPos.Sat = satGeom.Sat;

			propagateEllipse(nullStream, time, mid, rSatEci0, vSatEci0, satPos);

			rSat = satPos.rSatCom;

			sunMoonPos(time + mid, rSun, rMoon);

			if (inEclipse(rSat, rSun, rMoon))		start	= mid + precision;
			else									end		= mid - precision;
		}
		dt = end;
	}
	else // findEclipseBoundaries() usually gets called v. near to the start of the eclipse
	{
		while (inEclipse(rSat, rSun, rMoon))
		{
			dt -= precision;

			SatPos satPos;
			satPos.Sat = satGeom.Sat;

			propagateEllipse(nullStream, time, dt, rSatEci0, vSatEci0, satPos);

			rSat = satPos.rSatCom;

			sunMoonPos(time + dt, rSun, rMoon);
		}
	}

	return time + dt;
}

/** Yaw model for GPS-IIR sats
 * Ref: http://acc.igs.org/orbits/EclipseReadMe.pdf
 * Ref: https://igsac-cnes.cls.fr/documents/meeting/2021_04_28_Strasser_et_al_EGU21.pdf
 * Ref: https://www.researchgate.net/publication/306924379_Observed_features_of_GPS_Block_IIF_satellite_yaw_maneuvers_and_corresponding_modeling
 * Returns false if no modelled yaw available
*/
bool satYawGpsIIR(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom,		///< Satellite geometry
	double		betaBias = 0)	///< Beta angle bias
{
	auto& startTime		= attStatus.startTime;
	auto& startSign		= attStatus.startSign;
	auto& startYaw		= attStatus.startYaw;
	auto& startYawRate	= attStatus.startYawRate;
	auto& nominalYaw	= attStatus.nominalYaw;
	auto& modelYaw		= attStatus.modelYaw;
	auto& modelYawTime	= attStatus.modelYawTime;

	// Midnight / noon turns - Catch-up yaw steering
	double	maxYawRate		= 0.2 * D2R;
	bool	maxYawRateFound	= getSnxSatMaxYawRate(Sat.svn(), time, maxYawRate);
	if (maxYawRateFound	== false)
		BOOST_LOG_TRIVIAL(warning) << "Warning: Max yaw rate not found for " << Sat.svn() << " in " << __FUNCTION__ << ", check sinex files for '+SATELLITE/YAW_BIAS_RATE' block";

	nominalYaw				= nominalYawGps(satGeom.beta, satGeom.mu);
	double	dYaw			= nominalYaw - modelYaw;
	wrapPlusMinusPi(dYaw);
	if	( maxYawRateFound	== false
		||modelYawTime		== GTime::noTime())
	{
		modelYaw = nominalYaw;
		return true;
	}

	double dt = 1;
	GTime currTime = modelYawTime;
	while (currTime < time)
	{
		double dtFromTime = (currTime - time).to_double();
		double currNominalYaw = nominalYawGpsAtTime(satGeom, time, currTime);
		dYaw = currNominalYaw - modelYaw;
		wrapPlusMinusPi(dYaw);
		if	( abs(dYaw) / dt >	maxYawRate
			&&startTime		 ==	GTime::noTime()) // Not exiting eclipse
		{
			if (startSign == 0)	// Entering catchup
			{
				double currBeta	= satGeom.beta	+ satGeom.betaRate	* dtFromTime;
				startSign = SGN(dYaw);
				if	( abs(currBeta) <	abs(betaBias)
					&&SGN(currBeta) ==	SGN(betaBias))
				{
					startSign *= -1; // beta angles between 0 & betaBias result in opposite direction catch-up steering
				}
			}

			modelYaw += startSign * maxYawRate * dt;
			wrapPlusMinusPi(modelYaw);
		}
		else
		{
			modelYaw		= currNominalYaw;
			startTime		= GTime::noTime();
			startSign		= 0;
			startYaw		= 0;
			startYawRate	= 0;
		}
		currTime += dt;
	}

	if (time < attStatus.excludeTime)
		return false;

	return true;
}

/** Yaw model for GPS-IIA sats
 * Ref: http://acc.igs.org/orbits/EclipseReadMe.pdf
 * Ref: https://igsac-cnes.cls.fr/documents/meeting/2021_04_28_Strasser_et_al_EGU21.pdf
 * Ref: https://tda.jpl.nasa.gov/progress_report/42-123/123B.pdf
 * Returns false if no modelled yaw available
*/
bool satYawGpsIIA(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto& startTime		= attStatus.startTime;
	auto& startSign		= attStatus.startSign;
	auto& startYaw		= attStatus.startYaw;
	auto& startYawRate	= attStatus.startYawRate;

	attStatus.nominalYaw = nominalYawGps(satGeom.beta, satGeom.mu);
	double yawBias = 0.5 * D2R;

	if (inEclipse(satGeom.rSat, satGeom.rSun, satGeom.rMoon) == false)
		return satYawGpsIIR(Sat, attStatus, time, satGeom, yawBias);

	// Midnight turning - Shadow max yaw steering
	double maxYawRate = 0.12 * D2R;
	bool maxYawRateFound = getSnxSatMaxYawRate(Sat.svn(), time, maxYawRate);
	if (maxYawRateFound == false)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Max yaw rate not found for " << Sat.svn() << " in " << __FUNCTION__
		<< ", check sinex files for '+SATELLITE/YAW_BIAS_RATE' block";

		return false;
	}

	if (startTime == GTime::noTime()) // Start of eclipse
	{
		startTime	= findEclipseBoundaries(time, satGeom, false);
		startYaw	= nominalYawGpsAtTime(satGeom, time, startTime);
		double startBeta = satGeom.beta + satGeom.betaRate * (startTime - time).to_double();
		startSign	= SGN(yawBias);
		startYawRate = maxYawRate;
	}

	attStatus.modelYaw		= startYaw + startSign * startYawRate * (time - startTime).to_double();
	wrapPlusMinusPi(attStatus.modelYaw);
	attStatus.excludeTime	= time + 30 * 60; // Recommended to exclude up to 30min after exiting eclipse
	return true;
}

/** Yaw model for GPS-IIF sats
 * Ref: https://www.researchgate.net/publication/306924379_Observed_features_of_GPS_Block_IIF_satellite_yaw_maneuvers_and_corresponding_modeling
 * Returns false if no modelled yaw available
*/
bool satYawGpsIIF(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto& startTime		= attStatus.startTime;
	auto& startSign		= attStatus.startSign;
	auto& startYaw		= attStatus.startYaw;
	auto& startYawRate	= attStatus.startYawRate;

	attStatus.nominalYaw = nominalYawGps(satGeom.beta, satGeom.mu);

	if (inEclipse(satGeom.rSat, satGeom.rSun, satGeom.rMoon) == false)
		return satYawGpsIIR(Sat, attStatus, time, satGeom, -0.7 * D2R);

	// Midnight turning - Shadow constant yaw steering
	if (startTime == GTime::noTime()) // Start of eclipse
	{
		startTime		= findEclipseBoundaries(time, satGeom, false);
		startYaw		= nominalYawGpsAtTime(satGeom, time, startTime);

		GTime endTime	= findEclipseBoundaries(time, satGeom, true);
		double endYaw	= nominalYawGpsAtTime(satGeom, time, endTime);

		double dYaw		= endYaw - startYaw;
		wrapPlusMinusPi(dYaw);
		startYawRate	= abs(dYaw) / (endTime - startTime).to_double();
		startSign		= SGN(dYaw);
	}
	attStatus.modelYaw		= startYaw + startSign * startYawRate * (time - startTime).to_double();
	wrapPlusMinusPi(attStatus.modelYaw);
	return true;
}

/** Yaw model for GPS-III sats
 * Ref: https://www.gpsworld.com/new-type-on-the-block-generating-high-precision-orbits-for-gps-iii-satellites/
*/
bool satYawGpsIII(
	AttStatus&	attStatus,		///< Satellite att status
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto& beta = satGeom.beta;
	double betaModified = beta;
	double betaThreshold = 4.78 * D2R;
	if (abs(betaModified) < betaThreshold)
		betaModified = beta + (SGN(beta) * betaThreshold - beta) / (1 + 13000 * pow(sin(satGeom.mu), 4));

	attStatus.nominalYaw	= nominalYawGps(satGeom.beta, satGeom.mu);
	attStatus.modelYaw		= nominalYawGps(betaModified, satGeom.mu);
	return true;
}

/** Calculate nominal (ideal) sat yaw for GAL-IOV sats. Follows IGS convention.
 * Roughly equal to nominalYawGps()
 * Ref: https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.1
*/
double nominalYawGalIov(
	Vector3d&	eSunOrf)	///< Unit vector to Sun in Orbital reference frame {A,-C,-R}
{
	return atan2(	-eSunOrf(1) / sqrt(1 - SQR(eSunOrf(2))),
					-eSunOrf(0) / sqrt(1 - SQR(eSunOrf(2))));
}

/** Yaw model for GAL IOV sats. Follows original GAL convention.
 * Returns false if no modelled yaw available.
 * Aux Sun vector is not provided in [1], so is taken from [2]
 * Ref: https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.1
 * Ref: https://github.com/groops-devs/groops/blob/main/source/programs/simulation/simulateStarCameraGnss.cpp
*/
bool satYawGalIov(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto&	rSat		= satGeom.rSat;
	auto&	vSatPrime	= satGeom.vSatPrime;
	auto&	rSun		= satGeom.rSun;
	auto&	beta		= satGeom.beta;
	auto&	mu			= satGeom.mu;
	auto&	startSign	= attStatus.startSign;

	Vector3d eSunRac = ecef2rac(rSat, vSatPrime) * (rSun - rSat);
	eSunRac.normalize();
	Vector3d eSunOrf; // Orbital reference frame
	eSunOrf << eSunRac(1), -eSunRac(2), -eSunRac(0);
	attStatus.nominalYaw	= nominalYawGalIov(eSunOrf);			wrapPlusMinusPi(attStatus.nominalYaw);
	attStatus.modelYaw		= attStatus.nominalYaw;

	// Midnight/noon turns
	double sinBetaX = sin(15 * D2R);
	double sinBetaY = sin( 2 * D2R);
	if	( abs(eSunOrf(0)) < sinBetaX
		&&abs(eSunOrf(1)) < sinBetaY)
	{
		if (attStatus.modelYawTime == GTime::noTime()) // Check sat not mid-way through eclipse on startup
			return false;

		if (startSign == 0) // i.e. start of switchover period
			startSign = SGN(eSunOrf(1));

		Vector3d eSunAux;
		eSunAux(0) = eSunOrf(0);
		eSunAux(1) = 0.5 * (sinBetaY * startSign + eSunOrf(1))
					+0.5 * (sinBetaY * startSign - eSunOrf(1)) * cos(PI * abs(eSunOrf(0))/sinBetaX);
		eSunAux(2) = sqrt(1 - SQR(eSunOrf(0)) - SQR(eSunAux(1))) * SGN(eSunOrf(2));
		attStatus.modelYaw = nominalYawGalIov(eSunAux);
	}
	else
	{
		startSign = 0; // reset when exit eclipse switchover region
	}
	return true;
}

/** Smoothed yaw steering for GAL & BDS
*/
double smoothedYaw(
	double	startYaw,		///< Satellite yaw at start of modified yaw steering
	GTime	startTime,		///< Start time of modified yaw steering (due to noon/midnight turn)
	GTime	time,			///< Solution time
	double 	tMax)			///< Maximum mnvr time
{
	double sign = SGN(startYaw);
	double dtSinceStart = (time - startTime).to_double();
	return PI / 2 * sign + (startYaw - PI / 2 * sign) * cos(2 * PI / tMax * dtSinceStart);
}

/** Calculates colinear angle - the scalar angle between midnight or noon, whichever is closest
*/
double colinearAngle(
	double	mu)			///< Mu angle (rad)
{
	double colinearAngle = abs(mu);
	if (abs(mu) > PI / 2)
		colinearAngle = PI - abs(mu);
	return colinearAngle;
}

/** Yaw model for GAL FOC sats. Follows IGS convention.
 * Returns false if no modelled yaw available
 * Ref: https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.1
*/
bool satYawGalFoc(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite yaw status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto&	beta			= satGeom.beta;
	auto&	betaRate		= satGeom.betaRate;
	auto&	mu				= satGeom.mu;
	auto&	muRate			= satGeom.muRate;
	auto&	nominalYaw		= attStatus.nominalYaw;
	auto&	modelYaw		= attStatus.modelYaw;
	auto&	modelYawTime	= attStatus.modelYawTime;
	auto&	startTime		= attStatus.startTime;
	auto&	startYaw		= attStatus.startYaw;

	nominalYaw	= nominalYawGps(beta, mu);			wrapPlusMinusPi(nominalYaw);
	double betaThresh	= 4.1 * D2R;
	double colAngThresh	= 10  * D2R;
	if	( abs(beta)			< betaThresh
		&&colinearAngle(mu)	< colAngThresh)
	{
		if (startTime == GTime::noTime()) // Find start of modified-steering period
		{
			GTime currTime = time;
			double currMu = mu;
			double dt = -1;
			while (colinearAngle(currMu) < colAngThresh) // Ignore beta when finding start of modified-steering period
			{
				currTime += dt;
				currMu		= mu	+ muRate	* (currTime - time).to_double();
				wrapPlusMinusPi(currMu);
			}
			if (startTime == GTime::noTime())
				startTime = currTime;

			startYaw = nominalYawGpsAtTime(satGeom, time, currTime);
			wrapPlusMinusPi(startYaw);
		}
		modelYaw = smoothedYaw(startYaw, startTime, time, 5656);
	}
	else
	{
		modelYaw	= nominalYaw;
		startTime	= GTime::noTime();
		startYaw	= 0;
	}

	return true;
}

/** Finds time when midnight/noon-centred max yaw started, previous to this point in time
*/
void findCentredYawStart(
	GTime		earliestTime,	///< Search backwards until this time
	GTime		time,			///< Solution time
	SatGeom&	satGeom,		///< Satellite geometry
	double		maxYawRate,		///< Maximum yaw rate (rad/s)
	GTime&		startTime,		///< Time at max yaw start
	double&		startYaw,		///< Yaw at time of max yaw start
	double		dt = -1)		///< Time step to search backwards by (sec)

{
	auto&	mu				= satGeom.mu;
	auto&	muRate			= satGeom.muRate;

	GTime currTime = time;
	double currNominalYaw = 0;
	while (time > earliestTime)
	{
		currTime += -1;
		currNominalYaw		= nominalYawGpsAtTime(satGeom, time, currTime);
		double currMu		= mu + muRate * (currTime - time).to_double();
		double yawFromMid	= abs(PI / 2 - abs(currNominalYaw));			wrapPlusMinusPi(yawFromMid);
		double angleFromNoon= PI - currMu;									wrapPlusMinusPi(angleFromNoon);
		double angleFromMid	= std::min(abs(currMu), abs(angleFromNoon));	wrapPlusMinusPi(angleFromMid);	// Orbital angle
		double timeTillMid	= angleFromMid / muRate;
		double midYawRate	= yawFromMid / timeTillMid;
		if (abs(midYawRate) < maxYawRate)
			break;
	}
	startTime	= currTime;
	startYaw	= currNominalYaw;
}

/** Yaw model for GLONASS sats
 * Returns false if no modelled yaw available
 * Ref: https://igsac-cnes.cls.fr/documents/meeting/2021_04_28_Strasser_et_al_EGU21.pdf
*/
bool satYawGlo(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto&	rSat			= satGeom.rSat;
	auto&	vSatPrime		= satGeom.vSatPrime;
	auto&	rSun			= satGeom.rSun;
	auto&	rMoon			= satGeom.rMoon;
	auto&	beta			= satGeom.beta;
	auto&	mu				= satGeom.mu;
	auto&	muRate			= satGeom.muRate;
	auto&	nominalYaw		= attStatus.nominalYaw;
	auto&	modelYaw		= attStatus.modelYaw;
	auto&	startTime		= attStatus.startTime;
	auto&	startSign		= attStatus.startSign;
	auto&	startYaw		= attStatus.startYaw;

	// Nominal behaviour
	nominalYaw = nominalYawGps(beta, mu);

	double maxYawRate = 0.25 * D2R;
	bool maxYawRateFound = getSnxSatMaxYawRate(Sat.svn(), time, maxYawRate);
	if (maxYawRateFound == false)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Max yaw rate not found for " << Sat.svn() << " in " << __FUNCTION__
		<< ", check sinex files for '+SATELLITE/YAW_BIAS_RATE' block";

		return false;
	}

	// Eclipse turn - Shadow max yaw steering and stop
	if (inEclipse(rSat, rSun, rMoon))
	{
		if (startTime == GTime::noTime()) // Start of eclipse
		{
			startTime		= findEclipseBoundaries(time, satGeom, false);
			startYaw		= nominalYawGpsAtTime(satGeom, time, startTime);

			startSign	= -SGN(abs(startYaw) - PI / 2);
			if (startYaw < 0)
				startSign *= -1;
		}
		double endYaw = startSign * (PI - abs(startYaw));
		double dYaw = endYaw - startYaw;
		wrapPlusMinusPi(dYaw);
		GTime endTime = startTime + abs(dYaw) / maxYawRate;
		if (time < endTime)
		{
			modelYaw = startYaw + startSign * maxYawRate * (time - startTime).to_double();
			wrapPlusMinusPi(modelYaw);
		}
		else
		{
			modelYaw = endYaw;
		}
		return true;
	}

	double yawFromNoon		= abs(PI / 2 - abs(nominalYaw));	wrapPlusMinusPi(yawFromNoon);	// from current position to noon
	double angleFromNoon	= PI - mu;							wrapPlusMinusPi(angleFromNoon);
	double timeTillNoon		= angleFromNoon / muRate;
	double noonYawRate		= yawFromNoon / timeTillNoon;
	if (abs(noonYawRate) > maxYawRate)
	{
		if (startTime == GTime::noTime())
		{
			findCentredYawStart(attStatus.modelYawTime, time, satGeom, maxYawRate, startTime, startYaw);
			startSign	= -SGN(abs(startYaw) - PI / 2);
			if (startYaw < 0)
				startSign *= -1;
		}
		modelYaw	= startYaw + startSign * maxYawRate * (time - startTime).to_double();
	}
	else
	{
		modelYaw	= nominalYaw;
		startTime	= GTime::noTime();
		startSign	= 0;
		startYaw	= 0;
	}
	return true;
}

/** Yaw model for GLONASS Block K sats
 * Note: no yaw model exists for GLONASS-K yet
*/
bool satYawGloK(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto&	beta			= satGeom.beta;
	auto&	mu				= satGeom.mu;
	auto&	nominalYaw		= attStatus.nominalYaw;
	auto&	modelYaw		= attStatus.modelYaw;

	// Nominal behaviour
	nominalYaw = nominalYawGps(beta, mu);
	modelYaw = nominalYaw;
	return true;
}

/** Orbit-normal mode yaw (always zero)
*/
double orbitNormalYaw()
{
	return 0;
}

/** Orbit normal yaw model
 * Will set nominalYaw & modelYaw to the orbit-normal yaw (0 or PI) closest to the given nominal yaw.
*/
bool satYawOrbNor(
	AttStatus&	attStatus,		///< Satellite att status
	double		nominalYaw = 0)	///< Nominal yaw
{
	wrapPlusMinusPi(nominalYaw);
	double offset = 0;
	if (abs(nominalYaw) > PI / 2)
		offset = PI;
	attStatus.nominalYaw	= orbitNormalYaw() + offset;	wrapPlusMinusPi(attStatus.modelYaw);
	attStatus.modelYaw		= orbitNormalYaw() + offset;	wrapPlusMinusPi(attStatus.modelYaw);
	return true;
}

/** Yaw model for QZSS-1 satellites
 * Requires qzss_yaw_modes.snx - generated by scripts/qzss_ohi_merge.py and ohi-qzs*.txt's from https://qzss.go.jp/en/technical/qzssinfo/index.html
 * Ref: https://qzss.go.jp/en/technical/qzssinfo/index.html
*/
bool satYawQzs1(
	SatSys&		Sat,						///< Satellite ID
	AttStatus&	attStatus,					///< Satellite att status
	GTime		time,						///< Solution time
	SatGeom&	satGeom,					///< Satellite geometry
	bool*		orbitNormal = nullptr)		///< Returns true if satellite is in ON mode
{
	attStatus.nominalYaw = nominalYawGps(satGeom.beta, satGeom.mu);
	attStatus.modelYaw = attStatus.nominalYaw;
	if (orbitNormal)
		*orbitNormal = false;

	string attMode;
	bool attModeFound = getSnxSatAttMode(Sat.svn(), time, attMode);
	if	( attModeFound
		&&attMode.substr(0,2) == "ON")
	{
		satYawOrbNor(attStatus);
		if (orbitNormal)
			*orbitNormal = true;
	}

	return attModeFound;
}

/** Yaw model for QZSS-2I & QZSS-2A satellites
 * Requires qzss_yaw_modes.snx - generated by scripts/qzss_ohi_merge.py and ohi-qzs*.txt's from https://qzss.go.jp/en/technical/qzssinfo/index.html
 * Ref: https://qzss.go.jp/en/technical/qzssinfo/index.html
*/
bool satYawQzs2I(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto&	mu				= satGeom.mu;
	auto&	muRate			= satGeom.muRate;
	auto&	startTime		= attStatus.startTime;
	auto&	startSign		= attStatus.startSign;
	auto&	startYaw		= attStatus.startYaw;
	auto&	modelYaw		= attStatus.modelYaw;
	auto&	nominalYaw		= attStatus.nominalYaw;

	// Check for Orbit-Normal mode
	double temp = modelYaw;
	bool onMode;
	bool modelYawValid = satYawQzs1(Sat, attStatus, time, satGeom, &onMode);
	if (onMode)
		return true;
	modelYaw = temp;


	// Centered max yaw steering around noon/midnight
	double	maxYawRate		= 0.055 * D2R; // Ref: https://qzss.go.jp/en/technical/qzssinfo/khp0mf0000000wuf-att/spi-qzs2_c.pdf
	double	yawFromMid		= abs(PI / 2 - abs(attStatus.nominalYaw));	wrapPlusMinusPi(yawFromMid);
	double	angleFromNoon	= PI - mu;									wrapPlusMinusPi(angleFromNoon);
	double	angleFromMid	= std::min(abs(mu), abs(angleFromNoon));	wrapPlusMinusPi(angleFromMid);
	double	timeTillMid		= angleFromMid / muRate;
	double	midYawRate		= yawFromMid / timeTillMid;
	if (abs(midYawRate) > maxYawRate)
	{
		if (startTime == GTime::noTime())
		{
			findCentredYawStart(attStatus.modelYawTime, time, satGeom, maxYawRate, startTime, startYaw);
			startSign	= -SGN(abs(startYaw) - PI / 2);
			if (startYaw < 0)
				startSign *= -1;
		}
		modelYaw	= startYaw + startSign * maxYawRate * (time - startTime).to_double();
	}
	else
	{
		modelYaw	= nominalYaw;
		startTime	= GTime::noTime();
		startSign	= 0;
		startYaw	= 0;
	}
	return modelYawValid;
}

/** Yaw model for BDS-3I/3M satellites
 * Ref: https://doi.org/10.1007/s10291-018-0783-1 https://doi.org/10.1017/S0373463318000103
*/
bool satYawBds3(
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom,		///< Satellite geometry
	double 		tMax)			///< Maximum mnvr time
{
	auto&		beta		= satGeom.beta;
	auto&		betaRate	= satGeom.betaRate;
	auto&		mu			= satGeom.mu;
	auto&		muRate		= satGeom.muRate;
	attStatus.nominalYaw = nominalYawGps(beta, mu);
	attStatus.modelYaw = attStatus.nominalYaw;
	double alpha	= PI - mu;					wrapPlusMinusPi(alpha);
	double beta0	= 3 * D2R;
	double mu0		= 6 * D2R;

	if	( ( abs(beta)	<= beta0)
		&&( abs(alpha)	<= mu0
		  ||abs(mu)		<= mu0))
	{
		if	( alpha	== 0
			||beta	== 0)
		{
			attStatus.modelYaw = nominalYawGps(SGN(beta) * beta0, mu);
			return true;
		}

		if (attStatus.startTime == GTime::noTime()) // start of switchover period
		{
			double	currBeta	= beta;
			double	currAlpha	= alpha;
			double	currMu		= mu;
			GTime	currTime	= time;
			while	( ( abs(currBeta)	<= beta0)
					&&( abs(currAlpha)	<= mu0
						||abs(currMu)		<= mu0))
			{
				currTime += -1;
				currBeta	= beta	+ betaRate	* (currTime - time).to_double();	wrapPlusMinusPi(currBeta);
				currMu		= mu	+ muRate	* (currTime - time).to_double();	wrapPlusMinusPi(currMu);
				currAlpha	= PI - currMu;											wrapPlusMinusPi(currAlpha);
			}
			attStatus.startTime	= currTime;
			attStatus.startYaw	= nominalYawGpsAtTime(satGeom, time, currTime);
		}

		attStatus.modelYaw = smoothedYaw(attStatus.startYaw, attStatus.startTime, time, tMax);
	}
	else
	{
		attStatus.startTime	= GTime::noTime();
		attStatus.startYaw	= 0;
	}

	return true;
}

/** Yaw model for BDS-2I/2M satellites
 * Requires bds_yaw_modes.snx
 * Ref: https://doi.org/10.1007/s10291-018-0783-1 https://doi.org/10.1017/S0373463318000103
*/
bool satYawBds2(
	SatSys&		Sat,							///< Satellite ID
	AttStatus&	attStatus,						///< Satellite att status
	GTime		time,							///< Solution time
	SatGeom&	satGeom,						///< Satellite geometry
	double 		tMax)							///< Maximum mnvr time
{
	auto&	beta		= satGeom.beta;
	auto&	betaRate	= satGeom.betaRate;
	auto&	mu			= satGeom.mu;
	auto&	muRate		= satGeom.muRate;
	auto&	startTime	= attStatus.startTime;
	attStatus.nominalYaw = nominalYawGps(beta, mu);

	// Beta-dependent smoothed yaw
	string attMode;
	bool attModeFound = getSnxSatAttMode(Sat.svn(), time, attMode);
	if	( attModeFound
		&&attMode.substr(0,7) == "BETA_SY")
	{
		return satYawBds3(attStatus, time, satGeom, tMax);
	}

	// Beta-dependent orbit-normal yaw
	bool orbitNormalMode = false;
	double betaThresh = 4 * D2R;
	if (abs(beta) <= betaThresh)
	{
		if (PI - abs(attStatus.nominalYaw) < 5 * D2R)
		{
			orbitNormalMode = true;
			startTime = time;
		}
		else
		{
			if (startTime != GTime::noTime())
			{
				// Check if next yaw cycle has a qualifying yaw < 5deg & beta < 4deg
				Vector3d&	rSat		= satGeom.rSat;
				Vector3d&	vSatPrime	= satGeom.vSatPrime;

				double muAtPeak = PI / 2; // mu when yaw is closest to PI or -PI
				if (muAtPeak < mu)
					muAtPeak += 2 * PI;

				double muRange		= muAtPeak - mu;
				double timeTillPeak	= muRange / muRate;
				double betaAtPeak	= beta + betaRate * timeTillPeak;
				if (abs(betaAtPeak) <= betaThresh)
					orbitNormalMode = true;
				else
					startTime = GTime::noTime();
			}
		}
	}

	if (orbitNormalMode)	satYawOrbNor(attStatus, PI);
	else					attStatus.modelYaw = attStatus.nominalYaw;

	return true;
}

/** Yaw model for BDS-3M-SECM satellites
 * Ref: https://doi.org/10.48550/arXiv.2112.13252
*/
bool satYawBds3Secm(
	AttStatus&	attStatus,		///< Satellite att status
	SatGeom&	satGeom)		///< Satellite geometry
{
	auto&	beta		= satGeom.beta;
	auto&	mu			= satGeom.mu;
	auto&	startSign	= attStatus.startSign;
	attStatus.nominalYaw = nominalYawGps(beta, mu);
	attStatus.modelYaw = attStatus.nominalYaw;
	double beta0 = 3 * D2R;
	if (abs(beta) <= beta0)
	{
		double absYaw = abs(nominalYawGps(beta0, mu));
		if	( ( startSign	==	0)
			||( startSign	!=	SGN(beta)
			  &&absYaw		<	5 * D2R))
		{
			startSign = SGN(beta);
		}
		attStatus.modelYaw = nominalYawGps(startSign * beta0, mu);
	}
	return true;
}

/** Calculates unit vectors of satellite-fixed coordinates (ECEF) given yaw (assuming Z+ is facing toward Earth)
 */
void yawToAttVecs(
	Vector3d&	rSat,				///< Sat position (ECEF)
	Vector3d&	satVel,				///< Sat velocity (ECEF)
	double		yaw,				///< Yaw (rad)
	Vector3d&	eXSat,				///< X+ unit vector of satellite-fixed coordinates (ECEF)
	Vector3d&	eYSat,				///< Y+ unit vector of satellite-fixed coordinates (ECEF)
	Vector3d&	eZSat)				///< Z+ unit vector of satellite-fixed coordinates (ECEF)
{
	Vector3d vSatPrime = satVel;
	vSatPrime[0] -= OMGE * rSat[1];
	vSatPrime[1] += OMGE * rSat[0];
	Vector3d n = rSat.cross(vSatPrime);	//orbit-axis
	Vector3d en		= n.	normalized();
	Vector3d eSat	= rSat.	normalized();
	Vector3d ex = en.cross(eSat);

	double cosy = cos(yaw);
	double siny = sin(yaw);
	eXSat = siny * en - cosy * ex;
	eYSat = cosy * en + siny * ex;
	eZSat = eXSat.cross(eYSat);
}

/** Calculates nominal & model yaw
*/
void updateSatYaw(
	SatPos&		satPos,			///< Observation
	AttStatus&	attStatus)		///< Satellite att status. Use a disposable copy if calling inside multithreaded code
{
	if (satPos.posTime <= attStatus.modelYawTime)
		return; // The requested time is in the past, all the models are designed to propagate forwards, so we're out of luck.


	auto&	modelYawValid	= attStatus.modelYawValid;
	auto&	Sat				= satPos.Sat;
	auto&	time			= satPos.posTime;
	SatGeom	satGeom			= satOrbitGeometry(satPos);

	string	blockTypeStr	= Sat.blockType();
	std::replace(blockTypeStr.begin(), blockTypeStr.end(), '-', '_');
	std::replace(blockTypeStr.begin(), blockTypeStr.end(), '+', 'P');
	E_Block	blockType		= E_Block::UNKNOWN;
	if (E_Block::_is_valid(blockTypeStr.c_str()))
		blockType			= E_Block::_from_string_nocase(blockTypeStr.c_str());

	switch (blockType)
	{
		case E_Block::GPS_I:		// Unmodelled
		case E_Block::GPS_II:
		case E_Block::GPS_IIA:		{	modelYawValid =	satYawGpsIIA	(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::GPS_IIR_A:
		case E_Block::GPS_IIR_B:
		case E_Block::GPS_IIR_M:	{	modelYawValid =	satYawGpsIIR	(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::GPS_IIF:		{	modelYawValid =	satYawGpsIIF	(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::GPS_IIIA:		{	modelYawValid =	satYawGpsIII	(	 	attStatus,	   	 satGeom);			break;	}
		case E_Block::GLO_M:
		case E_Block::GLO_MP:
		case E_Block::GLO:			{	modelYawValid =	satYawGlo		(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::GLO_K1A:
		case E_Block::GLO_K1B:
		case E_Block::GLO_K2:		{	modelYawValid =	satYawGloK		(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::GAL_0A:		// Unmodelled
		case E_Block::GAL_0B:		// Unmodelled
		case E_Block::GAL_1:		{	modelYawValid =	satYawGalIov	(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::GAL_2:		{	modelYawValid =	satYawGalFoc	(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::BDS_2M:		{	modelYawValid =	satYawBds2		(Sat,	attStatus, time, satGeom, 3090);	break;	}
		case E_Block::BDS_2G:		{	modelYawValid =	satYawOrbNor	(		attStatus, 					PI);	break;	}
		case E_Block::BDS_2I:		{	modelYawValid =	satYawBds2		(Sat,	attStatus, time, satGeom, 5740);	break;	}
		case E_Block::BDS_3SI_SECM:	// Unmodelled
		case E_Block::BDS_3SM_CAST:	// Unmodelled
		case E_Block::BDS_3SI_CAST:	// Unmodelled
		case E_Block::BDS_3SM_SECM:	// Unmodelled
		case E_Block::BDS_3I:		{	modelYawValid =	satYawBds3		(		attStatus, time, satGeom, 5740);	break;	}
		case E_Block::BDS_3M_CAST:	{	modelYawValid =	satYawBds3		(		attStatus, time, satGeom, 3090);	break;	}
		case E_Block::BDS_3M_SECM_A:{	modelYawValid =	satYawBds3Secm	(		attStatus, 		 satGeom);			break;	}
		case E_Block::BDS_3G:		{	modelYawValid =	satYawOrbNor	(		attStatus, 					PI);	break;	}
		case E_Block::BDS_3M_SECM_B:{	modelYawValid =	satYawBds3Secm	(		attStatus, 		 satGeom);			break;	}
		case E_Block::QZS_1:		{	modelYawValid =	satYawQzs1		(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::QZS_2G:		{	modelYawValid =	satYawOrbNor	(		attStatus, 					PI);	break;	}
		case E_Block::QZS_2A:
		case E_Block::QZS_2I:		{	modelYawValid =	satYawQzs2I		(Sat,	attStatus, time, satGeom);			break;	}
		case E_Block::IRS_1I:		// Unmodelled
		case E_Block::IRS_1G:		// Unmodelled
		case E_Block::IRS_2G:		// Unmodelled
		default:					{
										modelYawValid = false;
														satYawGpsIIR	(Sat,	attStatus, time, satGeom);
										BOOST_LOG_TRIVIAL(warning) << "Warning: Attitude model not implemented for " << Sat.blockType() << " in " << __FUNCTION__ << "; using GPS-IIR model instead.";
									}
	}

	if (modelYawValid)
		attStatus.modelYawTime = time;
}

/** Recalls satellite nominal/model attitude
 * Returns false if attitude is invalid
*/
bool satAttModel(
	Vector3d&	rSat,				///< Satellite position (ECEF)
	Vector3d&	vSat,				///< Satellite velocity (ECEF)
	AttStatus&	attStatus,			///< Attitude status for orientation
	E_Source	source)				///< Type of attitude model to return
{
	double yaw;
	if (source == +E_Source::NOMINAL)	yaw = attStatus.nominalYaw;		//Nominal yaw only - no advanced noon/midnight turns
	else								yaw = attStatus.modelYaw;

	bool pass = false;
	if	( attStatus.modelYawValid
		||source == +E_Source::NOMINAL)
	{
		pass = true;
	}

	yawToAttVecs(rSat, vSat, yaw, attStatus.eXBody, attStatus.eYBody, attStatus.eZBody);

	return pass;
}

/** Converts coords of frame A (expressed in frame G) into transformation matrix from A to G
 * E.g. if eX/eY/eZ are body coordinates (frame A) in ECEF (frame G), matrix will
 *  transform from body frame to ECEF frame: ecef = rot * body.
 */
Matrix3d rotBasisMat(
	Vector3d&	eX,				///< X+ unit vector of new coordinates
	Vector3d&	eY,				///< Y+ unit vector of new coordinates
	Vector3d&	eZ)				///< Z+ unit vector of new coordinates
{
	Matrix3d rot;
	rot.col(0) = eX;
	rot.col(1) = eY;
	rot.col(2) = eZ;

	return rot;
}

/** Calculates antenna attitude - unit vectors of antenna-fixed coordinates (ECEF)
*/
void updateAntAtt(
	Vector3d&	bore,				///< Sensor boresight vector (body frame)
	Vector3d&	azim,				///< Sensor azimuth vector (body frame)
	AttStatus&	attStatus)			///< Attitude status
{
	Vector3d eE			= azim.cross(bore);
	Vector3d eN			= azim;
	Vector3d eU			= bore;

	Matrix3d ant2Body	= rotBasisMat(eE, eN, eU);
	Matrix3d body2Ecef	= rotBasisMat(attStatus.eXBody, attStatus.eYBody, attStatus.eZBody);
	Matrix3d ant2Ecef	= body2Ecef * ant2Body;

	attStatus.eXAnt	= ant2Ecef.col(0);
	attStatus.eYAnt	= ant2Ecef.col(1);
	attStatus.eZAnt	= ant2Ecef.col(2);
}

/** Retrieves precise attitude (from file) - unit vectors of satellite-fixed coordinates (ECEF)
 * Returns false if no attitude available (e.g. not found in file, not enough data to interpolate, etc.)
*/
bool preciseAttitude(
	string		id,					///< Satellite/receiver ID
	GTime		time,				///< Solution time
	AttStatus&	attStatus)			///< Attitude status
{
	auto attMapItr = nav.attMapMap.find(id);		if (attMapItr	== nav.attMapMap.end())	return false;
	auto& [dummy, attMap] = *attMapItr;
	auto entryItr2	= attMap.lower_bound(time);		if (entryItr2	== attMap	 	.end())	return false;

	auto entryItr1 = entryItr2;
	if		(entryItr1 != attMap.begin())	entryItr1--;
	else if	(entryItr2 != attMap.end())		entryItr2++;
	else									return false;

	auto& [dummy1, entry1] = *entryItr1;
	auto& [dummy2, entry2] = *entryItr2;
	Quaterniond	quat1	= entry1.q;
	Quaterniond	quat2	= entry2.q;
	GTime t1			= entry1.time;
	GTime t2			= entry2.time;

	double frac = (time	- t1).to_double()
				/ (t2	- t1).to_double();
	if	( frac < 0
		||frac > 1) //note: Quaterniond::slerp only accepts frac = [0,1]
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Insufficient precise attitude data to perform slerp in " << __FUNCTION__;
		return false;
	}

	if	( t1 < time - 6.0 * 60 * 60 // 6hrs away (180deg behind orbitwise) is too far to slerp reliably
		||t2 > time + 6.0 * 60 * 60)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: No nearby precise attitude data to perform slerp in " << __FUNCTION__;
		return false;
	}

	Quaterniond	quatNow = quat1.slerp(frac, quat2);

	Matrix3d body2Ecef = Matrix3d::Identity();
	switch (entry1.frame)
	{
		case E_ObxFrame::ECI:
		{
			Matrix3d eci2Body = quatNow.toRotationMatrix();

			Matrix3d body2Eci = eci2Body.transpose();

			ERPValues erpv = getErp(nav.erp, time);

			Matrix3d i2tMatrix;
			eci2ecef(time, erpv, i2tMatrix);

			body2Ecef = i2tMatrix * body2Eci;

			break;
		}
		case E_ObxFrame::ECEF:
		{
			Matrix3d ecef2Body = quatNow.toRotationMatrix();

			body2Ecef = ecef2Body.transpose();

			break;
		}
		case E_ObxFrame::BCRS:
		{


			break;
		}
		default:
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Unknown frame type in " << __FUNCTION__ << ": " << entry1.frame._to_string();
			return false;
		}
	}

	attStatus.eXBody = body2Ecef.col(0);
	attStatus.eYBody = body2Ecef.col(1);
	attStatus.eZBody = body2Ecef.col(2);

	return true;
}

bool kalmanAttitude(
	string			id,					///< Satellite/receiver ID
	GTime			time,				///< Solution time
	AttStatus&		attStatus,			///< Attitude status
	const KFState*	kfState_ptr)
{
	if (kfState_ptr == nullptr)
	{
		return false;
	}

	auto& kfState = *kfState_ptr;

	bool found = true;

	Quaterniond quat;

	for (int i = 0; i < 4; i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::ORIENTATION;
		kfKey.str	= id;
		kfKey.num	= i;

		if (i == 0)		{	found &= kfState.getKFValue(kfKey, quat.w());	}
		if (i == 1)		{	found &= kfState.getKFValue(kfKey, quat.x());	}
		if (i == 2)		{	found &= kfState.getKFValue(kfKey, quat.y());	}
		if (i == 3)		{	found &= kfState.getKFValue(kfKey, quat.z());	}
	}

	if (found == false)
	{
		return false;
	}

	quat.normalize();

	Matrix3d body2Ecef = quat.toRotationMatrix().transpose();

	attStatus.eXBody = body2Ecef.col(0);
	attStatus.eYBody = body2Ecef.col(1);
	attStatus.eZBody = body2Ecef.col(2);

	return true;
}

/** Calculates satellite attitude - unit vectors of satellite-fixed coordinates (ECEF)
 * Returns false if no attitude available (e.g. due to eclipse, not found in file, etc.)
*/
bool updateSatAtt(
	SatSys&				Sat,				///< Satellite ID
	GTime				time,				///< Solution time
	VectorEcef&			rSat,				///< Satellite position (ECEF)
	VectorEcef&			vSat,				///< Satellite velocity (ECEF)
	vector<E_Source>	attitudeTypes,		///< Attitude type
	AttStatus&			attStatus)			///< Attitude status
{
	bool valid = false;
	for (auto attitudeType : attitudeTypes)
	{
		switch (attitudeType)
		{
			default:	BOOST_LOG_TRIVIAL(error) << "Unknown attitudeType in " << __FUNCTION__ << ": " << attitudeType._to_string();
			case E_Source::NOMINAL:	{	valid = satAttModel	 	(			rSat, vSat,	attStatus, E_Source::NOMINAL);	break;	}
			case E_Source::MODEL:	{	valid = satAttModel	 	(			rSat, vSat,	attStatus, E_Source::MODEL);	break;	}
			case E_Source::PRECISE:	{	valid = preciseAttitude	(Sat, time,				attStatus);						break;	}
		}

		if (valid)
		{
			break;
		}
	}
	return valid;
}

/** Satellite attitude - calculates unit vectors of satellite-fixed coordinates (ECEF)
 * Returns false if no attitude available (usually due to eclipse)
*/
bool updateSatAtt(
	SatPos&				satPos,				///< satellite position data
	vector<E_Source>	attitudeTypes,		///< Attitude type
	AttStatus&			attStatus)			///< Attitude status
{
	return	updateSatAtt(
				satPos.Sat,
				satPos.posTime,
				satPos.rSatCom,
				satPos.satVel,
				attitudeTypes,
				attStatus);
}

/** Satellite attitude - calculates attitude of satellite as a quaternion (ECEF)
* Also transforms coordinates in body frame into ECEF
* Returns false if no attitude available (usually due to eclipse)
*/
bool satQuat(
	SatPos&				satPos,				///< observation
	vector<E_Source>	attitudeTypes,		///< Attitude type
	Quaterniond&		quat)				///< Rotation of satellite from ECEF
{
	auto& attStatus = satPos.satNav_ptr->attStatus;

	bool pass = updateSatAtt(satPos, attitudeTypes, attStatus);

	Matrix3d body2Ecef = rotBasisMat(attStatus.eXBody, attStatus.eYBody, attStatus.eZBody);

	quat = Quaterniond(body2Ecef);

	return pass;
}

/** Update sat nominal/model yaws.
 * Call outside of multithreading code that may reference the same satellite in different threads
 */
void updateSatAtts(
	SatPos&		satPos)		///< observation
{
	auto& satNav	= *satPos.satNav_ptr;
	auto& attStatus	= satNav.attStatus;
	auto& satOpts	= acsConfig.getSatOpts(satPos.Sat);

	updateSatYaw(satPos, 									attStatus);
	updateSatAtt(satPos, satOpts.attitudeModel.sources,		attStatus);
	updateAntAtt(satNav.antBoresight, satNav.antAzimuth,	attStatus);
}

/** Nominal receiver attitude - unit vectors of receiver-fixed coordinates (ECEF)
 * Orientation of receiver body frame for nominal receiver attitude:
 * x -> east
 * y -> north
 * z -> up
 */
bool basicRecAttitude(
	Receiver&	rec,				///< Receiver position (ECEF)
	AttStatus&	attStatus)			///< Attitude status
{
	VectorPos pos = ecef2pos(rec.aprioriPos);

	Matrix3d E;
	pos2enu(pos, E.data());

	attStatus.eXBody = E.row(0);	// x = east
	attStatus.eYBody = E.row(1);	// y = north
	attStatus.eZBody = E.row(2);	// z = up

	return true;
}

/** Attitude of receiver
 */
void recAtt(
	Receiver&			rec,				///< Receiver
	GTime				time,				///< Time
	vector<E_Source>	attitudeTypes,		///< Attitude type
	const KFState*		kfState_ptr,
	const KFState*		remote_ptr)
{
	auto& attStatus = rec.attStatus;

	bool valid = false;
	for (auto& attitudeType : attitudeTypes)
	{
		switch (attitudeType)
		{
			default:	BOOST_LOG_TRIVIAL(error) << "Unknown attitudeType in " << __FUNCTION__ << ": " << attitudeType._to_string();
			case E_Source::MODEL:	//fallthrough
			case E_Source::NOMINAL:	{	valid = basicRecAttitude(rec,				attStatus);					break;	}
			case E_Source::PRECISE:	{	valid = preciseAttitude	(rec.id,	time,	attStatus);					break;	}
			case E_Source::KALMAN:	{	valid = kalmanAttitude	(rec.id,	time,	attStatus, kfState_ptr);	break;	}
			case E_Source::REMOTE:	{	valid = kalmanAttitude	(rec.id,	time,	attStatus, remote_ptr);		break;	}
		}

		if (valid)
		{
			break;
		}
	}

	updateAntAtt(rec.antBoresight, rec.antAzimuth, attStatus);

// 	SatSys Sat(rec.id.c_str());			//todo aaron, this should be the recSatId thing instead
// 	if (Sat.prn)
// 	{
// 		nav.satNavMap[Sat].attStatus = attStatus;
// 	}
}

/** phase windup model
*/
void phaseWindup(
	GObs&		obs,	///< Observation detailing the satellite to apply model to
	Receiver&	rec,	///< Position of receiver (ECEF)
	double&		phw)	///< Output of phase windup result
{
	auto& attStatus = obs.satNav_ptr->attStatus;
	Vector3d& eXSat = attStatus.eXAnt;
	Vector3d& eYSat = attStatus.eYAnt;
	Vector3d& eZSat = attStatus.eZAnt;

	Vector3d& eXRec = rec.attStatus.eXAnt;
	Vector3d& eYRec = rec.attStatus.eYAnt;
	Vector3d& eZRec = rec.attStatus.eZAnt;

	/* unit vector satellite to receiver */
	Vector3d look = obs.satStat_ptr->e;

	//Get axis of rotation between antenna boresight and look vector
	Vector3d recAxis	= +1 *	eZRec	.cross(look)	.normalized();
	Vector3d satAxis	= -1 *	eZSat	.cross(look)	.normalized();

	//We dont need 1,2 for the first axis, because they are common

	//Get another unit vector to finish the boresight, axis, coordinate set
	Vector3d recQuad1	= +1 *	eZRec	.cross(recAxis)	.normalized();
	Vector3d satQuad1	= -1 *	eZSat	.cross(satAxis)	.normalized();

	//Get another unit vector to finish the look vector, axis, coordinate set
	Vector3d recQuad2	= 		look	.cross(recAxis)	.normalized();
	Vector3d satQuad2	= 		look	.cross(satAxis)	.normalized();

	//Apply a zero-twist rotation to the antenna to align it with the look vector.
	//Get projection of x axis on coordinate set1, then apply to coordinate set 2
	Vector3d recXnew	= eXRec.dot(recAxis) * recAxis		+ eXRec.dot(recQuad1) * recQuad2;
	Vector3d recYnew	= eYRec.dot(recAxis) * recAxis		+ eYRec.dot(recQuad1) * recQuad2;

	Vector3d satXnew	= eXSat.dot(satAxis) * satAxis		+ eXSat.dot(satQuad1) * satQuad2;
	Vector3d satYnew	= eYSat.dot(satAxis) * satAxis		+ eYSat.dot(satQuad1) * satQuad2;

	//Get angle offset by looking at receiver's new components alignment with satellite's new x component
	double angleOffset	= atan2(satXnew.dot(recYnew), -satYnew.dot(recYnew));

	//Convert to a fraction of cycles (apply an offset to match old code)
	double phaseFraction = angleOffset / (2 * PI) - 0.25;

	//keep phase windup continuous from previous result across cycles
	phw = phaseFraction + floor(phw - phaseFraction + 0.5);
}
