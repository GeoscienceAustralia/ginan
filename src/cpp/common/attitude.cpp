
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
#include "corrections.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
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

		double r = obs.rSat.norm();
		if (r <= 0)
			continue;

		/* only block IIA */
// 		if (obs.satNav_ptr->pcv.type == "BLOCK IIA")		//todo take from the satSys object
// 			continue;

		/* sun-earth-satellite angle */
		double cosa = obs.rSat.dot(esun) / r;
		
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
	VectorEcef	rSat;				///< Satellite position (ECEF)
	VectorEcef	vSatPrime;			///< Satellite velocity (ECEF + Earth rotation component)
	VectorEcef	rSun;				///< Sun position (ECEF)
	VectorEcef	rMoon;				///< Moon position (ECEF)
	VectorEcef	eNorm;				///< Normalised orbit normal vector (ECEF)
	double		beta		= 0;	///< Sun elevation angle with respect to the orbital plane
	double		mu			= 0;	///< Angle of sat from 'midnight' (when sat is at the furthest point from Sun in its orbit)
};

/** Calculates satellite orbit geometry - for use in calculating modelled yaw
 */
SatGeom satOrbitGeometry(
	GObs&		obs)			///< Observation
{
	SatGeom satGeom;
	satGeom.rSat = obs.rSat;
	
	Vector3d&	vSat		= obs.satVel;

	Vector3d&	rSat		= satGeom.rSat;
	Vector3d&	vSatPrime	= satGeom.vSatPrime;
	VectorEcef&	rSun		= satGeom.rSun;
	VectorEcef&	rMoon		= satGeom.rMoon;
	Vector3d&	eNorm		= satGeom.eNorm;
	double&		beta		= satGeom.beta;
	double&		mu			= satGeom.mu;

	planetPosEcef(obs.time, E_ThirdBody::SUN,	rSun);
	planetPosEcef(obs.time, E_ThirdBody::MOON,	rMoon);
	
	vSatPrime = vSat;
	vSatPrime[0] -= OMGE * rSat[1];
	vSatPrime[1] += OMGE * rSat[0];

	Vector3d n = rSat.cross(vSatPrime);						//orbit-axis
	Vector3d p = rSun.cross(n);								//ascension?

	Vector3d eSat	= rSat.	normalized();
	Vector3d eSun	= rSun.	normalized();
	eNorm			= n.	normalized();					//orbit-axis
	Vector3d ep		= p.	normalized();					//ascension?

	beta		= asin(eNorm.dot(eSun));					//angle between sun and orbital plane

	double E	= acos(eSat	.dot(ep));						//angle between sat and ascension node?
	if (eSat.dot(eSun) <= 0)	mu = PI/2 - E;				//sat on dark side
	else						mu = PI/2 + E;				//sat on noon side
	
	wrapPlusMinusPi(beta);
	wrapPlusMinusPi(mu);
	
	return satGeom;
}

/** Calculate nominal (ideal) sat yaw for GPS sats
*/
double nominalYawGps(
	double	beta,		///< Sun elevation angle with respect to the orbital plane
	double	mu)			///< Angle of sat from 'midnight' (when sat is at the furthest point from Sun in its orbit)
{
	if	( fabs(beta)	< 1E-12
		&&fabs(mu)		< 1E-12)
		return PI;

	return atan2(-tan(beta), sin(mu)) + PI;
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

/** Yaw model for GPS sats
 * Ref: http://acc.igs.org/orbits/EclipseReadMe.pdf
 * Returns false if no modelled yaw available
*/
bool satYawGps(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	Vector3d&	rSat		= satGeom.rSat;
	Vector3d&	vSatPrime	= satGeom.vSatPrime;
	Vector3d&	rSun		= satGeom.rSun;
	Vector3d&	rMoon		= satGeom.rMoon;
	double&		beta		= satGeom.beta;
	double&		mu			= satGeom.mu;

	// Nominal behaviour
	attStatus.nominalYaw = nominalYawGps(beta, mu);							wrapPlusMinusPi(attStatus.nominalYaw);

	// Midnight/noon turns
	double reqDyaw	= attStatus.nominalYaw	- attStatus.modelYaw;			wrapPlusMinusPi(reqDyaw);
	double dt		= (time - attStatus.modelYawTime).to_double();		
	
	double modelYawRate;
	if (dt == 0)	modelYawRate = 0;
	else			modelYawRate = reqDyaw / dt;

	// Eclipse
	double fractionSunVisible = sunVisibility(rSat, rSun, rMoon);
	if	(fractionSunVisible == 0)
	{
		if (attStatus.modelYawTime == GTime::noTime()) // Check sat not mid-way through eclipse on startup
			return false;

		string bt = Sat.blockType().substr(0,7);
		if (bt == "GPS-IIA")
			return false;
		
		if (bt == "GPS-IIF")
		{
			if (attStatus.eclipseYawRate == 0) // start of eclipse
			{
				double eclipseMuRange		= 2 * abs(mu);
				double muRate				= vSatPrime.norm() / rSat.norm();
				double eclipseDuration		= eclipseMuRange / muRate;
				double eclipseYawRange		= 2 * abs(PI/2 - abs(attStatus.nominalYaw)); // note: assumes not already at max yaw rate
				attStatus.eclipseYawRate	= eclipseYawRange / eclipseDuration * SGN(modelYawRate);
			}
			modelYawRate = attStatus.eclipseYawRate;
		}
	}
	else
	{
		attStatus.eclipseYawRate = 0;
	}

	// Max yaw rate cap
	double maxYawRate = 0;
	bool maxYawRateFound = getSnxSatMaxYawRate(Sat.svn(), time, maxYawRate);
	maxYawRate *= SGN(modelYawRate);
	if	( maxYawRateFound
		&&abs(modelYawRate) > abs(maxYawRate))
	{
		modelYawRate = maxYawRate;
	}

	attStatus.modelYaw	+= modelYawRate * dt;			wrapPlusMinusPi(attStatus.modelYaw);
	
	return true;
}

/** Calculate Sun position in Galileo orbital reference frame {Along, -Cross, -Radial}
 */
Vector3d sunPosOrf(
	double		beta,	///< Sun elevation angle with respect to the orbital plane
	double		mu)		///< Angle of sat from 'midnight' (when sat is at the furthest point from Sun in its orbit)
{
	Vector3d eSun;
	eSun(0) = -sin(mu + PI) * cos(beta);
	eSun(1) = -sin(beta);
	eSun(2) = -cos(mu + PI) * cos(beta);
	
	return eSun;
}

/** Calculate nominal (ideal) sat yaw for GAL-IOV sats.
 * For use w/ ANTEX reference frame (+X axis pointing generally toward sun)
 * (Side-note: Approximates nominalYawGps() + PI)
 * Ref: https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.1
*/
double nominalYawGalIov(
	Vector3d&	eSunRac)	///< Unit vector to Sun in local reference frame (RAC)
{
	Vector3d eSunOrf; // Orbital reference frame
	eSunOrf << eSunRac(1), -eSunRac(2), -eSunRac(0);
	
	return atan2(	eSunOrf(1) / sqrt(1 - SQR(eSunOrf(2))),
					eSunOrf(0) / sqrt(1 - SQR(eSunOrf(2))));
}

/** Yaw model for GAL IOV sats
 * Ref: https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.1
 * Returns false if no modelled yaw available
*/
bool satYawGalIov(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite att status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	Vector3d&	rSat		= satGeom.rSat;
	Vector3d&	vSatPrime	= satGeom.vSatPrime;
	Vector3d&	rSun		= satGeom.rSun;
	double&		beta		= satGeom.beta;
	double&		mu			= satGeom.mu;

	Vector3d eSunRac = ecef2rac(rSat, vSatPrime) * (rSun - rSat);
	eSunRac.normalize();
	attStatus.nominalYaw	= nominalYawGalIov(eSunRac);			wrapPlusMinusPi(attStatus.nominalYaw);
	attStatus.modelYaw		= attStatus.nominalYaw;

	// Midnight/noon turns
	Vector3d eSunO = sunPosOrf(beta, mu); // note: this is approx. == eSunOrf - might be able to use eSunOrf directly?
	double sinBetaX = sin(15 * D2R);
	double sinBetaY = sin( 2 * D2R);
	if	( abs(eSunO(0)) < sinBetaX
		&&abs(eSunO(1)) < sinBetaY)
	{
		if (attStatus.modelYawTime == GTime::noTime()) // Check sat not mid-way through eclipse on startup
			return false;

		if (attStatus.sunDeltaAtSwitch == 0) // i.e. start of eclipse switchover period
			attStatus.sunDeltaAtSwitch = SGN(eSunO(1));

		Vector3d eSunAux;
		eSunAux(0) = eSunO(0);
		eSunAux(1) = 0.5 * (sinBetaY * attStatus.sunDeltaAtSwitch + eSunO(1))
					+0.5 * (sinBetaY * attStatus.sunDeltaAtSwitch - eSunO(1)) * cos((PI * abs(eSunO(0)))/sinBetaX);
		eSunAux(2) = sqrt(1 - SQR(eSunO(0)) - SQR(eSunAux(1))) * SGN(eSunO(2));
		
		attStatus.modelYaw = nominalYawGalIov(eSunAux);
	}
	else
	{
		attStatus.sunDeltaAtSwitch = 0; // reset when exit eclipse switchover region
	}
	return true;
}

/** Yaw model for GAL FOC sats
 * Ref: https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata#3.1
 * Returns false if no modelled yaw available
*/
bool satYawGalFoc(
	SatSys&		Sat,			///< Satellite ID
	AttStatus&	attStatus,		///< Satellite yaw status
	GTime		time,			///< Solution time
	SatGeom&	satGeom)		///< Satellite geometry
{
	Vector3d&	rSat	= satGeom.rSat;
	Vector3d&	rSun	= satGeom.rSun;
	Vector3d&	eNorm	= satGeom.eNorm;
	double&		beta	= satGeom.beta;
	double&		mu		= satGeom.mu;

	// Note: GAL +X axis points towards deep space but ANTEX ref frame follows GPS Block II/IIA 
	// attitude convention where +X axis points towards Sun, so +PI rotation req'd for GAL
	attStatus.nominalYaw	= nominalYawGps(beta, mu) + PI;			wrapPlusMinusPi(attStatus.nominalYaw);
	attStatus.modelYaw		= attStatus.nominalYaw;

	// Midnight/noon turns
	Vector3d eSun = rSun.normalized();
	Vector3d eSat = rSat.normalized();
	Vector3d x = eNorm.cross(eSun);
	Vector3d y = eNorm.cross(x);
	
	double colinearAngle;
	if (eSat.dot(y) >= 0)		colinearAngle = 	 acos(eSat.dot(y));
	else						colinearAngle = PI - acos(eSat.dot(y));
	
	if	( beta			< 4.1 * D2R
		&&colinearAngle	< 10  * D2R)
	{
		if (attStatus.modelYawTime == GTime::noTime()) // Check sat not mid-way through eclipse on startup
			return false;

		if (attStatus.switchTime == GTime::noTime()) // start of switchover period
		{
			attStatus.switchTime	= time;
			attStatus.yawAtSwitch	= attStatus.nominalYaw;
		}
		
		double tMod = (time - attStatus.switchTime).to_double();
		double sign = SGN(attStatus.yawAtSwitch);
		attStatus.modelYaw = 90 * D2R * sign + (attStatus.yawAtSwitch - 90 * D2R * sign) * cos(2 * PI/5656 * tMod);
	}
	else
	{
		attStatus.switchTime	= {};
		attStatus.yawAtSwitch	= 0;
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
 * Returns false if no modelled yaw available
*/
void satYaw(
	GObs&		obs,			///< Observation
	AttStatus&	attStatus)		///< Satellite att status. Use a disposable copy if calling inside multithreaded code
{
	SatGeom satGeom = satOrbitGeometry(obs);

	switch (obs.Sat.sys)
	{
		case E_Sys::GPS:
		{	
										attStatus.modelYawValid =	satYawGps	(obs.Sat, attStatus, obs.time, satGeom);
			break;
		}
		case E_Sys::GAL:
		{
			string bt = obs.Sat.blockType().substr(0,5);
			if		(bt == "GAL-1")		attStatus.modelYawValid =	satYawGalIov(obs.Sat, attStatus, obs.time, satGeom);
			else if (bt == "GAL-2")		attStatus.modelYawValid =	satYawGalFoc(obs.Sat, attStatus, obs.time, satGeom);
			else						attStatus.modelYawValid =	satYawGalFoc(obs.Sat, attStatus, obs.time, satGeom);
			break;
		}
		default:
		{
																	satYawGps	(obs.Sat, attStatus, obs.time, satGeom);
										attStatus.modelYawValid = false;
			// BOOST_LOG_TRIVIAL(warning) << "Attitude model not implemented for " << obs.Sat.sys._to_string() << " in " << __FUNCTION__ << ", using GPS model";
			break;
		}
	}
	
										attStatus.nominalYawTime	= obs.time;
	if (attStatus.modelYawValid)		attStatus.modelYawTime		= obs.time;
}

/** Recalls satellite nominal/model attitude
 * Returns false if attitude is invalid
*/
bool satAttModel(
	Vector3d&	rSat,				///< Satellite position (ECEF)
	Vector3d&	vSat,				///< Satellite velocity (ECEF)
	AttStatus&	attStatus,			///< Attitude status
	E_Source	source)				///< 
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
void antAtt(
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
		BOOST_LOG_TRIVIAL(error)
		<< "Insufficient precise attitude data to perform slerp in " << __FUNCTION__;
		
		return false;
	}
	
	Quaterniond	quatNow = quat1.slerp(frac, quat2);
	
	Matrix3d body2Ecef;
	if		(entry1.frame == +E_ObxFrame::ECI)
	{    
		Matrix3d eci2Body = quatNow.toRotationMatrix();

		Matrix3d body2Eci = eci2Body.transpose();

		ERPValues erpv = geterp(nav.erp, time);

		Matrix3d i2tMatrix	= Matrix3d::Identity();

		eci2ecef(time, erpv, i2tMatrix);

		body2Ecef = i2tMatrix * body2Eci;
	}
	else if	(entry1.frame == +E_ObxFrame::ECEF)
	{
		Matrix3d ecef2Body = quatNow.toRotationMatrix();
		
		body2Ecef = ecef2Body.transpose();
	}
	else if	(entry1.frame == +E_ObxFrame::BCRS)
	{
		// Eugene to implement
	}
	else
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Unknown frame type in " << __FUNCTION__ << ": " << entry1.frame._to_string();
		return false;
	}

	attStatus.eXBody = body2Ecef.col(0);
	attStatus.eYBody = body2Ecef.col(1);
	attStatus.eZBody = body2Ecef.col(2);

	return true;
}

/** Calculates satellite attitude - unit vectors of satellite-fixed coordinates (ECEF)
 * Returns false if no attitude available (e.g. due to eclipse, not found in file, etc.)
*/
bool satAtt(
	SatSys&				Sat,				///< Satellite ID
	GTime				time,				///< Solution time
	VectorEcef&			rSat,				///< Satellite position (ECEF)
	VectorEcef&			vSat,				///< Satellite velocity (ECEF)
	vector<E_Source>	attitudeTypes,		///< Attitude type
	AttStatus&			attStatus,			///< Attitude status
	bool				origGal)			///< Use original GAL frame of ref (rotate 180deg yaw from default GPS/antex) - affects GAL only
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
	
	if	( origGal == false
		&&Sat.sys == +E_Sys::GAL)
	{
		attStatus.eXBody *= -1;
		attStatus.eYBody *= -1;
	}


	if (valid)		attStatus.var = acsConfig.model.sat_attitude.valid_var;
	else			attStatus.var = acsConfig.model.sat_attitude.invalid_var;

	return valid;
}

/** Satellite attitude - calculates unit vectors of satellite-fixed coordinates (ECEF)
 * Returns false if no attitude available (usually due to eclipse)
*/
bool satAtt(
	GObs&				obs,				///< Observation data
	vector<E_Source>	attitudeTypes,		///< Attitude type
	AttStatus&			attStatus,			///< Attitude status
	bool				origGal = false)	///< Use original GAL frame of ref (rotate 180deg yaw from default GPS/antex) - affects GAL only
{
	return	satAtt(
				obs.Sat,
				obs.time,
				obs.rSat,
				obs.satVel,
				attitudeTypes,
				attStatus,
				origGal);
}

/** Satellite attitude - calculates attitude of satellite as a quaternion (ECEF)
* Also transforms coordinates in body frame into ECEF
* Returns false if no attitude available (usually due to eclipse)
*/
bool satQuat(
	GObs&				obs,				///< observation
	vector<E_Source>	attitudeTypes,		///< Attitude type
	Quaterniond&		quat,				///< Rotation of satellite from ECEF
	bool				origGal)			///< Use original GAL frame of ref (rotate 180deg yaw from default GPS/antex) - affects GAL only
{
	auto& attStatus = obs.satNav_ptr->attStatus;
	
	bool pass = satAtt(obs, attitudeTypes, attStatus, origGal);

	Matrix3d body2Ecef = rotBasisMat(attStatus.eXBody, attStatus.eYBody, attStatus.eZBody);
	
	quat = Quaterniond(body2Ecef);

	return pass;
}

/** Update sat nominal/model yaws.
 * Call outside of multithreading code
 */
void updateSatAtts(
	GObs&		obs)		///< observation
{
	auto& satNav	= *obs.satNav_ptr;
	auto& attStatus = satNav.attStatus;

	satYaw(obs, 										attStatus);
	satAtt(obs, acsConfig.model.sat_attitude.sources,	attStatus);
	antAtt(satNav.antBoresight, satNav.antAzimuth,		attStatus);
}


/** Nominal receiver attitude - unit vectors of receiver-fixed coordinates (ECEF)
 * Orientation of receiver body frame for nominal receiver attitude:
 * x -> east
 * y -> north
 * z -> up
 */
bool basicRecAttitude(
	Station&	rec,				///< Receiver position (ECEF)
	AttStatus&	attStatus)			///< Attitude status
{
	VectorPos pos = ecef2pos(rec.sol.sppRRec);
	
	Matrix3d E;
	pos2enu(pos, E.data());

	attStatus.eXBody = E.row(0);	// x = east
	attStatus.eYBody = E.row(1);	// y = north
	attStatus.eZBody = E.row(2);	// z = up
	
	return true;
}

void recAtt(
	Station&			rec,				///< Receiver
	GTime				time,
	vector<E_Source>	attitudeTypes)		///< Attitude type
{
	auto& attStatus = rec.attStatus;
	
	bool valid = false;
	for (auto& attitudeType : attitudeTypes)
	{
		switch (attitudeType)
		{
			default:	BOOST_LOG_TRIVIAL(error) << "Unknown attitudeType in " << __FUNCTION__ << ": " << attitudeType._to_string();
			case E_Source::MODEL:	//fallthrough
			case E_Source::NOMINAL:	{	valid = basicRecAttitude(rec,				attStatus);			break;	}
			case E_Source::PRECISE:	{	valid = preciseAttitude	(rec.id,	time,	attStatus);			break;	}
		}
		
		if (valid)
		{
			break;
		}
	}
	
	antAtt(rec.antBoresight, rec.antAzimuth, attStatus);
}

/** phase windup model
*/
void phaseWindup(
	GObs&		obs,	///< Observation detailing the satellite to apply model to
	Station&	rec,	///< Position of receiver (ECEF)
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
