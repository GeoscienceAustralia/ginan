
// #pragma GCC optimize ("O0")

#include "navigation.hpp"
#include "instrument.hpp"
#include "constants.hpp"
#include "satRefSys.hpp"
#include "acsConfig.hpp"
#include "gravity.hpp"
#include "jplEph.hpp"
#include "common.hpp"
#include "sofa.hpp"


Vector3d CalcPolarAngles(
	Vector3d mVec)
{
	/* Norm of vector
	*/
	double mR = mVec.norm();

	/* Azimuth of vector
	*/
	double mPhi;
	if ((mVec(0) == 0) && (mVec(1) == 0))	{		mPhi = 0;						}	
	else									{		mPhi = atan2(mVec(1), mVec(0));	}
	
	if (mPhi < 0)
	{
		mPhi += 2 * PI;
	}

	double rho = sqrt(mVec(0) * mVec(0) + mVec(1) * mVec(1)); // Length of projection in x - y - plane:

	/* Altitude of vector
	*/
	double mTheta;
	if ((mVec(2) == 0) && (rho == 0))		{		mTheta = 0;						}	
	else									{		mTheta = atan2(mVec(2), rho);	}

	Vector3d vecRAE;
	vecRAE(0) = mR;
	vecRAE(1) = mPhi;
	vecRAE(2) = mTheta;

	return  vecRAE;
}

/* Calculate normalized Legendre polynomial values
*
*/
MatrixXd Legendre(
	int m,		///< Maximum degree
	int n,		///< Maximum order
	double phi) ///< Geocentric latitude in radian
{
	MatrixXd pnm = MatrixXd::Zero(m + 1, n + 1);

	pnm(0, 0) = 1;
	pnm(1, 1) = sqrt(3) * cos(phi);

	/* diagonal coefficients
	*/
	for (int i = 2; i <= m; i++)
	{
		double s = i;
		pnm(i, i) = sqrt((2 * s + 1) / (2 * s)) * cos(phi) * pnm(i - 1, i - 1);
	}

	/* horizontal first step coefficients
	*/
	for (int i = 1; i <= m; i++)
	{
		double s = i;
		pnm(i, i - 1) = sqrt(2 * s + 1) * sin(phi) * pnm(i - 1, i - 1);
	}

	/* horizontal second step coefficients
	*/
	int j = 0, k = 2;
	do
	{
		for (int i = k; i <= m; i++)
		{
			double s = i;
			double h = j;
			pnm(i, j) = sqrt((2 * s + 1) / ((s - h) * (s + h))) * (sqrt(2 * s - 1) * sin(phi) * pnm(i - 1, j) - sqrt(((s + h - 1) * (s - h - 1)) / (2 * s - 3)) * pnm(i - 2, j));
		}
		j++;
		k++;
	}
	while (j <= n);

	return pnm;
}

/* Calculate normalized Legendre polynomial first derivative values
*
*/
MatrixXd LegendreD(
	int 			m,		///< Maximum degree
	int 			n,		///< Maximum order
	MatrixXd		pnm,	///< Normalised Legendre polynomial matrix
	double 			phi) 	///< Geocentric latitude in radian
{
	MatrixXd dpnm = MatrixXd::Zero(m + 1, n + 1);
	dpnm(0, 0) = 0;
	dpnm(1, 1) = -sqrt(3) * sin(phi);

	/* diagonal coefficients
	*/
	for (int i = 2; i <= m; i++)
	{
		double s = i;
		dpnm(i, i) = sqrt((2 * s + 1) / (2 * s)) * (cos(phi) * dpnm(i - 1, i - 1) - sin(phi) * pnm(i - 1, i - 1));
	}

	/* horizontal first step coefficients
	*/
	for (int i = 1; i <= m; i++)
	{
		double s = i;
		dpnm(i, i - 1) = sqrt(2 * s + 1) * ((cos(phi) * pnm(i - 1, i - 1)) + (sin(phi) * dpnm(i - 1, i - 1)));
	}

	/* horizontal second step coefficients
	*/
	int j = 0, k = 2;
	do
	{
		for (int i = k; i <= m; i++)
		{
			double s = i;
			double h = j;
			dpnm(i, j) = sqrt((2 * s + 1) / ((s - h) * (s + h))) * ((sqrt(2 * s - 1) * sin(phi) * dpnm(i - 1, j)) + sqrt(2 * s - 1) * cos(phi) * pnm(i - 1, j) - sqrt(((s + h - 1) * (s - h - 1)) / (2 * s - 3)) * dpnm(i - 2, j));
		}
		j++;
		k++;
	}
	while (j <= n);

	return dpnm;
}

GravityModel::GravityModel(
	EarthGravMdlOpt			gravMdlOpt,
	EGMCoef	      			uncorrectedEgmCoef)
{
	gravModelType				= gravMdlOpt.earthGravMdl;
	mEarthGravAccDeg			= gravMdlOpt.earthGravAccDeg;
	mEarthGravSTMDeg			= gravMdlOpt.earthGravSTMDeg;
	this->uncorrectedEgmCoef	= uncorrectedEgmCoef;	
}

void GravityModel::solidEarthTidesCorrection(
	Trace&               trace,
	double				 mjdUTC,			///< UTC in modified julian date format
	EGMCoef&	         egmCoef,			///< Struct of Earth gravity coefficients
	IERS&		 		 iers,				///< Instance of IERS class
	const Vector3d&		 vecRAESun,			///< Rho, azimuth and altitude information of Sun
	const Vector3d&		 vecRAEMoon)		///< Rho, azimuth and altitude information of Moon
{
	/* Effect of Solid Earth Tides(elastic Earth)
	* For dC21and dS21
	* The coefficients we choose are in - phase(ip) amplitudes and out - of - phase amplitudes of the
	* corrections for frequency dependence, and multipliers of the Delaunay variables
	* Refer to Table 6.5a in IERS2010
	*/
	double coeff0[48][7] =
	{
		{2, 0, 2, 0, 2, -0.1, 0},
		{0, 0, 2, 2, 2, -0.1, 0},
		{1, 0, 2, 0, 1, -0.1, 0},
		{1, 0, 2, 0, 2, -0.7, 0.1},
		{-1, 0, 2, 2, 2, -0.1, 0},
		{0, 0, 2, 0, 1, -1.3, 0.1},
		{0, 0, 2, 0, 2, -6.8, 0.6},
		{0, 0, 0, 2, 0, 0.1, 0},
		{1, 0, 2, -2, 2, 0.1, 0},
		{-1, 0, 2, 0, 1, 0.1, 0},
		{-1, 0, 2, 0, 2, 0.4, 0},
		{1, 0, 0, 0, 0, 1.3, -0.1},
		{1, 0, 0, 0, 1, 0.3, 0},
		{-1, 0, 0, 2, 0, 0.3, 0},
		{-1, 0, 0, 2, 1, 0.1, 0},
		{0, 1, 2, -2, 2, -1.9, 0.1},
		{0, 0, 2, -2, 1, 0.5, 0},
		{0, 0, 2, -2, 2, -43.4, 2.9},
		{0, -1, 2, -2, 2, 0.6, 0},
		{0, 1, 0, 0, 0, 1.6, -0.1},
		{-2, 0, 2, 0, 1, 0.1, 0},
		{0, 0, 0, 0, -2, 0.1, 0},
		{0, 0, 0, 0, -1, -8.8, 0.5},
		{0, 0, 0, 0, 0, 470.9, -30.2},
		{0, 0, 0, 0, 1, 68.1, -4.6},
		{0, 0, 0, 0, 2, -1.6, 0.1},
		{-1, 0, 0, 1, 0, 0.1, 0},
		{0, -1, 0, 0, -1, -0.1, 0},
		{0, -1, 0, 0, 0, -20.6, -0.3},
		{0, 1, -2, 2, -2, 0.3, 0},
		{0, -1, 0, 0, 1, -0.3, 0},
		{-2, 0, 0, 2, 0, -0.2, 0},
		{-2, 0, 0, 2, 1, -0.1, 0},
		{0, 0, -2, 2, -2, -5.0, 0.3},
		{0, 0, -2, 2, -1, 0.2, 0},
		{0, -1, -2, 2, -2, -0.2, 0},
		{1, 0, 0, -2, 0, -0.5, 0},
		{1, 0, 0, -2, 1, -0.1, 0},
		{-1, 0, 0, 0, -1, 0.1, 0},
		{-1, 0, 0, 0, 0, -2.1, 0.1},
		{-1, 0, 0, 0, 1, -0.4, 0},
		{0, 0, 0, -2, 0, -0.2, 0},
		{-2, 0, 0, 0, 0, -0.1, 0},
		{0, 0, -2, 0, -2, -0.6, 0},
		{0, 0, -2, 0, -1, -0.4, 0},
		{0, 0, -2, 0, 0, -0.1, 0},
		{-1, 0, -2, 0, -2, -0.1, 0},
		{-1, 0, -2, 0, -1, -0.1, 0}
	};

	/* For dC22and dS22, Refer to Table 6.5c in IERS2010
	*/
	double  coeff2[2][6] =
	{
		{1, 0, 2, 0, 2, -0.3},
		{0, 0, 2, 0, 2, -1.2}
	};

	/* STEP1 CORRECTIONS
	*/
	for (auto body : {eSun ,eMoon})
	{
		double		GM_Body;
		double		rho;
		double		az;
		MatrixXd	lg;
		if (body == eMoon)	{	GM_Body = GM_Moon;	rho = vecRAEMoon(0);	az = vecRAEMoon(1);		lg = Legendre	(4, 4, vecRAEMoon(2));		}
		if (body == eSun)	{	GM_Body = GM_Sun;	rho = vecRAESun (0);	az = vecRAESun (1);		lg = Legendre	(4, 4, vecRAESun (2));		}
		
		double GM_Ratio = GM_Body / GM_Earth;
		double RE_Rho_Pow;
		
		double extPotentialLoveNumbers[4][4] = 
		{
			{},
			{},
			{0.29525,	0.29470,	0.29801				},
			{0.093,		0.093,		0.093,		0.094	}
		};
		
		for (int n = 2; n <= 3; n++)
		for (int m = 0; m <= n; m++)
		{
			double loveNumber	= extPotentialLoveNumbers[n][m];
			double div			= 2*n+1;
			
			RE_Rho_Pow = pow((RE_WGS84 / rho), n+1);
			egmCoef.smn(n, m) += (loveNumber / div) * GM_Ratio * RE_Rho_Pow * lg(n, m) * sin(m * az);
			egmCoef.cmn(n, m) += (loveNumber / div) * GM_Ratio * RE_Rho_Pow * lg(n, m) * cos(m * az);
		}
		
		
		double extPotentialLoveNumbers_[3] = 
		{
			-0.00087,	-0.00079,	-0.00057
		};
		
		for (int m = 0; m <= 2; m++)
		{
			double loveNumber	= extPotentialLoveNumbers_[m];
			double div			= 5;
			
			RE_Rho_Pow = pow((RE_WGS84 / rho), 3);
			egmCoef.smn(4, m) += (loveNumber / div) * GM_Ratio * RE_Rho_Pow * lg(2, m) * sin(m * az);
			egmCoef.cmn(4, m) += (loveNumber / div) * GM_Ratio * RE_Rho_Pow * lg(2, m) * cos(m * az);
		}	                                                                 
	}
	
	/* STEP2 CORRECTIONS
	*/
	double mjdUT1	= iers.UT1_UTC	() / 86400.0 + mjdUTC;
	double mjdTT	= iers.TT_UTC	() / 86400.0 + mjdUTC;

	double theta_g = iauGmst06(JD2MJD, mjdUT1, JD2MJD, mjdTT);

	{
		double dC21 = 0;
		double dS21 = 0;
		for (int jj = 0; jj < 48; jj++)
		{
			dC21 += 1e-12 * coeff0[jj][5] * sin(theta_g + PI);	//todo aaron, cos -sin
			dS21 += 1e-12 * coeff0[jj][5] * cos(theta_g + PI);
		}
		
		egmCoef.cmn(2, 1) += dC21;
		egmCoef.smn(2, 1) += dS21;
	}

	{
		double dC22 = 0;
		double dS22 = 0;
		for (int jj = 0; jj <= 1; jj++)
		{
			dC22 += 1e-12 * coeff2[jj][5] * sin(theta_g + PI);
			dS22 += 1e-12 * coeff2[jj][5] * cos(theta_g + PI);
		}
		
		egmCoef.cmn(2, 2) += dC22;
		egmCoef.smn(2, 2) += dS22;
	}

	/* Treatment of the Permanent Tide(elastic Earth)
	*/
	{
		double  dC20 = 4.4228e-8 * (-0.31460) * 0.29525;
		
		egmCoef.cmn(2, 0) -= dC20;
	}

	/* Effect of Solid Earth Pole Tide(elastic Earth)
	*/
	{
		double dC21 = -1.290e-9 * iers.xp;
		double dS21 = +1.290e-9 * iers.yp;
		
		egmCoef.cmn(2, 1) += dC21;
		egmCoef.smn(2, 1) += dS21;
	}
}

void GravityModel::oceanTidesCorrection(
	Trace&               trace,
	double				 mjdUTC,			///< UTC in modified julian date format
	EGMCoef&	         egmCoef,			///< Struct of Earth gravity coefficients
	const Vector3d&		 vecRAESun,			///< Rho, azimuth and altitude information of Sun
	const Vector3d&		 vecRAEMoon)		///< Rho, azimuth and altitude information of Moon
{
	/* rho, azimuth and elevation (altitude) of Sun
	*/
	double rhoSun 	= vecRAESun(0);
	double azSun	= vecRAESun(1);
	double elSun	= vecRAESun(2);

	/* rho, azimuth and elevation (altitude) of Sun
	*/
	double rhoMoon 	= vecRAEMoon(0);
	double azMoon	= vecRAEMoon(1);
	double elMoon	= vecRAEMoon(2);

	for (auto body : {eSun ,eMoon})
	{
		double		GM_Body;
		double		rho;
		double		az;
		MatrixXd	lg;
		if (body == eMoon)	{	GM_Body = GM_Moon;	rho = vecRAEMoon(0);	az = vecRAEMoon(1);		lg = Legendre	(7, 7, vecRAEMoon(2));		}
		if (body == eSun)	{	GM_Body = GM_Sun;	rho = vecRAESun (0);	az = vecRAESun (1);		lg = Legendre	(7, 7, vecRAESun (2));		}
		
		double GM_Ratio = GM_Body / GM_Earth;
		double RE_Rho_Pow;
		
		double loadDefCoeffs[7] = 
		{
			0,
			0,
			-0.3075,
			-0.1950,
			-0.1320,
			-0.1032,
			-0.0892
		};
		
		for (int n = 2; n <= 6; n++)
		{
			RE_Rho_Pow = pow((RE_WGS84 / rho), n+1);
			
			double coeff_n	= loadDefCoeffs[n];
			double div		= 2*n+1;
			
			for (int m = 0; m <= n; m++)
			{
				egmCoef.smn(n, m) += GM_Ratio * RE_Rho_Pow * lg(n, m) * sin(m * az) / div * coeff_n;
				egmCoef.cmn(n, m) += GM_Ratio * RE_Rho_Pow * lg(n, m) * cos(m * az) / div * 4 * PI * SQR(RE_WGS84) * RHO_w / M_Earth * (1 + coeff_n);
			}
		}
	}
}

void GravityModel::correctEgmCoefficients(
	Trace&               trace,
	const double 		 mjdUTC,
	ERPValues&			 erpv,			///< xp, yp, ut1_utc, lod and leapsecond
	const Matrix3d&      mECI2BF)		///< Transformation matrix from ECI to central body fixed system)
{
	//copy the uncorrected then (re)apply corrections
	correctedEgmCoef = uncorrectedEgmCoef;
	
	if	(  acsConfig.forceModels.solid_earth_tides	== false
		&& acsConfig.forceModels.ocean_tide_loading	== false)
	{
		//no corrections need to be applied.
		return;
	}
	
	auto& egmCoef = correctedEgmCoef;
	
	Instrument instrument("gravtop");
	
	double dUTC_TAI	= -(19 + erpv.leaps);
	double xp		= erpv.xp;
	double yp		= erpv.yp;
	double dUT1_UTC	= erpv.ut1_utc;
	double lod		= erpv.lod;
	
	IERS iers = IERS(dUT1_UTC, dUTC_TAI, xp, yp, lod);
	
	double jdTT = mjdUTC + JD2MJD + iers.TT_UTC() / 86400.0;

	// from inertial coordinate to earth centred fixed coordiante		
	Vector3d rSun;			jplEphPos(nav.jplEph_ptr, jdTT, E_ThirdBody::SUN,	rSun);				rSun	= mECI2BF * rSun; 
	Vector3d rMoon;			jplEphPos(nav.jplEph_ptr, jdTT, E_ThirdBody::MOON,	rMoon);				rMoon	= mECI2BF * rMoon;

	Vector3d vecRAESun	= CalcPolarAngles(rSun); // calculating the range, azimuth and altitude
	Vector3d vecRAEMoon	= CalcPolarAngles(rMoon);

	if (acsConfig.forceModels.solid_earth_tides)
	{
		Instrument instrument("solid");
		solidEarthTidesCorrection	(trace, mjdUTC, egmCoef, iers,	vecRAESun, vecRAEMoon);
	}

	if (acsConfig.forceModels.ocean_tide_loading)
	{
		Instrument instrument("ocean");
		oceanTidesCorrection		(trace, mjdUTC, egmCoef,		vecRAESun, vecRAEMoon);
	}
}

Vector3d GravityModel::centralBodyGravityAcc(
	Trace&               trace,
	const double 		 mjdUTC,
	ERPValues&			 erpv,			///< xp, yp, ut1_utc, lod and leapsecond
	const Vector3d&      rSat,			///< Satellite position vector in the inertial system
	const Matrix3d&      eci2ecef,		///< Transformation matrix from ECI to central body fixed system
	bool				 bVarEq)		///< bVarEq = 1 if for the variational equation
{
	Instrument instrument(__FUNCTION__);
	
	auto& egmCoef = correctedEgmCoef;
	
	int mMax;
	int nMax;
	if (bVarEq)		{	mMax = mEarthGravSTMDeg.mMax;	nMax = mEarthGravSTMDeg.nMax;	}
	else			{	mMax = mEarthGravAccDeg.mMax;	nMax = mEarthGravAccDeg.nMax;	}

	Vector3d rSat_ecef = eci2ecef * rSat;
	
	double R = rSat_ecef.norm();
	
	double rSat_latgc = asin(rSat_ecef.z() / R);					// Geocentric latitude of satellite (n)
	double rSat_longc = atan2(rSat_ecef.y(), rSat_ecef.x());		// Geocentric longitude of satellite (n)

	MatrixXd pnm	= Legendre (mMax, nMax,			rSat_latgc);		// Legendre matrix given order/degree
	MatrixXd dpnm	= LegendreD(mMax, nMax, pnm,	rSat_latgc);		// Normalised Legendre matrix given order/degree

	double dUdr		= 0;
	double dUdlatgc	= 0;
	double dUdlongc	= 0;
	double q1		= 0;
	double q2		= 0;
	double q3		= 0;
	for (int m = 0; m <= nMax; m++)
	{
		int nd = m;
		double b1 = (-GM_Earth / SQR(R))	* pow((RE_WGS84 / R), nd) * (m + 1);
		double b2 = ( GM_Earth / R)			* pow((RE_WGS84 / R), nd);
		double b3 = ( GM_Earth / R)			* pow((RE_WGS84 / R), nd);

		for (int n = 0; n <= mMax; n++)
		{
			q1 += 		pnm (m, n) * (egmCoef.cmn(m, n) * cos(n * rSat_longc) + egmCoef.smn(m, n) * sin(n * rSat_longc));
			q2 += 		dpnm(m, n) * (egmCoef.cmn(m, n) * cos(n * rSat_longc) + egmCoef.smn(m, n) * sin(n * rSat_longc));
			q3 += n * 	pnm (m, n) * (egmCoef.smn(m, n) * cos(n * rSat_longc) - egmCoef.cmn(m, n) * sin(n * rSat_longc));
		}

		dUdr		+= q1 * b1;
		dUdlatgc	+= q2 * b2;
		dUdlongc	+= q3 * b3;

		q3 = 0;
		q2 = q3;
		q1 = q2;
	}

	double r2xy	= SQR(rSat_ecef.x()) + SQR(rSat_ecef.y());

	Vector3d acc;

    acc.x() = rSat_ecef.x() * dUdr / R 		- dUdlatgc * rSat_ecef.z() / SQR(R) / sqrt(r2xy) / rSat_ecef.x()			- rSat_ecef.y() * dUdlongc / r2xy;
    acc.y() = rSat_ecef.y() * dUdr / R 		- dUdlatgc * rSat_ecef.z() / SQR(R) / sqrt(r2xy) / rSat_ecef.y()			+ rSat_ecef.x() * dUdlongc / r2xy;
    acc.z() = rSat_ecef.z() * dUdr / R 		+ dUdlatgc * sqrt(r2xy)  / SQR(R);
	
	return eci2ecef.transpose() * acc;
}

/* Relativistic Effects
*
*/
Vector3d GravityModel::relativityEffectsAcc(
	Trace&    				trace,	 		///< Trace to output to (similar to cout)
	const Vector3d& 		rSat,			///< Inertial position of satellite (m)
	const Vector3d& 		vSat)			///< Inertial velocity of satellite (m/s)
{
	double R = rSat.norm();
	double V = vSat.norm();

	Vector3d acc	= GM_Earth / (SQR(CLIGHT) * pow(R, 3)) 
					* (		(4 * GM_Earth / R	- SQR(V))		* rSat				+ 4 * rSat.dot(vSat) * vSat);
					
	return acc;
}


Vector3d accelPointMassGravity(
	Trace&               	trace,
	const double 			mjdTT, 
	const Vector3d&			pos,
	E_ThirdBody				thirdBody,
	Vector3d&				thirdBodyPos)
{
	double mGM = 0;

	switch (thirdBody)
	{
		case E_ThirdBody::MERCURY:		mGM = GM_Mercury;		break;		
		case E_ThirdBody::VENUS:		mGM = GM_Venus;			break;		
		case E_ThirdBody::EARTH:		mGM = GM_Earth;			break;	
		case E_ThirdBody::MARS:			mGM = GM_Mars;			break;		
		case E_ThirdBody::JUPITER:		mGM = GM_Jupiter;		break;	
		case E_ThirdBody::SATURN:		mGM = GM_Saturn;		break;	
		case E_ThirdBody::URANUS:		mGM = GM_Uranus;		break;	
		case E_ThirdBody::NEPTUNE:		mGM = GM_Neptune;		break;	
		case E_ThirdBody::PLUTO:		mGM = GM_Pluto;			break;		
		case E_ThirdBody::MOON:			mGM = GM_Moon;			break;		
		case E_ThirdBody::SUN:			mGM = GM_Sun;			break;
	}
	
	Vector3d relativePos = pos - thirdBodyPos;
	
	/* Acceleration
	*/
	return  -mGM * ( 	relativePos	.normalized()	/ relativePos	.squaredNorm() 
					+	thirdBodyPos.normalized()	/ thirdBodyPos	.squaredNorm());
}

