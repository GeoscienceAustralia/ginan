
#include "acceleration.hpp"
#include "constants.hpp"
#include "common.hpp"


#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

Legendre::Legendre()
{
	nmax = 0;
	init();
}

Legendre::Legendre(
	int nmax)
	:	nmax(nmax)
{
	init();
}

void Legendre::setNmax(
	int n)
{
	nmax = n;
	init();
}

void Legendre::init()
{
	anm		= MatrixXd::Zero(nmax + 1, nmax + 1);
	bnm		= MatrixXd::Zero(nmax + 1, nmax + 1);
	fnm		= MatrixXd::Zero(nmax + 1, nmax + 1);
	Pnm		= MatrixXd::Zero(nmax + 1, nmax + 1);
	dPnm	= MatrixXd::Zero(nmax + 1, nmax + 1);

	for (int n = 0; n < nmax + 1;	n++)
	for (int m = 0; m <= n;			m++)
	{
		anm(n, m) = sqrt(((double)(2 * n - 1) * (2 * n + 1))				/ ((n - m) * (n + m)));
		bnm(n, m) = sqrt(((double)(2 * n + 1) * (n + m - 1) * (n - m - 1))	/ ((n - m) * (n + m) * (2 * n - 3)));
		fnm(n, m) = sqrt(((double)(SQR(n) - SQR(m)) * (2 * n + 1))			/ (2 * n - 1));
	}
}

void Legendre::calculate(double x)
{
	if (abs(x) >= 1)
		throw std::runtime_error("Legendre polynomial computation arguments should be between -1 and 1.");

	double u = sqrt(1 - SQR(x));
	Pnm(0, 0) = 1;
	Pnm(1, 0) = sqrt(3) * x;
	Pnm(1, 1) = sqrt(3) * u;

	dPnm(1, 0) = 1 / u * (x * Pnm(1, 0) - sqrt(3) * Pnm(0, 0));
	dPnm(1, 1) = x / u * Pnm(1, 1);

	for (int n = 2; n < nmax + 1; n++)
	{
		for (int m = 0; m < n; m++)
		{
			Pnm	(n, m) = anm(n, m)		* x * Pnm(n - 1,	m)	- bnm(n, m)	* Pnm(n - 2, m);
			dPnm(n, m) = 1 / u * ( n	* x * Pnm(n,		m)	- fnm(n, m)	* Pnm(n - 1, m));
		}
		Pnm	(n, n) = u * sqrt((2.0 * n + 1) / (2 * n)) * Pnm(n - 1, n - 1);
		dPnm(n, n) = n * x / u * Pnm(n, n);
	}
}

/** Central force acceleration
 */
Vector3d accelCentralForce(
	const	Vector3d	observer, 		///< position of the oberserver
	const	double		GM,				///< value of GM of the acting force
			Matrix3d*	dAdPos_ptr)		///< Optional pointer to differential matrix
{
	Vector3d acc = -1 * GM * observer.normalized() / observer.squaredNorm();

	if (dAdPos_ptr)
	{
		*dAdPos_ptr += GM / pow(observer.norm(), 5) * (3 * observer * observer.transpose() - Matrix3d::Identity() * observer.squaredNorm());
	}

	return acc;
}

/** Source point acceleration from a planet acting on a satellite.
 */
Vector3d accelSourcePoint(
	const	Vector3d	sat,		///< Vector posiiong of the satellite w.r.t to the orbiting body
	const	Vector3d	planet, 	///< Vector position of the planet w.r.t to the orbiting body
	const	double		GM,			///< Constant GM of the acting body
			Matrix3d*	dAdPos_ptr)	///< Optional pointer to differential matrix
{
	Vector3d relativePosition = planet - sat;
	Vector3d acc_sat 	= accelCentralForce(relativePosition,	GM, dAdPos_ptr);
	Vector3d acc_e 		= accelCentralForce(planet,				GM, dAdPos_ptr);

	return  acc_e - acc_sat;
}

/** "The indirect J2 effect".
 *  ref: GOCE standards GO-TN-HPF-GS-0111
 */
Vector3d accelJ2(
	const	double		C20, 			///< Value of the C20
	const	Matrix3d	eci2ecf,  		///< Rotation inertial to terestrila
			Vector3d	bodyPos,		///< Position of the planets
			double		GM)				///< Value of GM constant of the body in question
{
	Vector3d pos_ecef = eci2ecf * bodyPos;

	double dist = pos_ecef.norm();
	double term = (GM / pow(dist, 3)) * SQR(RE_WGS84 / dist);

	Vector3d vec = Vector3d::Zero();
	vec.head(2)	= term * (5 * SQR(pos_ecef.z()/dist) - 1) * pos_ecef.head(2);
	vec.z()		= term * (5 * SQR(pos_ecef.z()/dist) - 3) * pos_ecef.z();

	Vector3d accJ2 = -1 * ( (3 * sqrt(5)) / 2) * C20 * vec;

	return accJ2;
}

/** Compute the acceleration of due to a spherical harmonic field acting on the satellite
 * @note This function does not contain the degree 0 acceleration need to be done via "accelCentralForce"
 */
Vector3d accelSPH(
	const Vector3d	r,			///< Vector of the position of the satelite (ECEF)
	const MatrixXd	C, 			///< Matrix of the "C" spherical harmonic coefficient
	const MatrixXd	S,			///< Matrix of the "S" spherical harmonic coefficient
	const int		maxDeg, 	///< Maximum degree use for the summation of the harmonics	//todo aaron, limit this to max found in file/struct
	const double	GM)			///< Value of GM constant of the body in question.
{
	double R		= r.norm();
	double sin_lat	= r.z() / R; // Is Cos colat too.

	double Rxy		= sqrt(SQR(r.x()) + SQR(r.y()));
	double cos_lon	= r.x() / Rxy;
	double sin_lon	= r.y() / Rxy;

	VectorXd cosphi(maxDeg+1);
	VectorXd sinphi(maxDeg+1);

	cosphi(0) = 1;
	sinphi(0) = 0;

	cosphi(1) = cos_lon;
	sinphi(1) = sin_lon;

	for (int i = 2; i <= maxDeg; i++)
	{
		cosphi(i) =  cosphi(i-1) * cos_lon - sinphi(i-1) * sin_lon;
		sinphi(i) =  sinphi(i-1) * cos_lon + cosphi(i-1) * sin_lon;
	}

	Legendre leg(maxDeg);
	leg.calculate(sin_lat);

	double dVr		= 0;
	double dVtheta	= 0;
	double dVlambda	= 0;

	double const_Radius = RE_GLO / R;
	for (int i = 2; i <= maxDeg; i++)
	{
		const_Radius *= RE_GLO/R; /** (Re/R)**n : we start from deg 2 this formulation works. */
		double dVr_n		= 0;
		double dVtheta_n	= 0;
		double dVlambda_n	= 0;

		for (int j = 0; j <= i; j++)
		{
			dVr_n		+=		leg.Pnm(i,j)  * (C(i,j) * cosphi(j) + S(i,j) * sinphi(j));
			dVtheta_n	+= 		leg.dPnm(i,j) * (C(i,j) * cosphi(j) + S(i,j) * sinphi(j)); // lat
			dVlambda_n 	+= j *	leg.Pnm(i,j)  * (S(i,j) * cosphi(j) - C(i,j) * sinphi(j)); // lon
		}
		dVr 		+= -1*(i+1) * 	const_Radius * dVr_n;
		dVtheta 	+= 				const_Radius * dVtheta_n;
		dVlambda 	+= 				const_Radius * dVlambda_n;
	}

	//step 2, scale by GM/r; GM/r2 for dVr.

	double constant = GM / R;
	dVtheta		*= constant;
	dVlambda	*= constant;
	dVr			*= GM / SQR(R);

	// step3, project in cartesian

	Vector3d acc;
	acc.x() = dVr * r.x() / R + dVtheta * r.z() / 	SQR(R) * r.x() / Rxy - dVlambda * r.y() / SQR(Rxy);
	acc.y() = dVr * r.y() / R + dVtheta * r.z() / 	SQR(R) * r.y() / Rxy + dVlambda * r.x() / SQR(Rxy);
	acc.z() = dVr * r.z() / R - dVtheta * Rxy	/	SQR(R);

	return acc;
}


