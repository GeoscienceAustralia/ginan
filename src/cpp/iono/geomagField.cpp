
/** References
* 1. P.Alken, E.Thébault, C.D.Beggan, H.Amit, J.Aubert, J.Baerenzung, T.N.Bondar, W.J.Brown, S.Califf, A.Chambodut & A.Chulliat, International geomagnetic reference field: the thirteenth generation. Earth, Planets and Space, 2021.
* 2. E.Thébault, C.C.Finlay, C.D.Beggan, P.Alken, J.Aubert, O.Barrois, F.Bertrand, T.Bondar, A.Boness, L.Brocco & E.Canet. International geomagnetic reference field: the 12th generation. Earth, Planets and Space, 2015.
* 3. D.E.Winch, D.J.Ivers, J.P.R.Turner & R.J.Stening. Geomagnetism and Schmidt quasi-normalization. Geophysical Journal International, 2005.
* 4. Wikipedia, Spherical harmonics. https://en.wikipedia.org/wiki/Spherical_harmonics#Orthogonality_and_normalization
*/


#include <iostream>
#include <fstream>
#include <vector>

using std::vector;

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "acceleration.hpp"
#include "coordinates.hpp"
#include "geomagField.hpp"
#include "constants.hpp"
#include "common.hpp"

map<short int, GeomagMainField>	igrfMFMap;	///< igrfMFMap[year]
GeomagSecularVariation			igrfSV;

/** Read the IGRF file
* Only support the latest generations of IGRF file from IGRF-11
*/
void readIGRF(
	string	filename)			///< IGRF file to read
{
	if (filename.empty())
	{
		return;
	}

	std::ifstream infile(filename);
	if (!infile)
	{
		return;
	}

	string line;
	vector<int>		yearList;
	while (std::getline(infile, line))
	{
		if (line[0] == '#')
		{
			continue;
		}

		vector<string> split;
		boost::trim(line);
		boost::algorithm::split(split, line, boost::algorithm::is_space(), boost::token_compress_on);

		if		(split[0] == "g/h")
		{
			int i;
			for (i = 3; i < split.size()-1; i++)
			{
				int		year			= stoi(split[i]);
				int		maxDegree		= 13;
				bool	isDefinitive	= false;

				if	(year <= 1995)
					maxDegree = 10;

				GeomagMainField& igrfMF	= igrfMFMap[year];
				igrfMF.year				= year;
				igrfMF.maxDegree		= maxDegree;

				yearList.push_back(year);
			}

			// SV
			igrfSV.year			= stoi(split[i].substr(0,4));
			igrfSV.yearEnd		= stoi(split[i].substr(5,2)) + 2000;
			igrfSV.maxDegree	= 8;
		}
		else if	(  split[0] == "g"
				&& split.size() > 3)
		{
			int n = stoi(split[1]);
			int m = stoi(split[2]);

			int i;
			for (i = 3; i < split.size()-1; i++)
			{
				int year 	= yearList[i-3];
				auto& gnm	= igrfMFMap[year].gnm;
				gnm(n, m)	= stod(split[i]);
			}

			// SV
			igrfSV.gnm(n, m)	= stod(split[i]);
		}
		else if	(  split[0] == "h"
				&& split.size() > 3)
		{
			int n = stoi(split[1]);
			int m = stoi(split[2]);

			int i;
			for (i = 3; i < split.size()-1; i++)
			{
				int year	= yearList[i-3];
				auto& hnm	= igrfMFMap[year].hnm;
				hnm(n, m)	= stod(split[i]);
			}

			// SV
			igrfSV.hnm(n, m)	= stod(split[i]);
		}
	}
}

/** Convert time to decimal year
* For example: 2019-07-18 00:00:00 -> 2019.54247
*/
double decimalYear(
	GTime	time)					///< time
{
	GEpoch ep  = time;
	GEpoch ep0 = {ep.year,   1, 1, 0, 0, 0};
	GEpoch ep1 = {ep.year+1, 1, 1, 0, 0, 0};

	double secOfYear = ((GTime)ep  - (GTime)ep0).to_double();
	double secInYear = ((GTime)ep1 - (GTime)ep0).to_double();

	return ep.year + secOfYear/secInYear;
}

/** Calculate the SH coefficients at given epoch based on a piecewise linear model
* For years starting in 1900 and ending in 2020, the change rates should be calculated using the two nearest data points
* For the final 5 years of model validity (2020 to 2025 for IGRF-13), the Secular Variation coefficients are explicitly provided
* See ref [1]
*/
bool getSHCoef(
	GTime				time,	///< time of IGRF coefficients to calculate
	GeomagMainField&	igrfMF)	///< map of the IGRF coefficients calculated
{
	if (igrfMFMap.empty())
		return false;

	double year	= decimalYear(time);

	if	( year >= igrfSV.yearEnd
		||year <  1900)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Input epoch " << time.to_string() << " out of range covered by the IGRF file: 1900-" << igrfSV.yearEnd;
		return false;
	}

	int year0	= ((int)year / 5) * 5;
	int year1	= year0 + 5;
	double dt	= year - year0;

	MatrixXd gnmDot	= MatrixXd::Zero(14, 14);
	MatrixXd hnmDot	= MatrixXd::Zero(14, 14);
	if (year >= igrfSV.year)
	{
		gnmDot	= igrfSV.gnm;
		hnmDot	= igrfSV.hnm;
	}
	else
	{
		gnmDot	= (igrfMFMap[year1].gnm - igrfMFMap[year0].gnm) / 5.0;
		hnmDot	= (igrfMFMap[year1].hnm - igrfMFMap[year0].hnm) / 5.0;
	}

	igrfMF.year			= year;
	igrfMF.maxDegree	= igrfMFMap[year0].maxDegree;
	igrfMF.gnm			= igrfMFMap[year0].gnm + gnmDot * dt;
	igrfMF.hnm			= igrfMFMap[year0].hnm + hnmDot * dt;

	return true;
}

/** Compute the geomagnetic field components in geocentric local reference system at given time and position
* See ref [1] [2]
* Note that the normalisation of associated Legendre functions for Geomagnetic field is different from that for Geogravitational field, i.e.
*      Pnm_gra_norm(x) = sqrt((2-dm0)*(2*n+1) *	(n-m)/(n+m)) * Pnm(x)
*      Pnm_mag_norm(x) = sqrt((2-dm0)*			(n-m)/(n+m)) * Pnm(x)
* where dm0 is the Kronecker delta which is equal to 1 for m==0 and 0 for m!=0
* Thus we have Pnm_gra_norm(x) = sqrt(2*n+1) * Pnm_mag_norm(x)
* See ref [3] [4]
*/
VectorEnu getGeomagIntensity(
	GTime				time,		///< time of geomagnetic field to model
	const VectorPos&	pos)		///< position in geocentric spherical coordinates where geomagnetic field to model
{
	VectorEnu intensityEnu;

	GeomagMainField igrfMF;

	bool pass = getSHCoef(time, igrfMF);
	if (pass == false)
	{
		return intensityEnu;
	}

	double lat		= pos.lat();
	double lon		= pos.lon();
	double radius	= pos[2];

	const double maxLat	= PI/2 - 1E-6;

	// resolve singularity
	if		(pos.lat() > +maxLat)		{	lat = +maxLat;	}
	else if	(pos.lat() < -maxLat)		{	lat = -maxLat;	}

	double cosTheta	= sin(lat);	// theta is co-latitude
	double sinTheta	= cos(lat);
	double cosPhi	= cos(lon);
	double sinPhi	= sin(lon);

	VectorXd cosmPhi(igrfMF.maxDegree+1);
	VectorXd sinmPhi(igrfMF.maxDegree+1);
	cosmPhi(0) = 1;
	sinmPhi(0) = 0;
	cosmPhi(1) = cosPhi;
	sinmPhi(1) = sinPhi;
	for (int m = 2; m <= igrfMF.maxDegree; m++)
	{
		cosmPhi(m) = cosmPhi(m-1) * cosPhi - sinmPhi(m-1) * sinPhi;
		sinmPhi(m) = sinmPhi(m-1) * cosPhi + cosmPhi(m-1) * sinPhi;
	}

	Legendre leg(igrfMF.maxDegree);
	leg.calculate(cosTheta);	// Pnm_gra_norm(cos(theta))

	double Btheta	= 0;
	double Bphi		= 0;
	double Br		= 0;

	double radiusRatio = SQR(RE_IGRF / radius);
	for (int n = 1; n <= igrfMF.maxDegree; n++)
	{
		radiusRatio *= RE_IGRF / radius;	// (a/r)**n+2

		double dVtheta	= 0;
		double dVphi	= 0;
		double dVr		= 0;

		double factor	= 1 / sqrt(2 * n + 1);	// factor converting Pnm_gra_norm(x) to Pnm_mag_norm(x)
		for (int m = 0; m <= n; m++)
		{
			dVtheta	+= 		leg.dPnm(n, m) * (igrfMF.gnm(n, m) * cosmPhi(m) + igrfMF.hnm(n, m) * sinmPhi(m));	// lat
			dVphi	+= m *	leg. Pnm(n, m) * (igrfMF.hnm(n, m) * cosmPhi(m) - igrfMF.gnm(n, m) * sinmPhi(m));	// lon
			dVr		+= 		leg. Pnm(n, m) * (igrfMF.gnm(n, m) * cosmPhi(m) + igrfMF.hnm(n, m) * sinmPhi(m));	// r
		}

		Btheta	+=			factor * radiusRatio * dVtheta;
		Bphi	+=			factor * radiusRatio * dVphi;
		Br		+= -(n+1) *	factor * radiusRatio * dVr;
	}

	intensityEnu.e() = -Bphi / sinTheta;
	intensityEnu.n() = +Btheta;
	intensityEnu.u() = -Br;

	return intensityEnu;
}

/** Compute the geomagnetic field components in ECEF reference system at given time and position
*/
VectorEcef getGeomagIntensityEcef(
	GTime				time,		///< time of geomagnetic field to model
	const VectorPos&	pos)		///< position in geocentric spherical coordinates where geomagnetic field to model
{
	VectorEnu	intensityEnu	= getGeomagIntensity(time, pos);

	VectorEcef	intensityEcef	= enu2ecef(pos, intensityEnu);

	return intensityEcef;
}
