#include "tropModels.hpp"

#define ERR_SAAS	0.3			///< saastamoinen model error std (m)

const double coefNMF[][5] =
{
	{ 1.2769934E-3, 1.2683230E-3, 1.2465397E-3, 1.2196049E-3, 1.2045996E-3},
	{ 2.9153695E-3, 2.9152299E-3, 2.9288445E-3, 2.9022565E-3, 2.9024912E-3},
	{ 62.610505E-3, 62.837393E-3, 63.721774E-3, 63.824265E-3, 64.258455E-3},

	{ 0.0000000E-0, 1.2709626E-5, 2.6523662E-5, 3.4000452E-5, 4.1202191E-5},
	{ 0.0000000E-0, 2.1414979E-5, 3.0160779E-5, 7.2562722E-5, 11.723375E-5},
	{ 0.0000000E-0, 9.0128400E-5, 4.3497037E-5, 84.795348E-5, 170.37206E-5},

	{ 5.8021897E-4, 5.6794847E-4, 5.8118019E-4, 5.9727542E-4, 6.1641693E-4},
	{ 1.4275268E-3, 1.5138625E-3, 1.4572752E-3, 1.5007428E-3, 1.7599082E-3},
	{ 4.3472961E-2, 4.6729510E-2, 4.3908931E-2, 4.4626982E-2, 5.4736038E-2}
};

double interpc(const double coef[], double lat)
{
	int i = (int)(lat / 15);

	if		(i < 1) 		return coef[0];
	else if (i > 4) 		return coef[4];

	return coef[i - 1] * (1 - lat / 15 + i) + coef[i] * (lat / 15 - i);
}

/* troposphere model -----------------------------------------------------------
* compute tropospheric delay by standard atmosphere (relative humidity of 0.7
* 15 degrees of temperature at sea level) and saastamoinen model
* Neill Mapping functions ia used for mapping
* args   : gtime_t time     I   time
*          double *pos      I   receiver position {lat,lon,h} (rad,m)
* return : tropospheric delay (m)
*-----------------------------------------------------------------------------*/
double tropSAAS(
	Trace&		trace,
	GTime		time,
	VectorPos&	pos,
	double		el,
	double&		dryZTD,
	double&		dryMap,
	double&		wetZTD,
	double&		wetMap,
	double&		var)
{
	double lat = pos.latDeg();
	double hgt = pos.hgt();

	if	(hgt < -100
		|| hgt	> +20000
		|| el	<  0)
	{
		dryMap = 0;
		wetMap = 0;
		dryMap = 0;
		wetMap = 0;
		var = SQR(ERR_TROP);
		return 0;
	}

	UYds yds = time;
	/* year from doy 28, added half a year for southern latitudes */
	double y = (yds.doy - 28) / 365.25 + (lat < 0 ? 0.5 : 0);
	double cosy = cos(2 * PI * y);
	lat = fabs(lat);

	double ah[3];
	double aw[3];
	for (int i = 0; i < 3; i++)
	{			/* year average           +    seasonal variation  */
		ah[i] = interpc(coefNMF[i    ], lat) - interpc(coefNMF[i + 3], lat) * cosy;
		aw[i] = interpc(coefNMF[i + 6], lat);
	}
	/* height correction */
	double dm = (1 / sin(el) - mapHerring(el, 2.53E-5, 5.49E-3, 1.14E-3)) * hgt / 1E3;
	dryMap = mapHerring(el, ah[0], ah[1], ah[2]) + dm;
	wetMap = mapHerring(el, aw[0], aw[1], aw[2]);

	double temp = 15 - 6.5E-3 * hgt + ZEROC;
	double pres	= 1013.25 * pow(288.15 / temp, -5.255877);
	double e = 6.108 * 0.7 * exp((17.15 * temp - 4684) / (temp - 38.45));

	dryZTD = 0.0022768 * pres / (1 - 0.00266 * cos(2 * pos.lat()) - 0.00028 * hgt / 1E3);
	wetZTD = 0.002277 * (1255 / temp + 0.05) * e;

	var = SQR(ERR_SAAS / (sin(el) + 0.1));

	return dryMap * dryZTD + wetMap * wetZTD;
}
