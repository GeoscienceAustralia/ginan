
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include <stdexcept>

#include "constants.hpp"
#include "common.hpp"

const double ura_eph[] =	///< URA values (ref [3] 20.3.3.3.1.1)
{
	2.4, 3.4, 4.85, 6.85, 9.65, 13.65, 24, 48, 96, 192, 384, 768, 1536, 3072, 6144
};

/** URA index to URA value (m)
* GLOBAL POSITIONING SYSTEM
* STANDARD POSITIONING SERVICE
* SIGNAL SPECIFICATION
* 2nd Ed, June 2,1995
* see section - 2.5.3 User Range Accuracy
*/
double svaToUra(int sva)
{
	double ura = 0;

	if (sva < 0)
		ura = -1;
	else if (sva <= 6)
	{
		ura = 10 * pow(2, 1 + ((double)sva / 2.0));
		ura = round(ura) / 10.0;
	}
	else if (sva < 15)
		ura = pow(2, (double)sva - 2.0);
	else
		ura = -1;

	return ura;
}

/** URA value (m) to URA index
*/
int uraToSva(double ura)
{
	int sva = 0;

	if	(ura < 0)
		sva = 15;
	else
	{
		for (sva = 0; sva < 15; sva++)
			if (ura_eph[sva] >= ura)
				break;
	}

	return sva;
}

/** Galileo SISA index to SISA value (m)
* EUROPEAN GNSS (GALILEO) OPEN SERVICE SIGNAL-IN-SPACE INTERFACE CONTROL DOCUMENT
* Issue 2.0, January 2021
* See Section, 5.1.12. Signal In Space Accuracy (SISA)
*/
double svaToSisa(int sva)
{
	if		(sva <  0)		return -1;
	else if	(sva <= 49)		return (0.0 + (sva - 0) * 0.01);
	else if	(sva <= 74)		return (0.5 + (sva - 50) * 0.02);
	else if	(sva <= 99)		return (1.0 + (sva - 75) * 0.04);
	else if	(sva <= 125)	return (2.0 + (sva - 100) * 0.16);
	else					return -1;
}

/** Galileo SISA value (m) to SISA index
*/
int sisaToSva(double sisa)
{
	if		(sisa <  0)		return 255;
	else if	(sisa <= 0.49)	return (int)((((sisa - 0.0) / 0.01) + 0) + 0.5);
	else if	(sisa <= 0.98)  return (int)((((sisa - 0.5) / 0.02) + 50) + 0.5);
	else if	(sisa <= 1.96)  return (int)((((sisa - 1.0) / 0.04) + 75) + 0.5);
	else if	(sisa <= 6.00)  return (int)((((sisa - 2.0) / 0.16) + 100) + 0.5);
	else					return 255;
}

/** crc-24q parity
* compute crc-24q parity for sbas, rtcm3
* see reference [2] A.4.3.3 Parity
*/
unsigned int crc24q(
	const unsigned char*	buff,	///< data
	int						len)	///< data length (bytes)
{
//	trace(4,"%s: len=%d\n",__FUNCTION__, len);

	unsigned int crc = 0;

	for (int i = 0; i < len; i++)
		crc = ((crc<<8) & 0xFFFFFF) ^ tbl_CRC24Q[(crc >> 16) ^ buff[i]];

	return crc;
}

/** Wrap angle between (-pi, pi]
 */
void wrapPlusMinusPi(
	double&	angle)	///< Angle to wrap
{
	while (angle <= -PI)	angle += 2 * PI;
	while (angle >	 PI)	angle -= 2 * PI;
}

/** Wrap angle between [0, 2pi)
 */
void wrap2Pi(
	double&	angle)	///< Angle to wrap
{
	while (angle <  0)		angle += 2 * PI;
	while (angle >= 2 * PI)	angle -= 2 * PI;
}


double	VectorPos::latDeg()	const	{		return x() * R2D;	}
double	VectorPos::lonDeg()	const	{		return y() * R2D;	}
