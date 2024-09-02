
// #pragma GCC optimize ("O0")

#include "observations.hpp"
#include "coordinates.hpp"
#include "tropModels.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "receiver.hpp"
#include "common.hpp"
#include "sinex.hpp"
#include "slr.hpp"

map<string, vector<string>>						slrObsFiles;
map<string, map<GTime, shared_ptr<LObs>>>		slrSiteObsMap;

/** Calculates Water Vapour pressure (hPa)(mbar) using Buck's equation
 * Ref: https://en.wikipedia.org/wiki/Arden_Buck_equation
 */
double getWaterVapPressure(
	double	temperature,	///< Air temperature (K)
	double	humidity)		///< Percentage humidity (as decimal)
{
	assert(humidity <= 1);

	double temperatureC					= temperature - 273.15; // K to C
	double saturationVaporPressureHpa	= 6.1121 * exp((18.678 - temperatureC / 234.5) * (temperatureC / (257.14 + temperatureC))); //hPa

	return saturationVaporPressureHpa * humidity;
}

/** Calculate trop delay on SLR observation
*/
double laserTropDelay(
	LObs&			obs,		///< SLR observation
	VectorPos&		pos,		///< Receiver position
	AzEl&			azel,		///< Azimuth/Elevation of sat (azimuth for future use)
	TropStates&		tropStates,	///< (gradients for future use)
	TropMapping&	dTropDx,	///< (gradients for future use)
	double&			var)
{
	double temperature	= obs.temperature;
	double humidity		= obs.humidity - obs.humidityBias;
	double pressure		= obs.pressure - obs.pressureBias;
	double wavelengthUm	= obs.wavelengthNm * 1e-3;

	double waterVapourPressure = getWaterVapPressure(temperature, humidity);

	double ztd;
	double zhd;
	double zwd;
				iers2010::fcul_zd_hpa	(pos.latDeg(), pos.hgt(), pressure, waterVapourPressure, wavelengthUm, ztd, zhd, zwd);
	double mf =	iers2010::fcul_a		(pos.latDeg(), pos.hgt(), temperature, azel.el * R2D);

	tropStates.zenith	= ztd;
	dTropDx.dryMap		= mf;
	dTropDx.wetMap		= mf;

	return ztd * mf;
}

/** Update a-priori biases
* Don't corrected for raw observations directly in LObs struct
*/
void updateSlrRecBiases(
	LObs&				obs)
{
	map<char, double> biasMap;		// Indexed by "M" models codes - Ref: https://ilrs.gsfc.nasa.gov/docs/2024/ILRS_Data_Handling_File_2024.02.13.snx

	getSlrRecBias(std::to_string(obs.recCdpId), obs.Sat, obs.time, biasMap);	// Always do this to allow checking excluded data

	obs.rangeBias		= biasMap['R'] + biasMap['E'];
	obs.timeBias		= biasMap['T'] + biasMap['U'];
	obs.humidityBias	= biasMap['H'];
	obs.pressureBias	= biasMap['P'];

	if	( biasMap['X']
		||biasMap['N']
		||biasMap['Q']
		||biasMap['V'])
	{
		obs.excludeDataHandling = true;
	}
}
