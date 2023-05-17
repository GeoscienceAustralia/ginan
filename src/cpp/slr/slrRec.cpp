
// #pragma GCC optimize ("O0")

#include "coordinates.hpp"
#include "constants.hpp"
#include "iers2010.hpp"
#include "common.hpp"
#include "sinex.hpp"
#include "slr.hpp"


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

/** Retrieve receiver a-priori position & eccentricity
*/
void getRecPosApriori(//todo aaron, remove this, use other function
	LObs&		obs,	///< SLR observation
	Station&	rec)	///< Receiver
{
	assert(obs.recCdpId >= 1000); //if fails, need to consider zero-padding in sinex files
		
	SinexRecData snx;
	auto result = getStnSnx(std::to_string(obs.recCdpId), obs.time, snx);
	if (result.failureSiteId)		{	BOOST_LOG_TRIVIAL(error) << "Station " << obs.recCdpId << " not found in sinex file";				obs.excludeSinex		= true;		return;	}
	if (result.failureEccentricity)	{	BOOST_LOG_TRIVIAL(error) << "Station " << obs.recCdpId << " eccentricity not found in sinex file";	obs.excludeEccentricity	= true;		return;	}
	if (result.failureEstimate)		{	BOOST_LOG_TRIVIAL(error) << "Station " << obs.recCdpId << " position not found in sinex file";		obs.excludeSinexPos		= true;		return;	}
	if (snx.ecc_ptr->rs != "UNE")
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: Station eccentricity referency system != UNE: " << snx.ecc_ptr->rs;	//todo aaron, this needs duplication elsewhere, rs unchecked
	}

	rec.aprioriPos = snx.pos;

	double yearsSinceRef = (obs.time - snx.refEpoch).to_double() / 86400.0 / 365.25;
	rec.aprioriPos += snx.vel * yearsSinceRef;

	if (snx.ecc_ptr == nullptr)
		return;
	auto eccEntry = *snx.ecc_ptr;

	if (eccEntry.rs == "UNE")
		rec.antDelta = eccEntry.ecc;
// 	else if (eccEntry.rs == "XYZ")
// 	{
// 		auto& pos = rec.pos
// 		ecef2pos(rec.aprioriPos, pos);
// 		
// 		rec.antDelta = ecef2enu(pos, eccEntry.ecc);
// 	}
	else
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Error: Station eccentricity referency system != UNE: " << eccEntry.rs;
	}
}

/** Calculate trop delay on SLR observation
*/
double getTropDelay(
	LObs&		obs,		///< SLR observation
	VectorPos&	pos,		///< Receiver position
	double		elevation)	///< Elevation of sat
{
	double waterVapourPressure = getWaterVapPressure(obs.temperature, obs.humidity);
	
	double ztd;
	double zhd;
	double zwd;
				iers2010::fcul_zd_hpa	(pos.latDeg(), pos.hgt(), obs.pressure, waterVapourPressure, obs.wavelength * 1e-3, ztd, zhd, zwd);
	double mf =	iers2010::fcul_a		(pos.latDeg(), pos.hgt(), obs.temperature, elevation * R2D);
	
	return ztd * mf;
}

/** Apply a-priori biases to observation
*/
void applyBiases(
	LObs&	obs)	///< SLR observation
{
	map<char, double> biases; 	// Indexed by "M" models codes - Ref: https://ilrs.dgfi.tum.de/fileadmin/data_handling/ILRS_Data_Handling_File.snx
	getRecBias(std::to_string(obs.recCdpId), obs.time, biases);

	obs.timeTx				-=  biases['T'] + biases['U'];
	obs.twoWayTimeOfFlight	-= (biases['R'] + biases['E']) / CLIGHT;
	obs.humidity			-=  biases['H'];
	obs.pressure			-=  biases['P'];
	
	if	( biases['X']
		||biases['N']
		||biases['Q']
		||biases['V'])
	{
		obs.excludeBias = true;
	}

	obs.time = obs.timeTx + obs.twoWayTimeOfFlight / 2;
}
