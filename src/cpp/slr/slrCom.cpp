#include <iostream>
#include <sstream>
#include <fstream>
#include <cmath>
#include <map>

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "coordinates.hpp"
#include "navigation.hpp"
#include "planets.hpp"
#include "common.hpp"
#include "tides.hpp"
#include "sinex.hpp"
#include "enums.h"
#include "slr.hpp"

using namespace boost::algorithm;

constexpr int MIN_LINE_LEN = 64;

SphericalComMap	sphericalComMap;


/** Read SLR spherical satellite centre-of-mass (CoM) file
*/
void readCom(
	string	filepath)	///< Path to CoM file
{
	std::ifstream comFile(filepath);
	if (!comFile)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error: could not read in CoM file " << filepath;
		return;
	}

	// Extract name between last '_' and following '.'
	// e.g. filepath = "path/to/com_lageos.txt"
	vector<string> usFields;
	vector<string> dotFields;
	boost::split(usFields,	filepath,			boost::is_any_of("_"));
	boost::split(dotFields,	usFields.back(),	boost::is_any_of("."));
	string satName = to_lower_copy(dotFields.front());

	string line;
	while (std::getline(comFile, line))
	{
		if (line.size() < MIN_LINE_LEN)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: line size is too short: " << line.size();
			continue;
		}

		// Get all the values from the file
		GEpoch startEp	= {std::stod(line.substr(11,4)), std::stod(line.substr(8, 2)),	std::stod(line.substr(5,2)),	0, 0, 0};
		GEpoch endEp	= {std::stod(line.substr(22,4)), std::stod(line.substr(19,2)),	std::stod(line.substr(16,2)),	0, 0, 0};
		string recId = line.substr(0, 4);
		double comValue = std::stoi(line.substr(60, 4)) * 1e-3; // mm->m

		// Convert to gps time
		GTime start	= startEp;
		GTime end	= endEp;

		// Initialise the centre of mass struct
		SphericalCom newCom;
		newCom.comValue		= comValue;
		newCom.startTime	= start;
		newCom.endTime		= end;
		sphericalComMap[satName][recId][start] = newCom;
	}
}

/** Strip numbers from string
*/
string stripNumbers(
	string	str)	///< String to strip
{
	str.erase(std::remove_if(str.begin(), str.end(), [](unsigned char x) { return std::isdigit(x) || std::isspace(x); }), str.end());

	return str;
}

/** Check if sat is spherical (i.e. listed in spherical CoM map)
*/
bool isSpherical(
	string satName)
{
	auto satFind = sphericalComMap.find(stripNumbers(satName)); // sphericalComMap indexed by sat ID
	if (satFind == sphericalComMap.end())
	{
		return false;
	}

	return true;
}

/** Center of mass to laser retroreflector array offset for spherical satellites
*/
double satComOffSphere(
	LObs&		obs)		///< SLR observation
{
	string satName	= stripNumbers(obs.satName);
	string recId	= std::to_string(obs.recCdpId);

	auto it = sphericalComMap[satName][recId].lower_bound(obs.time);
	if (it != sphericalComMap[satName][recId].end())
	{
		auto& [dummy, comEntry] = *it;
		if (comEntry.endTime > obs.time)
		{
			return comEntry.comValue;
		}
	}

	if (acsConfig.require_reflector_com)
	{
		BOOST_LOG_TRIVIAL(warning)	<< "Warning: Observation rejected due to lack of laser retroreflector COM correction";

		obs.excludeCom = true;
	}
	else
	{
		BOOST_LOG_TRIVIAL(warning)	<< "Warning: Laser retroreflector COM correction not found";
	}

	return 0;
}

/** Center of mass to laser retroreflector array offset for GNSS satellites
*/
VectorEcef satComOffGnss(
	LObs&		obs)	///< SLR observation
{
	GTime time = obs.time;

	SinexSatSnx	satSnx;
	auto result = getSatSnx(obs.Sat, time, satSnx);
	if	(!result.failure
		&&obs.satNav_ptr != nullptr)
	{
		auto& satNav = *obs.satNav_ptr;

		Vector3d ecc	= satSnx.ecc_ptrs[E_EccType::L_LRA]->ecc
						- satSnx.com;

		VectorEcef comOffset	= body2ecef(satNav.attStatus, ecc);

		return comOffset;
	}

	if (acsConfig.require_reflector_com)
	{
		BOOST_LOG_TRIVIAL(warning)	<< "Warning: Observation rejected due to lack of laser retroreflector COM correction";

		obs.excludeCom = true;
	}
	else
	{
		BOOST_LOG_TRIVIAL(warning)	<< "Warning: Laser retroreflector COM correction not found";
	}

	return VectorEcef();
}
