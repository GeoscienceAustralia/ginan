
#include <iostream>
#include <fstream>
#include <map>

using std::map;

#include "eigenIncluder.hpp"

#include "peaCommitVersion.h"
#include "algebraTrace.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "station.hpp"

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>

/** Returns the type of object that is located at the specified position in a file
*/
E_SerialObject getFilterTypeFromFile(
	long int& startPos,	///< Position of object
	string filename)	///< Path to archive file
{
	std::fstream fileStream(filename, std::ifstream::binary | std::ifstream::in);

	if (!fileStream)
	{
		return E_SerialObject::NONE;
	}

	binary_iarchive serial(fileStream, 1); //no header

	long int itemDelta;

	fileStream.seekg (0, fileStream.end);
	long int fileSize = fileStream.tellg();

	if (startPos < 0)	{	fileStream.seekg(			-sizeof(itemDelta),	fileStream.end);	}
	else				{	fileStream.seekg(startPos	-sizeof(itemDelta),	fileStream.beg);	}

	long int currentPosition = fileStream.tellg();
	if	( (currentPosition >= fileSize)
		||(currentPosition <  0))
	{
		return E_SerialObject::NONE;
	}

	serialize(serial, itemDelta);

	long int itemPosition = currentPosition - itemDelta;

	fileStream.seekg(itemPosition, fileStream.beg);

	int type_int;
	serialize(serial, type_int);
	E_SerialObject type = E_SerialObject::_from_integral(type_int);

	return type;
}

/** Initialises the outputs for input/output of filter states during (re)processing
*/
void initFilterTrace(
	KFState&	kfState,		///< Filter object to apply to
	string		traceFilename,	///< Filename to store/read data from
	string		stationId,		///< Station description for top line of the trace file
	int			rts_lag)		///< Number of epochs to lag when doing RTS smoothing (-ve => complete reverse)
{
	kfState.rts_filename			= traceFilename;
	kfState.rts_forward_filename	= traceFilename + FORWARD_SUFFIX;
	kfState.rts_lag					= rts_lag;

	//remove logtime from forward file.
	replaceString(kfState.rts_forward_filename, "<LOGTIME>", "");

	std::ofstream ofs1(kfState.rts_forward_filename,			std::ofstream::out | std::ofstream::trunc);
}


void outputPersistanceNav()
{
	string navFilename = acsConfig.persistance_filename + "_nav";

	std::fstream fileStream(navFilename, std::ifstream::binary | std::ifstream::out | std::ifstream::app);

	if (!fileStream)
	{
		std::cout << std::endl << "Error opening persistance file " << navFilename <<  " for writing";
		return;
	}

	binary_oarchive serial(fileStream, 1);	//no header

	serialize(serial, nav);
}

void inputPersistanceNav()
{
	string navFilename = acsConfig.persistance_filename + "_nav";

	std::fstream fileStream(navFilename, std::ifstream::binary | std::ifstream::in);

	if (!fileStream)
	{
		std::cout << std::endl << "Error opening persistance file " << navFilename <<  " for input";
		return;
	}

	binary_iarchive serial(fileStream, 1); //no header

	serialize(serial, nav);
}

void outputPersistanceStates(
	map<string, Station>&	stationMap,
	KFState&				netKFState)
{
	string stateFilename	= acsConfig.persistance_filename + "_states";

	std::fstream fileStream(stateFilename, std::ifstream::binary | std::ifstream::out | std::ifstream::app);

	if (!fileStream)
	{
		std::cout << std::endl << "Error opening persistance file " << stateFilename <<  " for writing";
		return;
	}

	binary_oarchive serial(fileStream, 1);	//no header

	serialize(serial, netKFState);

	int stationMapSize = stationMap.size();
	serialize(serial, stationMapSize);

	for (auto& [id, station] : stationMap)
	{
		string tempId = id;
		serialize(serial, tempId);
		serialize(serial, station.rtk.pppState);
	}
}

void inputPersistanceStates(
	map<string, Station>&	stationMap,
	KFState&				netKFState)
{
	string stateFilename	= acsConfig.persistance_filename + "_states";

	std::fstream fileStream(stateFilename, std::ifstream::binary | std::ifstream::in);

	if (!fileStream)
	{
		std::cout << std::endl << "Error opening persistance file " << stateFilename <<  " for input";
		return;
	}

	binary_iarchive serial(fileStream, 1); //no header

	{
		KFState kfState;
		serialize(serial, kfState);

		KFState& destKFState = netKFState;

		destKFState.time		= kfState.time;
		destKFState.x			= kfState.x;
		destKFState.P			= kfState.P;
		destKFState.kfIndexMap	= kfState.kfIndexMap;

		//special case for netKFState - fix up the station pointers
		{
			map<KFKey, short int> newKFIndexMap;

			for (auto& [kfKey, index] : destKFState.kfIndexMap)
			{
				KFKey newKey = kfKey;
				string receiverId = kfKey.str;
				if (receiverId.empty() == false)
				{
					//get the appropriate station from the station map;
					newKey.station_ptr = &stationMap[receiverId];
				}

				newKFIndexMap[newKey] = index;
			}
			destKFState.kfIndexMap = newKFIndexMap;
		}
	}



	int stationMapSize;
	serialize(serial, stationMapSize);

	for (int i = 0; i < stationMapSize; i++)
	{
		string tempId;
		serialize(serial, tempId);

		KFState kfState;
		serialize(serial, kfState);

		KFState& destKFState = stationMap[tempId].rtk.pppState;

		destKFState.time		= kfState.time;
		destKFState.x			= kfState.x;
		destKFState.P			= kfState.P;
		destKFState.kfIndexMap	= kfState.kfIndexMap;
	}
}
