
#include <iostream>
#include <fstream>
#include <map>

using std::map;

#include "eigenIncluder.hpp"

#include "algebraTrace.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "station.hpp"

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>

map<short int, string> idStringMap;
map<string, short int> stringIdMap;

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

	serial & itemDelta;

	long int itemPosition = currentPosition - itemDelta;

	fileStream.seekg(itemPosition, fileStream.beg);

	int typeInt;
	serial & typeInt;
	E_SerialObject type = E_SerialObject::_from_integral(typeInt);

	return type;
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

	serial & netKFState;

	int stationMapSize = stationMap.size();
	serial & stationMapSize;

	for (auto& [id, rec] : stationMap)
	{
		string tempId = id;
		serial & tempId;
		serial & rec.pppState;
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
		serial & kfState;

		KFState& destKFState = netKFState;

		destKFState.time		= kfState.time;
		destKFState.x			= kfState.x;
		destKFState.P			= kfState.P;
		destKFState.kfIndexMap	= kfState.kfIndexMap;

		//fix up the station pointers
		{
			map<KFKey, short int> newKFIndexMap;

			for (auto& [kfKey, index] : destKFState.kfIndexMap)
			{
				KFKey newKey = kfKey;
				string receiverId = kfKey.str;
				if (receiverId.empty() == false)
				{
					//get the appropriate station from the station map;
					newKey.rec_ptr = &stationMap[receiverId];
				}

				newKFIndexMap[newKey] = index;
			}
			destKFState.kfIndexMap = newKFIndexMap;
		}
	}

	int stationMapSize;
	serial & stationMapSize;

	for (int i = 0; i < stationMapSize; i++)
	{
		string tempId;
		serial & tempId;

		KFState kfState;
		serial & kfState;

		KFState& destKFState = stationMap[tempId].pppState;

		destKFState.time		= kfState.time;
		destKFState.x			= kfState.x;
		destKFState.P			= kfState.P;
		destKFState.kfIndexMap	= kfState.kfIndexMap;
	}
}



void tryPrepareFilterPointers(
	KFState&		kfState, 
	StationMap*		stationMap_ptr)
{
	if (stationMap_ptr == nullptr)
	{
		return;
	}
	
	auto& stationMap = *stationMap_ptr;
	
	map<KFKey, short> replacementKFIndexMap;
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		KFKey kfKey = key;
		
		if	(  kfKey.rec_ptr == nullptr
			&& kfKey.str.empty() == false)
		{
			auto it = stationMap.find(kfKey.str);
			if (it != stationMap.end())
			{
				auto& [id, station]	= *it;
				kfKey.rec_ptr	= &station;
			}
		}
		
		replacementKFIndexMap[kfKey] = index;
	}
	
	kfState.kfIndexMap = replacementKFIndexMap;
}
