
#include <iostream>
#include <fstream>
#include <map>

using std::map;

#include "eigenIncluder.hpp"

#include "algebraTrace.hpp"
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
