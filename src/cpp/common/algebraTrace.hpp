
#pragma once

#include <iostream>
#include <utility>
#include <string>
#include <vector>
#include <map>

using std::vector;
using std::string;
using std::pair;
using std::map;

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>

#include "navigation.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "enum.h"

/** Types of objects that are stored in kalman filter binary archives
*/
BETTER_ENUM(E_SerialObject,		int,
			NONE,
			FILTER_MINUS,
			FILTER_PLUS,
			FILTER_SMOOTHED,
			TRANSITION_MATRIX,
			NAVIGATION_DATA,
			STRING,
			MEASUREMENT,
			METADATA
)

struct TransitionMatrixObject
{
	map<pair<int, int>, double>		forwardTransitionMap;
	int								rows;
	int								cols;
	
	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, const unsigned int& version)
	{
		ar & forwardTransitionMap;
		ar & rows;
		ar & cols;
	}
};

extern map<short int, string> idStringMap;
extern map<string, short int> stringIdMap;
	

using boost::serialization::serialize;
using boost::archive::binary_oarchive;
using boost::archive::binary_iarchive;

/** Output filter state to a file for later reading.
 * Uses a binary archive which requires all of the relevant class members to have serialization functions written.
 * Output format is TypeId, ObjectData, NumBytes - this allows seeking backward from the end of the file to the beginning of each object.
*/
template<class TYPE>
void spitFilterToFile(
	TYPE&			object,		///< Object to output
	E_SerialObject	type,		///< Type of object
	string			filename)	///< Path to file to output to
{
	std::fstream fileStream(filename, std::ifstream::binary | std::ifstream::out | std::ifstream::app);

	if (!fileStream)
	{
		std::cout << std::endl << "Error opening algebra file '" << filename <<  "' for writing";
		return;
	}

// 	std::cout << "RTS - writing " << type._to_string() << " to file " << filename << "\n";
	
	binary_oarchive serial(fileStream, 1);	//no header

	long int pos = fileStream.tellp();

	int type_int = type;
	serial & type_int;
	serial & object;

	long int end = fileStream.tellp();
	long int delta = end - pos;
	serial & delta;
}

/* Retrieve an object from an archive
*/
template<class TYPE>
bool getFilterObjectFromFile(
	E_SerialObject	expectedType,	///< The expected type of object, (determine using getFilterTypeFromFile() first)
	TYPE&			object,			///< The pre-declared object to set the value of
	long int&		startPos,		///< The position in the file of the object's record
	string			filename)		///< The path to the archive file to read from
{
	std::fstream fileStream(filename, std::ifstream::binary | std::ifstream::in);

	if (!fileStream)
	{
		std::cout << std::endl << "Error opening algebra file " << filename <<  "for reading";
		return false;
	}

	binary_iarchive serial(fileStream, 1); //no header

	long int itemDelta;

	if (startPos < 0)	{	fileStream.seekg(			-sizeof(itemDelta),	fileStream.end);	}
	else				{	fileStream.seekg(startPos	-sizeof(itemDelta),	fileStream.beg);	}

	long int currentPosition = fileStream.tellg();

	serial & itemDelta;

	long int itemPosition = currentPosition - itemDelta;

	fileStream.seekg(itemPosition, fileStream.beg);

	int typeInt;
	serial & typeInt;

	E_SerialObject type = E_SerialObject::_from_integral(typeInt);
	if (type != expectedType)
	{
		std::cout << std::endl << "Error: Unexpected algebra file object type";
		return false;
	}

	serial & object;

	startPos = itemPosition;

	return true;
}

E_SerialObject getFilterTypeFromFile(
	long int&	startPos,
	string		filename);


void inputPersistanceNav();

void outputPersistanceNav();

void inputPersistanceStates(
	map<string, Station>&	stationMap,
	KFState&				netKFState);

void outputPersistanceStates(
	map<string, Station>&	stationMap,
	KFState&				netKFState);

void tryPrepareFilterPointers(
	KFState&		kfState, 
	StationMap*		stationMap_ptr);
