
#pragma once

#include <iostream>
#include <fstream>
#include <utility>
#include <string>
#include <vector>
#include <memory>
#include <map>

using std::make_shared;
using std::shared_ptr;
using std::vector;
using std::string;
using std::pair;
using std::map;

#include <boost/log/trivial.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>

#include "enums.h"

#include "architectureDocs.hpp"

struct ReceiverMap;
struct KFState;

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

	TransitionMatrixObject()
	{

	}

	TransitionMatrixObject(
		const MatrixXd& rhs)
	{
		forwardTransitionMap.clear();
		rows = rhs.rows();
		cols = rhs.cols();

		for (int row = 0; row < rhs.rows(); row++)
		for (int col = 0; col < rhs.cols(); col++)
		{
			double transition = rhs(row,col);

			if (transition == 0)
			{
				continue;
			}

			forwardTransitionMap[{row, col}] = transition;
		}
	}

	TransitionMatrixObject(
		const SparseMatrix<double>& rhs)
	{
		forwardTransitionMap.clear();
		rows = rhs.rows();
		cols = rhs.cols();

		for (int k = 0; k < rhs.outerSize(); ++k)
		for (Eigen::SparseMatrix<double>::InnerIterator it(rhs, k); it; ++it)
		{
			double transition = it.value();

			if (transition == 0)
			{
				continue;
			}

			forwardTransitionMap[{it.row(), it.col()}] = transition;
		}
	}

	MatrixXd asMatrix()
	{
		MatrixXd transition = MatrixXd::Zero(rows, cols);

		for (auto& [keyPair, value] : forwardTransitionMap)
		{
			transition(keyPair.first, keyPair.second) = value;
		}

		return transition;
	}
};

using boost::serialization::serialize;
using boost::archive::binary_oarchive;
using boost::archive::binary_iarchive;


void spitFilterToFileQueued(
	shared_ptr<void>&	object_ptr,
	E_SerialObject		type,
	string				filename);

/** Output filter state to a file for later reading.
 * Uses a binary archive which requires all of the relevant class members to have serialization functions written.
 * Output format is TypeId, ObjectData, NumBytes - this allows seeking backward from the end of the file to the beginning of each object.
*/
template<class TYPE>
void spitFilterToFile(
	TYPE&			object,			///< Object to output
	E_SerialObject	type,			///< Type of object
	string			filename,		///< Path to file to output to
	bool			queue = false)	///< Optionally queue outputs in a separate thread
{
	DOCS_REFERENCE(Binary_Archive__);

	if (queue)
	{
		shared_ptr<void> copy_ptr = make_shared<TYPE>(object);

		spitFilterToFileQueued(copy_ptr, type, filename);

		return;
	}

	try
	{
		std::fstream fileStream(filename, std::ifstream::binary | std::ifstream::out | std::ifstream::app);

		if (!fileStream)
		{
			std::cout << "\n" << "Error opening algebra file '" << filename <<  "' for writing";
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
	catch (...)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: Writing to " << filename << " failed, drive may be full";
	}
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
		std::cout << "\n" << "Error opening algebra file " << filename <<  "for reading";
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
		std::cout << "\n" << "Error: Unexpected algebra file object type";
		return false;
	}

	serial & object;

	startPos = itemPosition;

	return true;
}

E_SerialObject getFilterTypeFromFile(
	long int&	startPos,
	string		filename);


void tryPrepareFilterPointers(
	KFState&		kfState,
	ReceiverMap&	receiverMap);

extern bool spitQueueRunning;
