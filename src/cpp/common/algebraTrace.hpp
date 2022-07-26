
#ifndef __ALGEBRA_TRACE_HPP__
#define __ALGEBRA_TRACE_HPP__

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
#include "algebra.hpp"
#include "enum.h"

/** Types of objects that are stored in kalman filter binary archives
*/
BETTER_ENUM(E_SerialObject,		int,
			NONE,
			FILTER_MINUS,
			FILTER_PLUS,
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
};

typedef map<pair<KFKey, KFKey>, double>	CovarAdjustObject;
typedef map<KFKey, double>				StateAdjustObject;

namespace boost::serialization
{
	template<class ARCHIVE>    void serialize(ARCHIVE& ar, int&					integer)    {ar & integer;    	}
	template<class ARCHIVE>    void serialize(ARCHIVE& ar, long int&			integer)    {ar & integer;    	}
	template<class ARCHIVE>    void serialize(ARCHIVE& ar, short int&			integer)    {ar & integer;    	}
	template<class ARCHIVE>    void serialize(ARCHIVE& ar, size_t&				size_type)	{ar & size_type; 	}
	template<class ARCHIVE>    void serialize(ARCHIVE& ar, map<int, double>&	Map)		{ar & Map;			}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, VectorXd& vec)
	{
		int rows = vec.rows();
		ar & rows;

		vec.resize(rows);

		for (int row = 0; row < vec.rows(); row++)
		{
			ar & vec(row);
		}
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, MatrixXd& mat)
	{
		int rows = mat.rows();
		int cols = mat.cols();

		ar & rows;
		ar & cols;

		mat.resize(rows, cols);

		for (int row = 0; row < mat.rows(); row++)
		for (int col = 0; col < mat.cols(); col++)
		{
			ar & mat(row, col);
		}
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, GTime& time)
	{
		long int	time_int = time.time;
		double		time_sec = time.sec;
		ar & time_int;
		ar & time_sec;
		time.time	= time_int;
		time.sec	= time_sec;
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, E_Sys& sys)
	{
		short int sys_int = sys;
		ar & sys_int;
		sys = E_Sys::_from_integral(sys_int);
	}


	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, SatSys& Sat)
	{
		serialize(ar, Sat.sys);
		ar & Sat.prn;
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, KFKey& kfKey)
	{
		ar & kfKey.type;
		ar & kfKey.num;
		ar & kfKey.str;
		serialize(ar, kfKey.Sat);
	}

//     template<class ARCHIVE, class TYPE1, class TYPE2>
//     void serialize(ARCHIVE& ar, pair<TYPE1, TYPE2>& pair_)
//     {
// 		serialize(ar, pair_.first);
// 		serialize(ar, pair_.second);
// 	}

	template<class ARCHIVE, class TYPE>
	void serialize(ARCHIVE& ar, map<KFKey, TYPE>& mapItem)
	{
		int num = mapItem.size();
		ar & num;

		if (ARCHIVE::is_saving::value)
		{
			//writing
			for (auto& [kfKey, val] : mapItem)
			{
				KFKey key = kfKey;

				serialize(ar, key);

				ar & val;
			}
		}
		else
		{
			//reading
			for (int i = 0; i < num; i++)
			{
				KFKey kfKey;

				serialize(ar, kfKey);

				ar & mapItem[kfKey];
			}
		}
	}

	template<class ARCHIVE, class A, class B>
	void serialize(ARCHIVE& ar, pair<A,B>& pair_)
	{
		serialize(ar, pair_.first);
		serialize(ar, pair_.second);
	}
	
	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, string& str)
	{
		ar & str;
	}

	template<class ARCHIVE, class KEY, class TYPE>
	void serialize(ARCHIVE& ar, map<KEY, TYPE>& mapItem)
	{
		int num = mapItem.size();
		ar & num;

		if (ARCHIVE::is_saving::value)
		{
			//writing
			for (auto& [kfKey, val] : mapItem)
			{
				KEY key = kfKey;

				serialize(ar, key);

				ar & val;
			}
		}
		else
		{
			//reading
			for (int i = 0; i < num; i++)
			{
				KEY kfKey;

				serialize(ar, kfKey);

				ar & mapItem[kfKey];
			}
		}
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, TransitionMatrixObject& object)
	{
		ar & object.forwardTransitionMap;
		ar & object.rows;
		ar & object.cols;
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, ObsKey& obsKey)
	{
		ar & obsKey.Sat;
		ar & obsKey.str;
		ar & obsKey.type;
		ar & obsKey.num;
	}

	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, KFState& kfState)
	{
		serialize(ar, kfState.kfIndexMap);
		serialize(ar, kfState.time);
		serialize(ar, kfState.x);
		serialize(ar, kfState.P);
	}
	
	template<class ARCHIVE>
	void serialize(ARCHIVE& ar, KFMeas& kfMeas)
	{
		int rows = kfMeas.H.rows();
		int cols = kfMeas.H.cols();
		ar & rows;
		ar & cols;
		
		if (ARCHIVE::is_saving::value) 
		{
			//just wrote this, we are writing
			map<pair<int, int>, double>	H;
			
			ar & kfMeas.obsKeys;
			serialize(ar, kfMeas.time);
			serialize(ar, kfMeas.VV);
			
			for (int i = 0; i < rows; i++)
			for (int j = 0; j < cols; j++)
			{
				double value = kfMeas.H(i,j);
				if (value)
				{
					H[{i,j}] = value;
				}
			}
			
			serialize(ar, H);
		}
		else
		{
			//we're reading
			map<pair<int, int>, double>	H;
			
			ar & kfMeas.obsKeys;
			serialize(ar, kfMeas.time);
			serialize(ar, kfMeas.VV);
			serialize(ar, H);
			
			kfMeas.H = MatrixXd::Zero(rows,cols);
			kfMeas.R = MatrixXd::Zero(rows,rows);
			kfMeas.V = VectorXd::Zero(rows);
			
			for (auto & [index, value] : H)
			{
				kfMeas.H(index.first, index.second) = value;
			}
		}
	}
}

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
		std::cout << std::endl << "Error opening algebra file " << filename <<  "for writing";
		return;
	}

// 	std::cout << "RTS - writing " << type._to_string() << " to file " << filename << "\n";
	
	binary_oarchive serial(fileStream, 1);	//no header

	long int pos = fileStream.tellp();

	int type_int = type;
	serialize(serial, type_int);
	serialize(serial, object);

	long int end = fileStream.tellp();
	long int delta = end - pos;
	serialize(serial, delta);
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

	serialize(serial, itemDelta);

	long int itemPosition = currentPosition - itemDelta;

	fileStream.seekg(itemPosition, fileStream.beg);

	int type_int;
	serialize(serial, type_int);

	E_SerialObject type = E_SerialObject::_from_integral(type_int);
	if (type != expectedType)
	{
		std::cout << std::endl << "Error: Unexpected algebra file object type";
		return false;
	}

	serialize(serial, object);

	startPos = itemPosition;

	return true;
}

E_SerialObject getFilterTypeFromFile(
	long int&	startPos,
	string		filename);

#include "station.hpp"

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

#endif
