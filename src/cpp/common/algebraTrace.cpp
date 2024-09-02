
#include <iostream>
#include <fstream>
#include <thread>
#include <map>

using std::map;

#include "architectureDocs.hpp"
#include "eigenIncluder.hpp"
#include "algebraTrace.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "algebra.hpp"

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/binary_object.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/string.hpp>
#include <boost/log/trivial.hpp>


/** Save complex variables to files.
 *
 * This allows for complex variables including full filter states to be stored to file.
 *
 * When binary archives are written, first some metadata about the next segment of data is stored.
 * An enumerated variable type is written, so that a variable of that type may be prepared later for reading the data into.
 *
 * After the binary data, an entry with the position of the metadata block for that data is written.
 * This allows for the file to be read in reverse order, seeking backward from the length metadata to the type metadata, before reading the subsequent actual data.
 * This is of fundamental importance for RTS_Smoothing__(), and the primary use-case of the binary archive in Ginan.
 *
 * The binary archive is also used as storage for debugging functions such as mincon_only, but without the necessity for reverse reading
 */
Architecture Binary_Archive__()
{

}


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
	ReceiverMap&	receiverMap)
{
	map<KFKey, int> replacementKFIndexMap;
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		KFKey kfKey = key;

		if (kfKey.type == +KF::REC_POS)
		{
			//make sure all rec pos are associated with receivers
			receiverMap[kfKey.str].id = kfKey.str;
		}

		if	(  kfKey.rec_ptr == nullptr
			&& kfKey.str.empty() == false)
		{
			auto it = receiverMap.find(kfKey.str);
			if (it != receiverMap.end())
			{
				auto& [id, station]	= *it;

				kfKey.rec_ptr = &station;
			}
		}

		replacementKFIndexMap[kfKey] = index;
	}

	kfState.kfIndexMap = replacementKFIndexMap;
}

struct QueuedSpit
{
	shared_ptr<void>	ptr;
	E_SerialObject		type;
	string				filename;
	bool				valid		= false;
	bool				available	= false;
};


void spitQueuedToFile(
	QueuedSpit& spit)
{
	switch (spit.type)
	{
		default:	std::cout << "ERROR: missing queued type " << spit.type;				break;
		case E_SerialObject::FILTER_MINUS:		//fallthrough
		case E_SerialObject::FILTER_PLUS:		//fallthrough
		case E_SerialObject::FILTER_SMOOTHED:	{	auto&	kfState				= *std::static_pointer_cast<KFState>				(spit.ptr);	spitFilterToFile(kfState,			spit.type, spit.filename);	break;	}
		case E_SerialObject::TRANSITION_MATRIX:	{	auto&	transitionObject	= *std::static_pointer_cast<TransitionMatrixObject>	(spit.ptr);	spitFilterToFile(transitionObject,	spit.type, spit.filename);	break;	}
		case E_SerialObject::MEASUREMENT:		{	auto&	kfMeas				= *std::static_pointer_cast<KFMeas>					(spit.ptr);	spitFilterToFile(kfMeas,			spit.type, spit.filename);	break;	}
		case E_SerialObject::METADATA:			{	auto&	metatdata			= *std::static_pointer_cast<map<string, string>>	(spit.ptr);	spitFilterToFile(metatdata,			spit.type, spit.filename);	break;	}
	}
}

list<QueuedSpit>	spitQueue;
std::mutex			spitQueueMutex;
bool				spitQueueRunning = false;

void spitQueueRun()
{
	BOOST_LOG_TRIVIAL(debug) << "Running rts queue thread";

	while (1)
	{
		QueuedSpit* spit_ptr;

		//use pointer and braces to limit guard scope
		{
			lock_guard<mutex> guard(spitQueueMutex);

			if (spitQueue.empty())
			{
				break;
			}

			BOOST_LOG_TRIVIAL(debug) << "RTS queue has " << spitQueue.size() << " entries to go";

			spit_ptr = &spitQueue.front();
		}

		spitQueuedToFile(*spit_ptr);

		{
			lock_guard<mutex> guard(spitQueueMutex);

			spitQueue.pop_front();
		}
	}

	lock_guard<mutex> guard(spitQueueMutex);
	spitQueueRunning = false;
}

void spitFilterToFileQueued(
	shared_ptr<void>&	object_ptr,		///< Object to output
	E_SerialObject		type,			///< Type of object
	string				filename)		///< Path to file to output to
{
	QueuedSpit spit;

	spit.ptr 		= object_ptr;
	spit.type		= type;
	spit.filename	= filename;

	lock_guard<mutex> guard(spitQueueMutex);

	spitQueue.push_back(std::move(spit));

	if (spitQueueRunning == false)
	{
		spitQueueRunning = true;

		std::thread(spitQueueRun).detach();
	}
}
