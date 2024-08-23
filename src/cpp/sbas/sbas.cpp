
#include "inputsOutputs.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "sbas.hpp"

using std::lock_guard;

GTime		lastEMSWritten;
string		lastEMSFile;

mutex					sbasMessagesMutex;
map<GTime, SBASMessage>	sbasMessages;

void writeEMSline(
	GTime			time,				///< Time of bias to write
	SBASMessage&	sbasMessage,		///< SBAS message data
	Trace&			trace)				///< Stream to output to
{
	GEpoch epoch = time;

	tracepdeex(0, trace, " %3d %02d %02d %02d %02d %02d %02d %2d %s\r\n",
		sbasMessage.prn,
		((int)	epoch.year) % 100,
		 (int)	epoch.month,
		 (int)	epoch.day,
		 (int)	epoch.hour,
		 (int)	epoch.min,
		 (int)	epoch.sec,
		sbasMessage.type,
		sbasMessage.message);
}

/** Write received SBAS messages into EMS files
*/
void writeEMSdata(
	Trace&			trace,			///< Trace to output to
	string			filename)		///< File to write
{
	string checkFile = acsConfig.ems_filename;

	lock_guard<mutex> guard(sbasMessagesMutex);
	for (auto [frameTime, sbasData] :  sbasMessages)
	{
		if (frameTime > lastEMSWritten)
		{
			// todo aaron, use the standard file rotations
			PTime pTime = frameTime;
			boost::posix_time::ptime otherPTime	= boost::posix_time::from_time_t((time_t)pTime.bigTime);

			replaceTimes(checkFile, otherPTime);

			if (checkFile != lastEMSFile)
			{
				lastEMSFile = checkFile;
				tracepdeex(3, trace, "\nStarting new EMS file: %s\n", lastEMSFile.c_str());
			}

			std::ofstream outputStream(lastEMSFile, std::fstream::app);
			if (!outputStream)
			{
				BOOST_LOG_TRIVIAL(error)	<< "ERROR: cannot open EMS file:" << lastEMSFile;
				trace						<< "ERROR: cannot open EMS file:" << lastEMSFile;

				break;
			}

			tracepdeex(4, trace, "\nWriting EMS file line: %s\n", frameTime.to_string().c_str());
			writeEMSline(frameTime, sbasData, outputStream);

			lastEMSWritten = frameTime;
		}
	}
}
