
// #pragma GCC optimize ("O0")

#include "receiver.hpp"
#include "summary.hpp"

void outputStatistics(
	Trace&				trace,
	map<string, int>&	statisticsMap,
	map<string, int>&	statisticsMapSum)
{
	Block block(trace, "Filter Statistics");

	for (auto& [str, count] : statisticsMap)
	{
		tracepdeex(0, trace, "! %-40s: %d\n", str.c_str(), count);
		statisticsMapSum[str] += count;
	}

	statisticsMap.clear();
}

/** Output statistics from each station.
* Including observation counts, slips, beginning and ending epochs*/
void outputSummaries(
	Trace&		trace,		///< Trace stream to output to
	ReceiverMap&	receiverMap)	///< Map of stations used throughout the program.
{
	trace << std::endl << "--------------- SUMMARIES ------------------- " << std::endl;

	for (auto& [id, rec] : receiverMap)
	{
		trace << std::endl << "------------------- " << rec.id << " --------------------";
		auto a	= boost::posix_time::from_time_t((time_t)rec.firstEpoch.bigTime);
		auto b	= boost::posix_time::from_time_t((time_t)rec.lastEpoch.	bigTime);
		auto ab	= b-a;

		trace << std::endl << "First Epoch : " << a;
		trace << std::endl << "Last  Epoch : " << b;
		trace << std::endl << "Epoch Count : " << rec.epochCount;
		if (rec.epochCount > 1)
			trace << std::endl << "Epoch Step  : " << ab / (rec.epochCount - 1);
		trace << std::endl << "Duration    : " << ab;
		trace << std::endl << "Observations: " << rec.obsCount;

		bool first = true;
		trace << std::endl << "By Code     : ";
		for (auto& [code, count] : rec.codeCount)
		{
			if (first)
				first = false;
			else
				trace << "  |  ";

			trace << code._to_string() << " : " << count;
		}

		first = true;
		trace << std::endl << "By Satellite: ";
		for (auto& [sat, count] : rec.satCount)
		{
			if (first)
				first = false;
			else
				trace << "  |  ";

			trace << sat << " : " << count;
		}
		trace << std::endl << "GObs/Slips   : " << rec.obsCount / (rec.slipCount + 1);
		trace << std::endl;
	}
}
