
// #pragma GCC optimize ("O0")

#include "interactiveTerminal.hpp"
#include "receiver.hpp"
#include "summary.hpp"

void outputStatistics(
	Trace&				trace,
	map<string, int>&	statisticsMap,
	map<string, int>&	statisticsMapSum)
{
	for (auto& [str, count] : statisticsMap)
	{
		statisticsMapSum[str] += count;
	}

	{	InteractiveTerminal	ss(			"Filter-Statistics/Epoch", trace);
		Block				block(ss,	"Filter-Statistics/Epoch");				for (auto& [str, count] : statisticsMap)		tracepdeex(0, ss, "! %-40s: %d\n", str.c_str(), count);		}
	{	InteractiveTerminal ss(			"Filter-Statistics/Total", trace);
		Block				block(ss,	"Filter-Statistics/Total");				for (auto& [str, count] : statisticsMapSum)		tracepdeex(0, ss, "! %-40s: %d\n", str.c_str(), count);		}

	statisticsMap.clear();
}

/** Output statistics from each station.
* Including observation counts, slips, beginning and ending epochs*/
void outputSummaries(
	Trace&		trace,		///< Trace stream to output to
	ReceiverMap&	receiverMap)	///< Map of stations used throughout the program.
{
	trace << "\n" << "--------------- SUMMARIES ------------------- " << "\n";

	for (auto& [id, rec] : receiverMap)
	{
		trace << "\n" << "------------------- " << rec.id << " --------------------";
		auto a	= boost::posix_time::from_time_t((time_t)rec.firstEpoch.bigTime);
		auto b	= boost::posix_time::from_time_t((time_t)rec.lastEpoch.	bigTime);
		auto ab	= b-a;

		trace << "\n" << "First Epoch : " << a;
		trace << "\n" << "Last  Epoch : " << b;
		trace << "\n" << "Epoch Count : " << rec.epochCount;
		if (rec.epochCount > 1)
			trace << "\n" << "Epoch Step  : " << ab / (rec.epochCount - 1);
		trace << "\n" << "Duration    : " << ab;
		trace << "\n" << "Observations: " << rec.obsCount;

		bool first = true;
		trace << "\n" << "By Code     : ";
		for (auto& [code, count] : rec.codeCount)
		{
			if (first)
				first = false;
			else
				trace << "  |  ";

			trace << code._to_string() << " : " << count;
		}

		first = true;
		trace << "\n" << "By Satellite: ";
		for (auto& [sat, count] : rec.satCount)
		{
			if (first)
				first = false;
			else
				trace << "  |  ";

			trace << sat << " : " << count;
		}
		trace << "\n" << "GObs/Slips   : " << rec.obsCount / (rec.slipCount + 1);
		trace << "\n";
	}
}
