#include "common/algebra.hpp"
#include "common/receiver.hpp"
#include "common/sinex.hpp"

#include <boost/iostreams/stream.hpp>

#include "common/trace.hpp"

// The Zhang full-rank unit target is intentionally header-oriented and does
// not link the complete PEA algebra/trace dependency graph.  Checkpoint tests
// need a concrete KFState container, so this target supplies only the exact
// key ordering and the empty-state initialisation used by KFState's inline
// constructor.  Production PEA links the real implementations in algebra.cpp
// and trace.cpp.

thread_local boost::iostreams::stream<boost::iostreams::null_sink> nullStream{
	boost::iostreams::null_sink{}};

SinexSiteId   dummySiteid;
SinexReceiver dummyReceiver;
SinexAntenna  dummyAntenna;
SinexSiteEcc  dummySiteEcc;

GTime::operator PTime() const
{
	PTime result;
	// ReceiverLogs only needs a deterministic sentinel in this focused test
	// target; no POSIX/GPST calendar conversion is exercised here.
	result.bigTime = bigTime;
	return result;
}

const KFKey KFState::oneKey = {.type = KF::ONE};

bool KFKey::operator==(const KFKey& other) const
{
	return str == other.str && Sat == other.Sat && type == other.type &&
		num == other.num;
}

bool KFKey::operator!=(const KFKey& other) const
{
	return !(*this == other);
}

bool KFKey::operator<(const KFKey& other) const
{
	if (str != other.str)
	{
		return str < other.str;
	}
	if (Sat != other.Sat)
	{
		return Sat < other.Sat;
	}
	if (type != other.type)
	{
		return type < other.type;
	}
	return num < other.num;
}

void KFState::initFilterEpoch(Trace&)
{
	initNoiseMap.clear();
	stateTransitionMap[oneKey][oneKey][0] = 1;
}
