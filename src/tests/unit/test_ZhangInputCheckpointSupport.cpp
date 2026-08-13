#include <istream>
#include <map>
#include <string>

#include "common/gTime.hpp"
#include "common/navigation.hpp"
#include "common/rinex.hpp"

// zhangInputCheckpoint.cpp deliberately works with the real stream classes.
// The focused unit target does not link the RINEX decoder or the complete PEA
// process, because none of these tests parse new bytes.  These definitions are
// the narrow link seam needed by RinexParser's inline virtual function.

Navigation nav;

long int streamPos(std::istream& stream)
{
	const auto position = stream.tellg();
	return position == std::istream::pos_type(-1)
		? -1
		: static_cast<long int>(position);
}

int readRnx(
	std::istream&,
	char&,
	ObsList&,
	Navigation&,
	RinexStation&,
	double&,
	E_Sys&,
	E_TimeSys&,
	std::map<E_Sys, std::map<int, CodeType>>&)
{
	return -1;
}

std::string GTime::to_string(int) const
{
	return std::to_string(static_cast<double>(bigTime));
}
