
#include <boost/log/trivial.hpp>

#include "coordinates.hpp"
#include "constants.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "trace.hpp"

#include <string>
#include <map>

using std::string;
using std::map;

map<string, size_t>	gpxEndOfContentPositionMap;

string gpxHeader = R"GPXHEADER(<?xml version="1.0" encoding="UTF-8" standalone="no" ?>

<gpx 
 xmlns="http://www.topografix.com/GPX/1/1" 
 creator="Ginan" 
 version="1.1" 
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
 xsi:schemaLocation="http://www.topografix.com/GPX/1/1 
 http://www.topografix.com/GPX/1/1/gpx.xsd">
  <metadata>
    <link href="https://github.com/GeoscienceAustralia/ginan">
      <text>Ginan</text>
    </link>
    )GPXHEADER";

string gpxFinaliser = R"GPXHEADER(
    </trkseg>
  </trk>
</gpx>)GPXHEADER";

void writeGPXHeader(
	Trace&	output,
	string	name,
	GTime	time)
{
	if (name.empty())
	{
		name = "Track";
	}
	
	output << gpxHeader;
	output << "<time>" << boost::posix_time::from_time_t((time_t)((PTime)time).bigTime) << "</time>";
	output << "  </metadata>\n";
	output << "<trk>"
	<< "<name>" << name << "</name>\n"
    << "  <trkseg>\n";
}


void writeGPXEntry(
	Trace&		output,
	string		id,
	KFState&	kfState)
{
	VectorEcef xyz;
	for (auto [kfKey, index] : kfState.kfIndexMap)
	{
		if	( kfKey.type	!= KF::REC_POS
			||kfKey.str		!= id)
		{
			continue;
		}
		
		kfState.getKFValue(kfKey, xyz[kfKey.num]);
	}
	
	VectorPos pos = ecef2pos(xyz);
	
	output << std::setprecision(10);
	
	output << "      <trkpt "
	<< "lat='" << pos.latDeg() << "' "
	<< "lon='" << pos.lonDeg() << "' "
	<< ">"
	<< "<ele>" << pos.hgt() << "</ele>"
	<< "<time>" << boost::posix_time::from_time_t((time_t)((PTime)kfState.time).bigTime) << "</time>"
	<< "</trkpt>\n";
}

void writeGPX(
	string		filename,
	KFState&	kfState,
	string		id)
{
	std::ofstream output(filename, std::fstream::in | std::fstream::out);
	if (!output)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening GPX file '" << filename << "'\n";
		return;
	}
	
	output.seekp(0, output.end);					// seek to end of file
	
	if (output.tellp() == 0)
	{
		writeGPXHeader(output, id, kfState.time);
		gpxEndOfContentPositionMap[filename] = output.tellp();
	}
	
	output.seekp(gpxEndOfContentPositionMap[filename]);
	
	writeGPXEntry(output, id, kfState);
	
	gpxEndOfContentPositionMap[filename] = output.tellp();
	
	output << gpxFinaliser;
}
