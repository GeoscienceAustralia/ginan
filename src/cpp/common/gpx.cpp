
#include <boost/log/trivial.hpp>

#include "coordinates.hpp"
#include "constants.hpp"
#include "receiver.hpp"
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
	output << "<time>" << boost::posix_time::from_time_t((time_t)((PTime)time).bigTime) << "</time>";	//todo aaron, check format, different to below
	output << "  </metadata>\n";
	output << "<trk>"
	<< "<name>" << name << "</name>\n"
    << "  <trkseg>\n";
}


struct XmlCloser
{
	Trace&	trace;
	string	id;

	XmlCloser(
		Trace&	trace,
		string id)
	:	trace	{trace},
		id		{id}
	{
		trace << "<" << id << ">";
	}

	~XmlCloser()
	{
		trace << "</" << id << ">";
	}

	template<typename TYPE>
	Trace& operator<<(
		const TYPE&	content)
	{
		trace << content;

		return trace;
	}
};

void writeGPXEntry(
	Trace&		output,
	string		id,
	KFState&	kfState)
{
	VectorEcef xyz;
	VectorEcef var;
	VectorEcef apriori;

	bool found = true;
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	( kfKey.type != KF::REC_POS
			||kfKey.str		!= id)
		{
			continue;
		}

		xyz[kfKey.num] =		kfState.x(index);
		var[kfKey.num] = sqrt(	kfState.P(index, index));

		if	( kfKey.num == 0
			&&kfKey.rec_ptr)
		{
			auto& rec = *kfKey.rec_ptr;

			apriori = rec.aprioriPos;
		}
	}

	if (found == false)
	{
		return;
	}

	VectorPos pos = ecef2pos(xyz);

	output << std::setprecision(11);

	output
	<< "      <trkpt "
	<< "lat='" << pos.latDeg() << "' "
	<< "lon='" << pos.lonDeg() << "' "
	<< ">";

	{	XmlCloser(output, "ele")			<< pos.hgt();	}
	{	XmlCloser(output, "time")			<< boost::posix_time::to_iso_extended_string(boost::posix_time::from_time_t((time_t)((PTime)kfState.time).bigTime)) << "Z";	}


	{
		auto extensions = XmlCloser(output, "extensions");

		{	XmlCloser(output, "ginan:time")		<< kfState.time;							}
		{	XmlCloser(output, "ginan:xyz")		<< xyz		.transpose().format(lightFmt);	}
		{	XmlCloser(output, "ginan:var")		<< var		.transpose().format(lightFmt);	}
		{	XmlCloser(output, "ginan:apriori")	<< apriori	.transpose().format(lightFmt);	}

		Quaterniond quat;
		found = true;
		for (int i = 0; i < 4; i++)
		{
			KFKey kfKey;
			kfKey.type	= KF::ORIENTATION;
			kfKey.str	= id;
			kfKey.num	= i;

			if (i == 0)		found &= kfState.getKFValue(kfKey, quat.w());
			if (i == 1)		found &= kfState.getKFValue(kfKey, quat.x());
			if (i == 2)		found &= kfState.getKFValue(kfKey, quat.y());
			if (i == 3)		found &= kfState.getKFValue(kfKey, quat.z());
		}

		if (found)
		{
			{	XmlCloser(output, "Ex")		<< (quat * Vector3d::UnitX()).transpose();	}
			{	XmlCloser(output, "Ey")		<< (quat * Vector3d::UnitY()).transpose();	}
			{	XmlCloser(output, "Ez")		<< (quat * Vector3d::UnitZ()).transpose();	}
			{	XmlCloser(output, "quat")	<< quat;									}
		}
	}

	output << "</trkpt>\n";
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
