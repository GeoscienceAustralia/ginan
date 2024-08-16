
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


string posHeader = R"POSHEADER(PBO Station Position Time Series. Reference Frame : IGS20
Format Version: 2.0.0
4-character ID: {ID}
First Epoch   : {FirstEpoch}
XYZ Reference position :  {POSX} {POSY} {POSZ} (IGS20)
NEU Reference position :  {POSN} {POSE} {POSU} (IGS20/WGS84)
Start Field Description
YYYY-MM-DDTHH:MM:SS.SS  Date and Time of given position epoch (GPS Time)
YYYY.YYYYYYYYY          Decimal year of given position epoch (GPS Time)
X                       X coordinate, Specified Reference Frame, meters
Y                       Y coordinate, Specified Reference Frame, meters
Z                       Z coordinate, Specified Reference Frame, meters
Sx                      Sigma of the X position
Sy                      Sigma of the Y position
Sz                      Sigma of the Z position
Rxy                     Correlation between the X and Y position
Rxz                     Correlation between the X and Z position
Ryz                     Correlation between the Y and Z position
Nlat                    North latitude, WGS-84 ellipsoid, decimal degrees
Elong                   East longitude, WGS-84 ellipsoid, decimal degrees
Height (Up)             Height relative to WGS-84 ellipsoid, meters
dN                      Difference in North component from NEU reference position, meters
dE                      Difference in East component from NEU reference position, meters
dU                      Difference in vertical component from NEU reference position, meters
Sn                      Sigma of dN, meters
Se                      Sigma of dE, meters
Su                      Sigma of dU, meters
Rne                     Correlation between dN and dE
Rnu                     Correlation between dN and dU
Reu                     Correlation between dE and dU
Soln                    Solution type
End Field Description
*YYYY-MM-DDTHH:MM:SS.SS YYYY.YYYYYYYYY        X             Y              Z           Sx         Sy        Sz      Rxy     Rxz     Ryz        NLat            Elong        Height         dN          dE          dU        Sn       Se         Su       Rne     Rnu     Reu  soln
)POSHEADER";

map<string, VectorEcef>	posAprioriValue;

void replacePlaceholder(string& str, const string& placeholder, const string& value) {
	size_t pos = str.find(placeholder);
	if (pos != string::npos) {
		str.replace(pos, placeholder.length(), value);
	}
}

void formatAndReplace(std::string& header, const std::string& placeholder, double value, int precision, int width) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(precision) << std::setw(width) << value;
    replacePlaceholder(header, placeholder, oss.str());
}


template <typename T>
void formatAndOutput(std::ostream& output, T value, int precision, int width) {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(precision) << std::setw(width) << value;
    output << " " << oss.str();
}


void writePOSHeader(
	Trace&	output,
	string	name,
	GTime	time)
{
	if (name.empty())
	{
		name = "Track";
	}

	VectorEcef aprEcef = posAprioriValue[name];
	VectorPos aprPos = ecef2pos(aprEcef);
	string header = posHeader;

	replacePlaceholder(header, "{ID}", name);
	replacePlaceholder(header, "{FirstEpoch}", time.to_ISOstring(2));
    formatAndReplace(header, "{POSX}", aprEcef.x(),		6,	13);
    formatAndReplace(header, "{POSY}", aprEcef.y(),		6,	13);
    formatAndReplace(header, "{POSZ}", aprEcef.z(),		6,	13);
    formatAndReplace(header, "{POSN}", aprPos.latDeg(), 12,	14);
    formatAndReplace(header, "{POSE}", aprPos.lonDeg(), 12,	14);
    formatAndReplace(header, "{POSU}", aprPos.hgt(),	9,	12);

	output << header;
}


void writePOSEntry(
	Trace&		output,
	Receiver&	rec,
	KFState&	kfState)
{
	VectorEcef xyz;
	VectorEcef apriori = rec.aprioriPos;
	Matrix3d VCV;
	bool found = true;
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	( kfKey.type	!= KF::REC_POS
			||kfKey.str		!= rec.id)
		{
			continue;
		}

		xyz[kfKey.num] =	kfState.x(index);

		for (auto& [kfKey2, index2] : kfState.kfIndexMap)
		{
			if (kfKey2.type != KF::REC_POS
				|| kfKey2.str != rec.id)
			{
				continue;
			}
			VCV(kfKey.num, kfKey2.num) = kfState.P(index, index2);
		}

	}
	if (found == false)
	{
		xyz = apriori;
	}

	VectorPos pos = ecef2pos(xyz);
	VectorEcef aprEcef = posAprioriValue[rec.id];
	VectorPos aprPos = ecef2pos(posAprioriValue[rec.id]);

	VectorEcef diff = xyz - aprEcef;
	Matrix3d E;
	pos2enu(aprPos, E.data());
	Vector3d diffENU = E * diff;
	Matrix3d VCVenu = E * VCV  * E.transpose();
	Vector3d var;
	Vector3d varENU;

	for (int i = 0; i < 3; i++)
	{
		var[i] = SQRT(VCV(i,i));
		varENU[i] = SQRT(VCVenu(i,i));
	}
	for (int i = 0; i < 3; i++)
		for (int j = i; j < 3; j++)
		{
			VCVenu(i,j) /= (varENU[i] * varENU[j]);
			VCV(i,j)    /= (var[i] * var[j]);
		}

    output << " " << kfState.time.to_ISOstring(2);
	formatAndOutput(output,	kfState.time.to_decyear(), 9, 14);
    formatAndOutput(output,	xyz.x(),		5,	11);
    formatAndOutput(output,	xyz.y(),		5,	11);
    formatAndOutput(output,	xyz.z(),		5,	11);
    formatAndOutput(output,	var.x(),		5,	9);
    formatAndOutput(output,	var.y(),		5,	9);
    formatAndOutput(output,	var.z(),		5,	9);
    formatAndOutput(output,	VCV(0,1),		3,	7);
    formatAndOutput(output,	VCV(0,2),		3,	7);
    formatAndOutput(output,	VCV(1,2),		3,	7);
    formatAndOutput(output,	pos.latDeg(),	10,	15);
    formatAndOutput(output,	pos.lonDeg(),	10,	15);
    formatAndOutput(output,	pos.hgt(),		5,	11);
    formatAndOutput(output,	diffENU.y(),	5,	11);
    formatAndOutput(output,	diffENU.x(),	5,	11);
    formatAndOutput(output,	diffENU.z(),	5,	11);
    formatAndOutput(output,	varENU(1),		5,	9);
    formatAndOutput(output,	varENU(0),		5,	9);
    formatAndOutput(output,	varENU(2),		5,	9);
    formatAndOutput(output,	VCVenu(0,1),	3,	7);
    formatAndOutput(output,	VCVenu(1,2),	3,	7);
    formatAndOutput(output,	VCVenu(0,2),	3,	7);
	output << " ginan"; //Placeholder for solution type.
    output << "\n";
}

void writePOS(
	string		filename,
	KFState&	kfState,
	Receiver&	rec)
{
	std::ofstream output(filename, std::fstream::in | std::fstream::out);
	if (!output)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: Error opening POS file '" << filename << "'\n";
		return;
	}

	output.seekp(0, output.end);					// seek to end of file

	if (output.tellp() == 0)
	{
		VectorEcef apriori = rec.aprioriPos;
		posAprioriValue[rec.id] = apriori;
		writePOSHeader(output, rec.id, kfState.time);
	}

	writePOSEntry(output, rec, kfState);

}
