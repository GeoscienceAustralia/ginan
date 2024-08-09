
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include <sstream>
#include <fstream>
#include <string>
#include <chrono>

using std::chrono::system_clock;
using std::stringstream;
using std::string;

#include "peaCommitStrings.hpp"
#include "navigation.hpp"
#include "ephPrecise.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "erp.hpp"

#define NMAX	3			/* order of polynomial interpolation */


string ERPValues::toString()
{
	Vector3d erp;
	erp << xp, yp, ut1Utc;

	stringstream ss;
	ss << erp.transpose().format(heavyFmt);
	return ss.str();
}

string ERPValues::toReadableString()
{
	Vector3d erp;
	erp <<
		xp		* R2MAS,
		yp		* R2MAS,
		ut1Utc	* S2MTS;

	stringstream ss;
	ss << erp.transpose().format(heavyFmt);
	return ss.str();
}

/** read IGS ERP products
 */
void readIgsErp(
	string					line,			///< line to read for IGS ERP file (IGS ERP ver.2)
	map<GTime, ERPValues>&	erpMap)			///< earth rotation parameters
{
	double	mjdval	= 0;
	double	v[13]	= {};
	int found = sscanf(line.c_str(),"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
			&mjdval,
			&v[0],
			&v[1],
			&v[2],
			&v[3],
			&v[4],
			&v[5],
			&v[6],
			&v[7],
			&v[8],
			&v[9],
			&v[10],
			&v[11],
			&v[12]);

	if (found < 14)
	{
		return;
	}

	MjDateUtc mjd;
	mjd.val = mjdval;

	ERPValues erpv;

	erpv.time			= mjd;
	erpv.xp				= v[0]	* 1E-6*AS2R;
	erpv.yp				= v[1]	* 1E-6*AS2R;
	erpv.ut1Utc			= v[2]	* 1E-7;
	erpv.lod			= v[3]	* 1E-7;
	erpv.xpSigma		= v[4]	* 1E-6*AS2R;
	erpv.ypSigma		= v[5]	* 1E-6*AS2R;
	erpv.ut1UtcSigma	= v[6]	* 1E-7;
	erpv.lodSigma		= v[7]	* 1E-7;
	erpv.xpr			= v[11]	* 1E-6*AS2R;
	erpv.ypr			= v[12]	* 1E-6*AS2R;

	erpMap[erpv.time] = erpv;
}

/** read IERS C04 EOP products
 */
void readIers14C04(
	string					line,			///< line to read for IERS C04 file
	map<GTime, ERPValues>&	erpMap)			///< earth rotation parameters
{
	int		date[3];
	double	mjdval	= 0;
	double	v[12]	= {};

	int found = sscanf(line.c_str(),"%d %d %d %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
			&date[0],
			&date[1],
			&date[2],
			&mjdval,
			&v[0],
			&v[1],
			&v[2],
			&v[3],
			&v[4],
			&v[5],
			&v[6],
			&v[7],
			&v[8],
			&v[9],
			&v[10],
			&v[11]);

	if (found < 16)
	{
		return;
	}

	MjDateUtc mjd;
	mjd.val = mjdval;

	ERPValues erpv;

	erpv.time	= mjd;
	if (erpv.time < GTime::noTime())
	{
		return;
	}

	erpv.xp				= v[0]	* AS2R;
	erpv.yp				= v[1]	* AS2R;
	erpv.ut1Utc			= v[2];
	erpv.lod			= v[3];
	erpv.xpSigma		= v[6]	* AS2R;
	erpv.ypSigma		= v[7]	* AS2R;
	erpv.ut1UtcSigma	= v[8];
	erpv.lodSigma		= v[9];

	erpMap[erpv.time] = erpv;
}

/** read IERS C04 EOP products
 */
void readIers20C04(
	string					line,			///< line to read for IERS C04 file
	map<GTime, ERPValues>&	erpMap)			///< earth rotation parameters
{
	int		date[4];
	double	mjdval	= 0;
	double	v[17]	= {};

	int found = sscanf(line.c_str(),"%d %d %d %d %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
			&date[0],
			&date[1],
			&date[2],
			&date[4],
			&mjdval,
			&v[0],
			&v[1],
			&v[2],
			&v[3],
			&v[4],
			&v[5],
			&v[6],
			&v[7],
			&v[8],
			&v[9],
			&v[10],
			&v[11],
			&v[12],
			&v[13],
			&v[14],
			&v[15]);

	if (found < 21)
	{
		return;
	}

	MjDateUtc mjd;
	mjd.val = mjdval;

	ERPValues erpv;

	erpv.time	= mjd;
	if (erpv.time < GTime::noTime())
	{
		return;
	}

	erpv.xp				= v[0]	* AS2R;
	erpv.yp				= v[1]	* AS2R;
	erpv.ut1Utc			= v[2];
	erpv.xpr			= v[5]	* AS2R;
	erpv.ypr			= v[6]	* AS2R;
	erpv.lod			= v[7];

	erpv.xpSigma		= v[8]	* AS2R;
	erpv.ypSigma		= v[9]	* AS2R;
	erpv.ut1UtcSigma	= v[10];
	erpv.xprSigma		= v[13]	* AS2R;
	erpv.yprSigma		= v[14]	* AS2R;
	erpv.lodSigma		= v[15];

	erpMap[erpv.time] = erpv;
}

/** read IERS final EOP products
 */
void readIersFinal(
	string					line,			///< line to read for IERS final file
	map<GTime, ERPValues>&	erpMap)			///< earth rotation parameters
{
	if (line.size() < 78)
	{
		return;
	}
	char* buff = &line[0];

	MjDateUtc mjd;
	mjd.val				= str2num(buff, 7,	8);
	double xp			= str2num(buff, 18,	9);
	double xpSigma		= str2num(buff, 27,	9);
	double yp			= str2num(buff, 37,	9);
	double ypSigma		= str2num(buff, 46,	9);
	double ut1Utc		= str2num(buff, 58,	10);
	double ut1UtcSigma	= str2num(buff, 68,	10);

	ERPValues erpv;

	erpv.time			= mjd;
	erpv.xp				= xp		* AS2R;
	erpv.yp				= yp		* AS2R;
	erpv.ut1Utc			= ut1Utc;
	erpv.xpSigma		= xpSigma	* AS2R;
	erpv.ypSigma		= ypSigma	* AS2R;
	erpv.ut1UtcSigma	= ut1UtcSigma;

	if	( buff[16] == 'P'
		||buff[57] == 'P')
	{
		erpv.isPredicted	= true;
	}

	if (line.size() >= 93)
	{
		double lod			= str2num(buff, 79,	7);
		double lodSigma		= str2num(buff, 86,	7);

		erpv.lod			= lod		* 1E-3;
		erpv.lodSigma		= lodSigma	* 1E-3;
	}

	erpMap[erpv.time] = erpv;
}

/** read IERS Bulletin-A EOP products
 */
void readIersBulletinA(
	string					line,			///< line to read for IERS Bulletin-A file
	map<GTime, ERPValues>&	erpMap)			///< earth rotation parameters
{
	int		date[3];
	double	mjdval	= 0;
	double	v[6]	= {};

	int found = sscanf(line.c_str(),"%d %d %d %lf %lf %lf %lf %lf %lf %lf",
			&date[0],
			&date[1],
			&date[2],
			&mjdval,
			&v[0],
			&v[1],
			&v[2],
			&v[3],
			&v[4],
			&v[5]);

	if (found < 7)
	{
		return;
	}

	MjDateUtc mjd;
	ERPValues erpv;

	if		(found == 10)	// Combined Earth Orientation Parameters (Rapid)
	{
		mjd.val = mjdval;
		erpv.time			= mjd;
		erpv.xp				= v[0]	* AS2R;
		erpv.xpSigma		= v[1]	* AS2R;
		erpv.yp				= v[2]	* AS2R;
		erpv.ypSigma		= v[3]	* AS2R;
		erpv.ut1Utc			= v[4];
		erpv.ut1UtcSigma	= v[5];

		erpMap[erpv.time] = erpv;
	}
	else if	(found == 7)	// Predictions
	{
		mjd.val = mjdval;
		erpv.time			= mjd;
		erpv.xp				= v[0]	* AS2R;
		erpv.yp				= v[1]	* AS2R;
		erpv.ut1Utc			= v[2];
		erpv.isPredicted	= true;

		erpMap[erpv.time] = erpv;
	}
}

/** Get earth rotation parameter values
 */
ERPValues getErp(
	ERP&		erp,		///< earth rotation parameters
	GTime		time,		///< Time
	bool		useFilter)	///< Optionally use the filter values stored in the erp object
{
	if	( useFilter
		&&erp.filterValues.isFiltered)
	{
		ERPValues erpv = erp.filterValues;

		double dt = (time - erpv.time).to_double() / S_IN_DAY;
		if (dt)
		{
			erpv.time	+= dt;
			erpv.xp		+= erpv.xpr * dt;
			erpv.yp		+= erpv.ypr * dt;
			erpv.ut1Utc	-= erpv.lod * dt;
		}

		return erpv;
	}

	ERPValues erpv;

	auto& recOpts = acsConfig.getRecOpts("global");

	if (recOpts.eop == false)
	{
		return erpv;
	}

	double dtMax = 2 * S_IN_DAY;

	for (auto rit = erp.erpMaps.rbegin(); rit != erp.erpMaps.rend(); rit++)
	{
		auto& erpMap = *rit;

		// at least two data points required
		if (erpMap.size() < 2)
			continue;

		int nMax = NMAX;
		auto erp_it = erpMap.lower_bound(time);
		if		(erp_it == erpMap.end())
		{
			// exceed the end of the map, do linear extrapolation
			nMax = 1;
			erp_it--;
		}
		else if	(erp_it == erpMap.begin())
		{
			// before the beginning of the map, do linear extrapolation
			nMax = 1;
		}

		//go forward a few steps to make sure we're far from the end of the map.
		for (int i = 0; i <= nMax/2; i++)
		{
			erp_it++;
			if (erp_it == erpMap.end())
			{
				break;
			}
		}

		//go backward a few steps to make sure we're far from the beginning of the map
		for (int i = 0; i <= nMax; i++)
		{
			erp_it--;
			if (erp_it == erpMap.begin())
			{
				break;
			}
		}

		vector<ERPValues>	erpvs;
		vector<double>		dt;

		//get interpolation parameters

		for (int i = 0; i <= nMax; i++, erp_it++)
		{
			if (erp_it == erpMap.end())
			{
				break;
			}

			auto& [itTime, erpv] = *erp_it;

			dt		.push_back((itTime - time).to_double());
			erpvs	.push_back(erpv);
		}

		if	( dt.front()	>= +dtMax
			||dt.back() 	<= -dtMax)
		{
			// we're further away than last time, try the next map/file
			continue;
		}

		erpv = interpolate(dt, erpvs);

		if	( dt.front()	<= 0
			&&dt.back()		>= 0)
		{
			if (erpvs.back().isPredicted)
			{
				BOOST_LOG_TRIVIAL(warning) << "Warning: Predicted ERP used for interpolation.\n";
			}

			// interpolation done
			return erpv;
		}

		if		( dt.front()	> 0		&& dt.front()	< +dtMax)		{	dtMax = dt.front();		continue;	}
		else if	( dt.back() 	< 0		&& dt.back() 	> -dtMax)		{	dtMax = -dt.back();		continue;	}
	}

	if (erpv.time > GTime::noTime())
	{
		erpv.isPredicted = true;

		BOOST_LOG_TRIVIAL(warning) << "Warning: No suitable data for ERP interpolation, extrapolated ERP in use.\n";
	}
	else
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: failed to get ERP at " << time.to_string() << ".\n";
	}

	return erpv;
}

void writeErp(
	string		filename,
	ERPValues&	erp)
{
	if (filename.empty())
	{
		return;
	}

	std::ofstream erpStream(filename, std::ios::app);

	if (!erpStream)
	{
		BOOST_LOG_TRIVIAL(error) << "Error opening " << filename << " for ERP file.";

		return;
	}

	erpStream.seekp(0, erpStream.end);					// seek to end of file

	if (erpStream.tellp() == 0)
	{
		erpStream << "VERSION 2" << "\n";
		erpStream << " Generated by GINAN " << ginanCommitVersion() << " branch " << ginanBranchName() << "\n";

		erpStream << "----------------------------------------------------------------------------------------------------------------" << "\n";
		erpStream << "       MJD    Xpole    Ypole  UT1-UTC      LOD     Xsig     Ysig    UTsig   LODsig  Nr  Nf  Nt      Xrt      Yrt" << "\n";
		erpStream << "             1E-6as   1E-6as    1E-7s  1E-7s/d   1E-6as   1E-6as    1E-7s  1E-7s/d               1E-6/d   1E-6/d" << "\n";
	}

	int numRecs			= 0;
	int numFixedRecs	= 0;
	int numSats			= 0;

	MjDateUtc mjd = erp.time;

	tracepdeex(0, erpStream, "%8.4f %8d %8d %8d %8d %8d %8d %8d %8d %3d %3d %3d %8d %8d\n",
					mjd.to_double(),
			(int)	(erp.xp				* 1E6 * R2AS),
			(int)	(erp.yp				* 1E6 * R2AS),
			(int)	(erp.ut1Utc			* 1E7),
			(int)	(erp.lod			* 1E7),
			(int)	(erp.xpSigma		* 1E6 * R2AS),
			(int)	(erp.ypSigma		* 1E6 * R2AS),
			(int)	(erp.ut1UtcSigma	* 1E7),
			(int)	(erp.lodSigma		* 1E7),
					numRecs,
					numFixedRecs,
					numSats,
			(int)	(erp.xpr			* 1E6 * R2AS),
			(int)	(erp.ypr			* 1E6 * R2AS));
}

/** Get earth rotation parameter values
 */
ERPValues getErpFromFilter(
	const KFState&	kfState)
{
	ERPValues erpv;

	bool found = false;
	for (int i = 0; i < 3; i++)
	{
		double val		= 0;
		double valVar	= 0;
		double rate		= 0;
		double rateVar	= 0;

		KFKey kfKey;
		kfKey.num	= i;

		kfKey.type	= KF::EOP;
		found |= kfState.getKFValue(kfKey, val,		&valVar);

		kfKey.type	= KF::EOP_RATE;
		found |= kfState.getKFValue(kfKey, rate,	&rateVar);

		switch (i)
		{
			case 0:	erpv.xp		= +val	* MAS2R;		erpv.xpSigma		= sqrt(valVar)	* MAS2R;
					erpv.xpr	= +rate	* MAS2R;		erpv.xprSigma		= sqrt(rateVar)	* MAS2R;	break;
			case 1:	erpv.yp		= +val	* MAS2R;		erpv.ypSigma		= sqrt(valVar)	* MAS2R;
					erpv.ypr	= +rate	* MAS2R;		erpv.yprSigma		= sqrt(rateVar)	* MAS2R;	break;
			case 2:	erpv.ut1Utc	= +val	* MTS2S;		erpv.ut1UtcSigma	= sqrt(valVar)	* MTS2S;
					erpv.lod	= -rate	* MTS2S;		erpv.lodSigma		= sqrt(rateVar)	* MTS2S;	break;
			default:
				break;
		}
	}

	if (found)
	{
		erpv.isFiltered	= true;
		erpv.time		= kfState.time;
	}

	return erpv;
}

void writeErpFromNetwork(
	string		filename,
	KFState&	kfState)
{
	static GTime lastTime = GTime::noTime();

	if (abs((lastTime - kfState.time).to_double()) < 10)
	{
		//dont write duplicate lines (closer than 10s (4dp mjd))
		return;
	}

	lastTime = kfState.time;

	ERPValues erpv = getErpFromFilter(kfState);

	writeErp(filename, erpv);
}


/** read earth rotation parameters
 */
void readErp(
	string	filename,		///< ERP file
	ERP&	erp)			///< earth rotation parameters
{
	std::ifstream filestream(filename);
	if (!filestream)
	{
//         trace(2, "erp file open error: file=%s\n", file);
		return;
	}

	map<GTime, ERPValues> erpMap;

	while (filestream)
	{
		string line;

		getline(filestream, line);

		if		( (line[16]		== 'I'	|| line[16]		== 'P')
				&&(line[57]		== 'I'	|| line[57]		== 'P'))	readIersFinal		(line, erpMap);
		else if	(  line.size()	== 218)								readIers20C04		(line, erpMap);
		else if	(  line.size()	== 155)								readIers14C04		(line, erpMap);
		else if	(  line.size()	<= 127	&& line.size()	>= 106)		readIgsErp			(line, erpMap);
		else if	(  line.size()	<=  79)								readIersBulletinA	(line, erpMap);
	}

	erp.erpMaps.push_back(erpMap);
}

Matrix3d stationEopPartials(
	Vector3d&	rRec)
{
	//compute partials and convert to units of MxS

	Matrix3d partials;
	auto& X = rRec(0);
	auto& Y = rRec(1);
	auto& Z = rRec(2);
	partials(0,0) = +Z * MAS2R;		//dx/dxp		= dx/dRotY
	partials(0,1) =  0;				//dy/dxp		= dy/dRotY
	partials(0,2) = -X * MAS2R;		//dz/dxp		= dz/dRotY

	partials(1,0) =  0;				//dx/dyp		= dx/dRotX
	partials(1,1) = -Z * MAS2R;		//dy/dyp		= dy/dRotX
	partials(1,2) = +Y * MAS2R;		//dz/dyp		= dz/dRotX

	partials(2,0) = +Y * MTS2R;		//dx/dut1		= dx/dRotZ
	partials(2,1) = -X * MTS2R;		//dy/dut1		= dy/dRotZ
	partials(2,2) =  0;				//dz/dut1		= dz/dRotZ

	return partials;
}
