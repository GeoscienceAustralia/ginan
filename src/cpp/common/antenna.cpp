
// #pragma GCC optimize ("O0")

#include "architectureDocs.hpp"

/**
 */
FileType ATX__()
{

}

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>
#include <fstream>

using std::ifstream;

#include "eigenIncluder.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "antenna.hpp"
#include "common.hpp"
#include "enums.h"


map<E_FType, double> roughFrequency =
{
	{F1, FREQ1},
	{F2, FREQ1},
	{F5, FREQ5},
	{F6, FREQ6},
	{F7, FREQ7},
	{F8, FREQ8},
	{G1, FREQ1_GLO},
	{G2, FREQ2_GLO},
	{G3, FREQ3_GLO},
	{G4, FREQ4_GLO},
	{G6, FREQ6_GLO},
	{B1, FREQ1_CMP},
	{B3, FREQ3_CMP},
	{I9, FREQ9_IRN}
};

/* decode antenna field */
int decodef(char *p, int n, double *v)
{
	int i;
	for (i = 0; i < n; i++)
		v[i] = 0;

	for (i = 0, p = strtok(p," "); p && i < n; p = strtok(nullptr, " "))
	{
		v[i] = atof(p) * 1E-3;
		i++;
	}
	return i;
}

bool findAntenna(
	string				code,
	E_Sys				sys,
	GTime				time,
	Navigation&			nav,
	E_FType				ft,
	PhaseCenterData**	pcd_ptr_ptr)
{
// 	BOOST_LOG_TRIVIAL(debug)
// 	<< "Searching for " << type << ", " << code;

	auto it0 = nav.pcvMap.find(code);
	if (it0 == nav.pcvMap.end())
	{
		return false;
	}

	auto& [dummyCode, pcvSysFreqMap] = *it0;

	auto it1 = pcvSysFreqMap.find(sys);
	if (it1 == pcvSysFreqMap.end())
	{
		return false;
	}

	auto& [dummySys, pcvFreqMap] = *it1;

	auto it2 = pcvFreqMap.find(ft);
	if (it2 == pcvFreqMap.end())
	{
		return false;
	}

	auto& [dummy2, pcvTimeMap] = *it2;

	auto it3 = pcvTimeMap.lower_bound(time);
	if (it3 == pcvTimeMap.end())
	{
		//just use the first chronologically, (last when sorted as they are) instead
		auto it4 = pcvTimeMap.rbegin();

		auto& [dummyTime, pcd] = *it4;

		if (pcd_ptr_ptr)
			*pcd_ptr_ptr = &pcd;

		return true;
	}

	auto& [dummyTime, pcd] = *it3;

	if (pcd_ptr_ptr)
		*pcd_ptr_ptr = &pcd;

	return true;
}

/** linearly interpolate
 */
template<typename TYPE>
TYPE interp(double x1, double x2, TYPE y1, TYPE y2, double x)
{
	return y2-(y2-y1)*(x2-x)/(x2-x1);
}


Vector3d makeAntPco(
	string		id,
	E_Sys		sys,
	E_FType		ftx,
	GTime		time,
	double&		var,
	E_Radio		radio)
{
	auto& pcoFreqMap = nav.pcoMap[id][sys];

	if (pcoFreqMap.empty())
		return Vector3d::Zero();

	if (roughFrequency.find(ftx) == roughFrequency.end())
		return Vector3d::Zero();

	double lamX = CLIGHT / roughFrequency[ftx];

	Vector3d	pco1 = Vector3d::Zero();
	Vector3d	pco2 = Vector3d::Zero();
	double		lam1 = 0;
	double		lam2 = 0;

	for (auto& [ft, pcoFreq] : pcoFreqMap)
	{
		if (roughFrequency.find(ft) == roughFrequency.end())
			continue;

		double lam = CLIGHT / roughFrequency[ft];

		double var;
		Vector3d pco = antPco(id, sys, ft, time, var, radio);

		if (pco.isZero())
			continue;

		if		(lam1 == 0)							{	lam1 = lam;		pco1 = pco;	}
		else if	(lam2 == 0)							{	lam2 = lam;		pco2 = pco;	}
		else
		{
			double close1 = fabs(lam1 - lamX) - fabs(lam - lamX);
			double close2 = fabs(lam2 - lamX) - fabs(lam - lamX);

			if (close1 > close2	&& close1 > 0)		{	lam1 = lam;		pco1 = pco;	}
			if (close2 > close1 && close2 > 0)		{	lam2 = lam;		pco2 = pco;	}
		}
	}

	if (lam1 == 0)
		return Vector3d::Zero();

	var = 0;

	if	(  lam2 == 0
		|| lam1 == lam2)
	{
		return pco1;
	}

	double k32 = (lamX-lam2)/(lam1-lam2);
	double k31 = (lamX-lam1)/(lam1-lam2);

	Vector3d pco3 = k32 * pco1
				  - k31 * pco2;
	return pco3;
}

/** fetch pco
 */
Vector3d antPco(
	string		id,
	E_Sys		sys,
	E_FType		ft,
	GTime		time,
	double&		var,
	E_Radio		radio,
	bool		interp)
{
	auto it0 = nav.pcoMap.find(id);
	if (it0 == nav.pcoMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No PCO found for '" << id << "'";

		return Vector3d::Zero();
	}

	auto& [dummy0, pcoSysFreqMap] = *it0;

	vector<E_Sys>	testSyss	= {sys};
	vector<E_FType>	testFts		= {ft};

	if (acsConfig.auto_fill_pco)
	{
		testSyss.push_back(E_Sys::GPS);
		testFts	.push_back(F2);
	}

	bool found = false;

	E_Sys foundTestSys;
	for (auto testSys : testSyss)
	{
		auto it1 = pcoSysFreqMap.find(testSys);
		if (it1 == pcoSysFreqMap.end())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: No PCO found for " << id << " for " << testSys;

			continue;
		}

		foundTestSys	= testSys;
		found			= true;
		break;
	}

	if (found == false)
	{
		return Vector3d::Zero();
	}

	auto& pcoFreqMap = pcoSysFreqMap[foundTestSys];

	found = false;

	E_FType foundTestFt;
	for (auto testFt : testFts)
	{
		auto it2 = pcoFreqMap.find(testFt);
		if (it2 == pcoFreqMap.end())
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: No PCO found for " << id << " for " << foundTestSys << " L" << testFt;

			if (interp == false)
				continue;

			Vector3d madePCO = makeAntPco(id, foundTestSys, testFt, time, var, radio);

			if (madePCO.isZero() == false)
			{
				return madePCO;
			}

			continue;
		}

		foundTestFt	= testFt;
		found		= true;
		break;
	}

	if (found == false)
	{
		return Vector3d::Zero();
	}

	auto& pcoTimeMap = pcoFreqMap[foundTestFt];

	auto it3 = pcoTimeMap.lower_bound(time);
	if (it3 == pcoTimeMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No PCO found for " << id << " for " << foundTestSys << " L" << foundTestFt << " at " << time;
		return Vector3d::Zero();
	}

	auto& [dummy3, pco] = *it3;

	var = 0;

	if (radio == +E_Radio::TRANSMITTER)		return pco.satPco;
	else									return pco.recPco;
}

/** find and interpolate antenna pcv
*/
double antPcv(
	string		id,			///< antenna id
	E_Sys		sys,		///< satellite system
	E_FType		ft,			///< frequency
	GTime		time,		///< time
	AttStatus&	attStatus,	///< Orientation of antenna
	VectorEcef	e,			///< Line of sight vector
	double*		az_ptr,		///< Optional pointer to output antenna frame azimuth in degrees
	double*		zen_ptr)	///< Optional pointer to output antenna frame zenith in degrees
{
	// Rotate relative look vector into local frame
	Matrix3d ant2Ecef = rotBasisMat(attStatus.eXAnt, attStatus.eYAnt, attStatus.eZAnt);

	Vector3d localLook = ant2Ecef.transpose() * e;

	double az		= atan2(localLook(0), localLook(1));
	double zen		= acos(localLook.z()) * R2D;

	wrap2Pi(az);

	az *= R2D;

	if (az_ptr)			*az_ptr		= az;
	if (zen_ptr)		*zen_ptr	= zen;

	auto it0 = nav.pcvMap.find(id);
	if (it0 == nav.pcvMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No PCV found for '" << id << "'";
		return 0;
	}

	auto& [dummy0, pcvSysFreqMap] = *it0;

	auto it1 = pcvSysFreqMap.find(sys);
	if (it1 == pcvSysFreqMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No PCV found for " << id << " for " << sys;
		return 0;
	}

	auto& [dummy1, pcvFreqMap] = *it1;

	auto it2 = pcvFreqMap.find(ft);
	if (it2 == pcvFreqMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No PCV found for " << id << " for " << sys << " L" << ft;
		return 0;
	}

	auto& [dummy2, pcvTimeMap] = *it2;

	auto it3 = pcvTimeMap.lower_bound(time);
	if (it3 == pcvTimeMap.end())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No PCV found for " << id << " for " << sys << " L" << ft << " at " << time;
		return 0;
	}

	auto& [dummy3, pcd] = *it3;

	auto& pcvMap1D = pcd.elMap;
	auto& pcvMap2D = pcd.azElMap;

	int		nz		= pcd.nz;
	int		naz		= pcd.naz;
	double	zen1	= pcd.zenStart;
	double	dzen	= pcd.zenDelta;
	double	dazi	= pcd.aziDelta;

	/* select zenith angle range */
	int zen_n;
	for (zen_n = 1; zen_n < nz - 1; zen_n++)
	{
		if ((zen1 + dzen * zen_n) >= zen)
		{
			break;
		}
	}

	double xz1 = zen1 + dzen * (zen_n - 1);
	double xz2 = zen1 + dzen * (zen_n);


	double	pcv;

	if	( naz	== 0
		||az	== 0)
	{
		// linear interpolate receiver pcv - non azimuth-dependent

		double	yz1 = pcvMap1D[zen_n - 1];		// lower bound
		double	yz2 = pcvMap1D[zen_n];			// upper bound
		pcv = interp(xz1, xz2, yz1, yz2, zen);
	}
	else
	{
		// bilinear interpolate receiver pcv - azimuth-dependent

		// select azimuth angle range */
		int az_n;
		for (az_n = 1; az_n < naz; az_n++)
		{
			if ((dazi * az_n) >= az)
			{
				break;
			}
		}
		if (az_n == naz)
		{
			az_n = 0;
		}

		double xa1 = dazi * (az_n -1);
		double xa2 = dazi * (az_n);

		double yz3 = pcvMap2D[az_n-1]	[zen_n-1];		double yz1 = pcvMap2D[az_n-1]	[zen_n];
		double yz4 = pcvMap2D[az_n]		[zen_n-1];		double yz2 = pcvMap2D[az_n]		[zen_n];

		// linear interpolation along zenith angle
		double ya1	= interp(xz1, xz2, yz3, yz1, zen);
		double ya2 	= interp(xz1, xz2, yz4, yz2, zen);

		// linear interpolation along azimuth angle
		pcv	= interp(xa1, xa2, ya1, ya2, az);
	}

	return pcv;
}

/**Change the last four characters of antenna type to NONE
 * e,g, "AOAD/M_T        JPLA" => "AOAD/M_T        NONE"
 * This function is useful for when searching for an antenna model in ANTEX
 * The IGS convention is to default to NONE for the radome if the calibration value is not available
 */
void radome2none(
	string& antenna_type)
{
	size_t length = antenna_type.size();
	if (length != 20)
	{
		printf("\n*** ERROR radome2none(): string length is less then 20 characters received %ld characters\n",length);
		return;
	}

	antenna_type.replace(length - 4, 4, "NONE");
}

map<string, E_FType> antexCodes =
{
	{"G01", F1 },
	{"G02", F2 },
	{"G05", F5 },
	{"R01", G1 },
	{"R02", G2 },
	{"R04", G4 },
	{"R06", G6 },
	{"E01", F1 },
	{"E05", F5 },
	{"E06", F6 },
	{"E07", F7 },
	{"E08", F8 },
	{"C01", F1 },
	{"C02", B1 },
	{"C05", F5 },
	{"C06", B3 },
	{"C07", F7 },
	{"C08", F8 },
	{"J01", F1 },
	{"J02", F2 },
	{"J05", F5 },
	{"J06", F6 },
	{"S01", F1 },
	{"S05", F5 },
	{"I05", F5 },
	{"I09", I9 }
};

/** Read antex file
 */
void readantexf(
	string		filepath,
	Navigation&	nav)
{
	DOCS_REFERENCE(ATX__);

	bool	noazi_flag		= false;
	int		num_azi_rd		= 0;
	int		irms			= 0;

	ifstream fileStream(filepath);
	if (!fileStream)
	{
		BOOST_LOG_TRIVIAL(error)
		<< "Error opening antex file" << filepath << "\n";
		return;
	}

	const PhaseCenterData	pcv0 = {};
	PhaseCenterData			recPcv;
	PhaseCenterData			freqPcv;
	VectorEnu				recPco;
	Vector3d				satPco = Vector3d::Zero();
	string					id;
	GTime					time;

	E_FType	ft	= FTYPE_NONE;
	E_Sys	sys	= E_Sys::NONE;

	while (fileStream)
	{
		string line;

		getline(fileStream, line);

		char* buff = &line[0];

		char* comment = buff + 60;

		if (irms)
			continue;

		// Read in the ANTEX header information

		if (strlen(buff) < 60 )								{	continue;	}
		if (strstr(comment, "ANTEX VERSION / SYST"))		{	continue;	}
		if (strstr(comment, "PCV TYPE / REFANT")) 			{	continue;	}
		if (strstr(comment, "COMMENT")) 					{	continue;	}
		if (strstr(comment, "END OF HEADER"))				{	continue;	}

		// Read in specific Antenna information now

		if (strstr(comment, "START OF ANTENNA"))
		{
			recPcv	= pcv0;
			freqPcv	= pcv0;
			recPco	= Vector3d::Zero();
			satPco	= Vector3d::Zero();
			id		= "";
			time	= GTime::noTime();

			continue;
		}
// 		if (strstr(comment, "END OF ANTENNA"))
// 		{
// 			GTime time = epoch2time(recPcv.tf);
//
// 			continue;
// 		}


		if (strstr(comment, "METH / BY / # / DATE"))
		{
// 			int num_calibrated;
// 			char cal_method[20];
// 			char cal_agency[20];
// 			char cal_date[10];
// 			strncpy(cal_method,	buff,		20);/* Should be CHAMBER or FIELD or ROBOT or COPIED ot CONVERTED */
// 			cal_method[19] = '\0';
// 			strncpy(cal_agency, buff + 20,	20);
// 			cal_agency[19] = '\0';
// 			strncpy(tmp,		buff + 40,	10);
// 			num_calibrated = atoi(tmp);
// 			strncpy(cal_date,	buff + 50,	10);
// 			cal_date[9] = '\0';

			continue;
		}

		if (strstr(comment, "DAZI"))
		{
			char tmp[10];
			strncpy(tmp,buff   ,8);		tmp[8] = '\0';
			recPcv.aziDelta = atof(tmp);

			if (recPcv.aziDelta < 0.0001)	recPcv.naz = 0;
			else							recPcv.naz = (360 / recPcv.aziDelta) + 1;

			continue;
		}

		if (strstr(comment, "SINEX CODE"))
		{
			recPcv.calibModel	.assign(buff,		10);
			continue;
		}

		if (strstr(comment, "TYPE / SERIAL NO"))
		{
			recPcv.type		.assign(buff,		20);
			recPcv.code		.assign(buff+20,	20);
			recPcv.svn		.assign(buff+40,	4);
			recPcv.cospar	.assign(buff+50,	10);

			// stack antenna pco and pcv
			string satId = recPcv.code;
			if (satId.find_first_not_of(' ') == satId.npos)		{ id = recPcv.type;	}
			else												{ id = recPcv.code;	}

			boost::trim_right(id);
			boost::trim_right(recPcv.type);

			continue;
		}

		if (strstr(comment, "ZEN1 / ZEN2 / DZEN"))
		{
			char tmp[10];
			strncpy(tmp, buff,		8);	tmp[8] = '\0'; 	recPcv.zenStart	= atof(tmp);
			strncpy(tmp, buff+8,	7);	tmp[8] = '\0'; 	recPcv.zenStop	= atof(tmp);
			strncpy(tmp, buff+16,	7);	tmp[8] = '\0'; 	recPcv.zenDelta	= atof(tmp);

			recPcv.nz = (recPcv.zenStop - recPcv.zenStart) / recPcv.zenDelta + 1 ;

			continue;
		}

		if (strstr(comment, "# OF FREQUENCIES"))
		{
//			strncpy(tmp, buff,		8);
// 			tmp[8] = '\0';
// 			pcv.nf		= atoi(tmp);

			continue;
		}

		if (strstr(comment, "VALID FROM"))
		{
			/* if (!str2time(buff,0,43,pcv.ts)) continue;*/
			char valid_from[44];
			strncpy(valid_from, buff, 43);	valid_from[43] = '\0';
			char* p = strtok(valid_from, " ");
			int j = 0;
			while (p != nullptr)
			{
				recPcv.tf[j] = (double) atoi(p);
				p = strtok(nullptr, " ");
				j++;
			}

			time = epoch2time(recPcv.tf);

			continue;
		}

		if (strstr(comment, "VALID UNTIL"))
		{
			/* if (!str2time(buff,0,43,pcv.te)) continue;*/
			char valid_until[44];
			strncpy(valid_until, buff   ,43);	valid_until[43] = '\0';
			char* p = strtok(valid_until, " ");
			int j = 0;
			while (p != nullptr)
			{
				recPcv.tu[j] = (double) atoi(p);
				p = strtok(nullptr, " ");
				j++;
			}

			continue;
		}

		if (strstr(comment, "NORTH / EAST / UP"))	// "NORTH / EAST / UP" for receiver and "X / Y / Z" for satellite
		{
			double neu[3];
			if (decodef(buff, 3, neu) < 3)
			{
				continue;
			}

			recPco.n() = neu[0];
			recPco.e() = neu[1];
			recPco.u() = neu[2];

			satPco.x() = neu[0];
			satPco.y() = neu[1];
			satPco.z() = neu[2];

			continue;
		}

		if (strstr(comment, "START OF FREQUENCY"))
		{
			num_azi_rd = 0;
			noazi_flag = false;

			string antexFCode;
			antexFCode.assign(&buff[3], 3);

			sys	= SatSys::sysFromChar(antexFCode[0]);
			ft	= antexCodes[antexFCode];

			freqPcv = recPcv;

			continue;
		}

		if (strstr(comment, "END OF FREQUENCY"))
		{
			noazi_flag	= false;

			nav.pcvMap[id][sys][ft][time]			= freqPcv;
			nav.pcoMap[id][sys][ft][time].recPco	= recPco;
			nav.pcoMap[id][sys][ft][time].satPco	= satPco;

			if (id.size() <= 3) // filters out non-PRNS e.g. "3S-02-TSADM     NONE"
			{
				nav.svnMap[SatSys(id.c_str())][time]	= recPcv.svn;
				nav.blocktypeMap[recPcv.svn]			= recPcv.type;
			}

			continue;
		}

		if (strstr(comment, "START OF FREQ RMS"))	{	irms	= 1;	continue;	}
		if (strstr(comment, "END OF FREQ RMS"))		{	irms	= 0;	continue;	}

		if	(  irms == 0
			&& strstr(buff, "NOAZI"))
		{
			for (int i = 0; i < recPcv.nz; i++)
			{
				int offset = i * 8 + 8;
				char tmp[10];
				strncpy(tmp, buff + offset, 8);		tmp[8]='\0';
				double pcv_val = atof(tmp);
				freqPcv.elMap.push_back(pcv_val * 1e-3);
			}

			noazi_flag = true;

			continue;
		}

		if	(  irms == 0
			&& noazi_flag)
		{
			char tmp[10];
			strncpy(tmp, buff, 8);			tmp[8]='\0';

			for (int i = 0; i < recPcv.nz; i++)
			{
				int offset = i * 8 + 8;
				strncpy(tmp, buff + offset, 8);		tmp[8]='\0';
				double pcv_val = atof(tmp);
				freqPcv.azElMap[num_azi_rd].push_back(pcv_val * 1e-3);
			}
			num_azi_rd++;

			continue;
		}
	}
}
