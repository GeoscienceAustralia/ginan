

// #pragma GCC optimize ("O0")


#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "satSys.hpp"

#define IONEX_NEXP		-1
#define SINGL_LAY_ERR	0.3

static double	ionexLatmin = -55;
static double	ionexLonmin = 90;
static double	ionexLatinc = 5;
static double	ionexLoninc = 5;
static int		ionexLatres = 15;
static int		ionexLonres = 19;

static int lastIonex = -1;

map<string, long> endLinePos;

static int ionexMapIndex = 0;

double ionVtec(
	Trace&		trace,
	GTime		time,
	VectorPos&	ionPP,
	int			layer,
	double&		vtecstd,
	KFState&	kfState)
{
	switch (acsConfig.ionModelOpts.model)
	{
		case E_IonoModel::SPHERICAL_HARMONICS:		return ionVtecSphhar(trace, time, ionPP, layer, vtecstd, kfState);
		case E_IonoModel::SPHERICAL_CAPS:			return ionVtecSphcap(trace, time, ionPP, layer, vtecstd, kfState);
		case E_IonoModel::BSPLINE:					return ionVtecBsplin(trace, time, ionPP, layer, vtecstd, kfState);
		default:									return 0;
	}
}

void writeIonexHead(
	Trace& ionex)
{
	ionexMapIndex = 1;

	ionexLatinc = acsConfig.ionexGrid.lat_res;
	ionexLoninc = acsConfig.ionexGrid.lon_res;
	ionexLatres = floor(acsConfig.ionexGrid.lat_width / ionexLatinc) + 1;
	ionexLonres = floor(acsConfig.ionexGrid.lon_width / ionexLoninc) + 1;
	ionexLatmin = acsConfig.ionexGrid.lat_centre - ionexLatinc * (ionexLatres - 1) / 2;
	ionexLonmin = acsConfig.ionexGrid.lon_centre - ionexLoninc * (ionexLonres - 1) / 2;

	if (acsConfig.ionModelOpts.layer_heights.empty())
	{
		return;
	}

	double hght1 = acsConfig.ionModelOpts.layer_heights.front()	/ 1000;
	double hght2 = acsConfig.ionModelOpts.layer_heights.back()	/ 1000;
	double dhght = (hght2 - hght1) / (acsConfig.ionModelOpts.layer_heights.size() - 1);

	if (acsConfig.ionModelOpts.layer_heights.size() == 1)
		dhght = 0;

	auto& recOpts = acsConfig.getRecOpts("global");

	tracepdeex(0, ionex, "%8.1f%12s%-20s%-20.20sIONEX VERSION / TYPE\n", 1.1, " ", "I", "GNS");
	tracepdeex(0, ionex, "%-20s%-20s%-20.20sPGM / RUN BY / DATE\n", acsConfig.analysis_software.c_str(), acsConfig.analysis_centre.c_str(), timeGet().to_string(0).c_str());
	tracepdeex(0, ionex, "  %4s%54sMAPPING FUNCTION\n", "COSZ", "");
	tracepdeex(0, ionex, "%8.1f%52sELEVATION CUTOFF\n", recOpts.elevation_mask_deg, " ");
	tracepdeex(0, ionex, "%8.1f%52sBASE RADIUS\n", RE_WGS84 / 1000.0, " ");
	tracepdeex(0, ionex, "%6d%54sMAP DIMENSION\n", acsConfig.ionModelOpts.layer_heights.size() > 1 ? 3 : 2, " ");
	tracepdeex(0, ionex, "  %6.1f%6.1f%6.1f%40sHGT1 / HGT2 / DHGT\n",			hght1,			hght2,			dhght, " ");
	tracepdeex(0, ionex, "  %6.1f%6.1f%6.1f%40sLAT1 / LAT2 / DLAT\n", ionexLatmin + ionexLatinc * (ionexLatres - 1), ionexLatmin, -ionexLatinc, " ");
	tracepdeex(0, ionex, "  %6.1f%6.1f%6.1f%40sLON1 / LON2 / DLON\n", ionexLonmin, ionexLonmin + ionexLoninc * (ionexLonres - 1), ionexLoninc, " ");
	tracepdeex(0, ionex, "%6d%54sEXPONENT\n", IONEX_NEXP, "");
	tracepdeex(0, ionex, "%-60s%s\n", acsConfig.rinex_comment.c_str(),												"COMMENT");
	tracepdeex(0, ionex, "%60sEND OF HEADER\n\n", " ");
}

void writeIonexEpoch(
	Trace&		trace,
	Trace&		ionex,
	GTime		time,
	KFState&	kfState)
{
	GTow tow	= time;
	int timeseg = floor(tow / acsConfig.ionexGrid.time_res);

	if	(  lastIonex >= 0
		&& lastIonex == timeseg)
	{
		return;
	}

	lastIonex = timeseg;

	GEpoch ep = time;
	tracepdeex(4, trace, "  ..Writing IONEX epoch:%6.0f%6.0f%6.0f%6.0f%6.0f%6.0f \n", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);

	tracepdeex(0, ionex, "%6d%54sSTART OF TEC MAP\n", ionexMapIndex, " ");
	tracepdeex(0, ionex, "%6.0f%6.0f%6.0f%6.0f%6.0f%6.0f%24sEPOCH OF CURRENT MAP\n", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5], " ");

	vector<double> tecrmsList;

	for (int ihgt = 0; ihgt < acsConfig.ionModelOpts.layer_heights.size();	ihgt++)
	for (int ilat = 0; ilat < ionexLatres;									ilat++)
	{
		VectorPos ipp;
		ipp[0] = ionexLatmin + (ionexLatres - ilat - 1) * ionexLatinc;

		tracepdeex(0, ionex, "  %6.1f%6.1f%6.1f%6.1f%6.1f%28sLAT/LON1/LON2/DLON/H",
				ipp[0],
				ionexLonmin,
				ionexLonmin + (ionexLonres - 1) * ionexLoninc,
				ionexLoninc,
				acsConfig.ionModelOpts.layer_heights[ihgt] / 1000, " ");

		ipp[0] *= D2R;

		for (int ilon = 0; ilon < ionexLonres; ilon++)
		{
			if (ilon % 16 == 0)
				tracepdeex(0, ionex, "\n");

			ipp[1] = (ionexLonmin + ilon * ionexLoninc) * D2R;

			double var = 0;
			double iono = ionVtec(trace, time, ipp, ihgt, var, kfState) / pow(10, IONEX_NEXP);

			tracepdeex(5, std::cout, "IPP: %8.4f,%9.4f; layr: %1d; delay: %12.6f; var: %.4e\n",
					ipp[0]*R2D,
					ipp[1]*R2D,
					ihgt,
					iono,
					var);

			if (acsConfig.ionModelOpts.layer_heights.size() == 1)
				var += SINGL_LAY_ERR;

			double tecrms = var / pow(10, 2 * IONEX_NEXP);

			if	(  tecrms >  9999
				|| tecrms <= 0 )
			{
				tecrms = 9999;
			}

			if	(  iono > +9999
				|| iono < -9999)
			{
				iono	= 9999;
				tecrms	= 9999;
			}

			tracepdeex(0, ionex, "%5.0f", iono);
			tecrmsList.push_back(tecrms);
		}

		tracepdeex(0, ionex, "\n");
	}

	tracepdeex(0, ionex, "%6d%54sEND OF TEC MAP\n",		ionexMapIndex, " ");
	tracepdeex(0, ionex, "%6d%54sSTART OF RMS MAP\n",	ionexMapIndex, " ");

	tracepdeex(0, ionex, "%6.0f%6.0f%6.0f%6.0f%6.0f%6.0f%24sEPOCH OF CURRENT MAP\n", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5], " ");

	auto it = tecrmsList.begin();

	for (int ihgt = 0; ihgt < acsConfig.ionModelOpts.layer_heights.size();	ihgt++)
	for (int ilat = 0; ilat < ionexLatres;									ilat++)
	{
		tracepdeex(0, ionex, "  %6.1f%6.1f%6.1f%6.1f%6.1f%28sLAT/LON1/LON2/DLON/H",
				ionexLatmin + (ionexLatres - ilat - 1)	* ionexLatinc,
				ionexLonmin,
				ionexLonmin + (ionexLonres - 1)			* ionexLoninc,
				ionexLoninc,
				acsConfig.ionModelOpts.layer_heights[ihgt] / 1000, "");

		for (int ilon = 0; ilon < ionexLonres; ilon++)
		{
			if (ilon % 16 == 0)
				tracepdeex(0, ionex, "\n");

			double tecrms = *it;
			it++;

			tracepdeex(0, ionex, "%5.0f", tecrms);
		}

		tracepdeex(0, ionex, "\n");
	}

	tracepdeex(0, ionex, "%6d%54sEND OF RMS MAP\n", ionexMapIndex, "");

	ionexMapIndex++;
}

void ionexFileWrite(
	Trace&		trace,
	string		filename,
	GTime		time,
	KFState&	kfState)
{
	if (acsConfig.ionModelOpts.model == +E_IonoModel::NONE)		return;
	if (acsConfig.ionModelOpts.model == +E_IonoModel::MEAS_OUT)	return;
	if (acsConfig.ionModelOpts.layer_heights.size()	< 1) 		return;
	if (acsConfig.ionModelOpts.function_order		< 1) 		return;

	double sigma0 = 0;
	KFKey std0Key;
	std0Key.type	= KF::IONOSPHERIC;
	std0Key.num		= 0;
	if (kfState.getKFSigma(std0Key, sigma0) == false)			return;
	if (sigma0 > 1) 											return;

	std::ofstream ionex(filename, std::fstream::in | std::fstream::out);

	ionex.seekp(0, std::ios::end);

	long endFilePos = ionex.tellp();

	if (endFilePos == 0)
	{
		writeIonexHead(ionex);

		endLinePos[filename] = ionex.tellp();
	}

	ionex.seekp(endLinePos[filename]);

	writeIonexEpoch(trace, ionex, time, kfState);

	endLinePos[filename] = ionex.tellp();

	tracepdeex(0, ionex, "%60sEND OF FILE\n", " ");
}

