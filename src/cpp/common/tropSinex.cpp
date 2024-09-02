#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "inputsOutputs.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "tropModels.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "gTime.hpp"
#include "sinex.hpp"
#include "EGM96.h"

using std::ofstream;


/** Reset comments to their default values
 */
void resetCommentsToDefault()
{
	if (!theSinex.tropDesc				.isEmpty)	{theSinex.blockComments["TROP/DESCRIPTION"]	.clear(); 	theSinex.blockComments["TROP/DESCRIPTION"]	.push_back("*_________KEYWORD_____________ __VALUE(S)_______________________________________");}
	if (!theSinex.mapsiteids			.empty())	{theSinex.blockComments["SITE/ID"]			.clear();	theSinex.blockComments["SITE/ID"]			.push_back("*STATION__ PT __DOMES__ T _STATION_DESCRIPTION__ _LONGITUDE _LATITUDE_ _HGT_ELI_ HGT_GEOID");}
	if (!theSinex.mapreceivers			.empty())	{theSinex.blockComments["SITE/RECEIVER"]	.clear(); 	theSinex.blockComments["SITE/RECEIVER"]		.push_back("*STATION__ PT SOLN T DATA_START____ DATA_END______ DESCRIPTION_________ S/N_________________ FIRMWARE___");}
	if (!theSinex.mapantennas			.empty())	{theSinex.blockComments["SITE/ANTENNA"]		.clear(); 	theSinex.blockComments["SITE/ANTENNA"]		.push_back("*STATION__ PT SOLN T DATA_START____ DATA_END______ DESCRIPTION_________ S/N_________________ PCV_MODEL_");}
	if (!theSinex.mapeccentricities		.empty())	{theSinex.blockComments["SITE/ECCENTRICITY"].clear(); 	theSinex.blockComments["SITE/ECCENTRICITY"]	.push_back("*                                                      _UP_____ _NORTH__ _EAST___");
																											theSinex.blockComments["SITE/ECCENTRICITY"]	.push_back("*STATION__ PT SOLN T DATA_START____ DATA_END______ REF _MARKER->ARP(m)____________");}
	if (!theSinex.tropSiteCoordMapMap	.empty())	{theSinex.blockComments["SITE/COORDINATES"]	.clear();	theSinex.blockComments["SITE/COORDINATES"]	.push_back("*STATION__ PT SOLN T __DATA_START__ __DATA_END____ __STA_X_____ __STA_Y_____ __STA_Z_____ SYSTEM REMRK");}
}

/** Write out trop sinex file header
 */
void writeTropHeader(
	ofstream& out)		///< stream to write out
{
	tracepdeex(0, out, "%%=TRO %4.2lf %3s %04d:%03d:%05d %3s %04d:%03d:%05d %04d:%03d:%05d %c %4s\n",
				theSinex.ver,
				theSinex.createagc,
				theSinex.filedate[0],
				theSinex.filedate[1],
				theSinex.filedate[2],
				theSinex.dataagc,
				theSinex.solutionstartdate[0],
				theSinex.solutionstartdate[1],
				theSinex.solutionstartdate[2],
				theSinex.solutionenddate[0],
				theSinex.solutionenddate[1],
				theSinex.solutionenddate[2],
				theSinex.obsCode,
				theSinex.markerName);
}

/** Set data for trop description block
 */
void setDescription(
	bool isSmoothed)		///< if solution is smoothed (RTS or fixed-lag)
{
	string tropEstMethod;
	if (isSmoothed)			tropEstMethod = "Smoother";
	else 					tropEstMethod = "Filter";

	auto& recOpts = acsConfig.getRecOpts("global");

	theSinex.tropDesc.strings	["TROPO MODELING METHOD"]		= tropEstMethod;
	theSinex.tropDesc.strings	["TIME SYSTEM"]					= acsConfig.time_system;
	theSinex.tropDesc.strings	["OCEAN TIDE LOADING MODEL"]	= acsConfig.ocean_tide_loading_model;
	theSinex.tropDesc.strings	["ATMOSPH TIDE LOADING MODEL"]	= acsConfig.atmospheric_tide_loading_model;
	theSinex.tropDesc.strings	["GEOID MODEL"]					= acsConfig.geoid_model;
	theSinex.tropDesc.ints		["TROPO SAMPLING INTERVAL"]		= acsConfig.epoch_interval;
	theSinex.tropDesc.strings	["A PRIORI TROPOSPHERE"]		= recOpts.tropModel.models.front()._to_string();
	theSinex.tropDesc.strings	["TROPO MAPPING FUNCTION"]		= recOpts.tropModel.models.front()._to_string();
	theSinex.tropDesc.strings	["GRADS MAPPING FUNCTION"]		= acsConfig.gradient_mapping_function;
	theSinex.tropDesc.ints		["ELEVATION CUTOFF ANGLE"]		= recOpts.elevation_mask_deg;
	theSinex.tropDesc.vecStrings["TROPO PARAMETER NAMES"].clear();
	theSinex.tropDesc.vecStrings["TROPO PARAMETER UNITS"].clear();
	theSinex.tropDesc.vecStrings["TROPO PARAMETER WIDTH"].clear();

	if (theSinex.tropSolList.empty() == false)
	for (auto& entry : theSinex.tropSolList.front().solutions)
	{
		theSinex.tropDesc.vecStrings["TROPO PARAMETER NAMES"].push_back(entry.type);
		std::ostringstream unitsSs;
		unitsSs << std::scientific << std::setprecision(0) << entry.units; // get into scientific format
		theSinex.tropDesc.vecStrings["TROPO PARAMETER UNITS"].push_back(unitsSs.str());
		theSinex.tropDesc.vecStrings["TROPO PARAMETER WIDTH"].push_back(std::to_string(entry.width));
	}
	theSinex.tropDesc.isEmpty = false;
}

/** Write TROP/DESCRIPTION block
 */
void writeTropDesc(
	ofstream& out)		///< stream to write out
{
	Block block(out, "TROP/DESCRIPTION");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [key, entry] : theSinex.tropDesc.strings)	tracepdeex(0, out, " %-29s %22s\n",	key,	entry);
	for (auto& [key, entry] : theSinex.tropDesc.ints)		tracepdeex(0, out, " %-29s %22d\n",	key,	entry);
	for (auto& [key, entry] : theSinex.tropDesc.doubles)	tracepdeex(0, out, " %-29s %22f\n",	key,	entry);
	for (auto& [key, entry] : theSinex.tropDesc.vecStrings)
	{
															tracepdeex(0, out, " %-29s",		key);
		for (auto& str : entry)								tracepdeex(0, out, " %6s",			str);
															tracepdeex(0, out, "\n");
	}
}

/** Write SITE/ID block
 */
void writeTropSiteId(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	Block block(out, "SITE/ID");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, ssi] : theSinex.mapsiteids)
	{
		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}
		if (ssi.used == false)
			continue;

		// Retrieve rec pos
		VectorPos pos;
		pos.lat()	= ssi.lat_deg	+ ssi.lat_min / 60.0	+ ssi.lat_sec / 60.0 / 60.0;
		pos.lon()	= ssi.lon_deg	+ ssi.lon_min / 60.0	+ ssi.lon_sec / 60.0 / 60.0;
		pos.hgt()	= ssi.height;

		Vector3d rRec;
		auto it = theSinex.tropSiteCoordMapMap.find(id);
		if (it != theSinex.tropSiteCoordMapMap.end())
		{
			rRec = theSinex.tropSiteCoordMapMap[id];
		}
		else
		{
			rRec = pos2ecef(pos);
		}

		// Calc ant offset (ECEF)
		SinexRecData	stationSinex;

		auto result = getRecSnx(id, theSinex.solutionenddate, stationSinex);

		if (result.failureSiteId)			continue;	// Receiver not found in sinex file
		if (result.failureEstimate)			continue;	// Position not found in sinex file		//todo aaron, remove this, use other function

		VectorEnu&	antdel	= stationSinex.ecc_ptr->ecc;
		Vector3d	dr1		= enu2ecef(pos, antdel);

		// Calc ant pos (ECEF), convert to lat/lon/ht
		rRec += dr1;
		pos = ecef2pos(rRec);
		double lat	= pos.latDeg();
		double lon	= pos.lonDeg();
		double hgt	= pos.hgt();

		while (lon < 0)
			lon += 360;

		double offset = egm96_compute_altitude_offset(lat, lon);

		tracepdeex(0, out, " %-9s %2s %9s %c %22s %10.6lf %10.6lf %9.3lf %9.3lf\n",
					ssi.sitecode,
					ssi.ptcode,
					ssi.domes,
					ssi.typecode,
					ssi.desc,
					lon,
					lat,
					hgt,
					hgt - offset);
	}
}

/** Write SITE/RECEIVER block
 */
void writeTropSiteRec(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	Block block(out, "SITE/RECEIVER");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, timemap] : theSinex.mapreceivers)
	for (auto it = timemap.rbegin(); it != timemap.rend(); it++)
	{
		auto& [time, receiver] = *it;

		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}

		if (receiver.used == false)
			continue;

		if (receiver.sn.	empty())	receiver.sn		= "-----";
		if (receiver.firm.	empty())	receiver.firm	= "-----";

		tracepdeex(0, out, " %-9s %2s %4s %c %04d:%03d:%05d %04d:%03d:%05d %20s %-20s %s\n",
					receiver.sitecode	.c_str(),
					receiver.ptcode		.c_str(),
					receiver.solnid		.c_str(),
					receiver.typecode,
					receiver.start[0],
					receiver.start[1],
					receiver.start[2],
					receiver.end[0],
					receiver.end[1],
					receiver.end[2],
					receiver.type	.c_str(),
					receiver.sn		.c_str(),
					receiver.firm	.c_str());
	}
}

/** Set antenna calibration model data for SITE/ANTENNA block
 */
void setSiteAntCalib()
{
	string defaultStr = "-----";
	for (auto& [site, antmap]	: theSinex.mapantennas)
	for (auto it = antmap.rbegin(); it != antmap.rend(); it++)
	{
		auto& [time, ant] = *it;
		if	( ant.calibModel.empty()
			||ant.calibModel == defaultStr)
		{
			ant.calibModel = defaultStr;
			PhaseCenterData* pcd_ptr;
			bool pass = findAntenna(ant.type, E_Sys::GPS, time, nav, F1, &pcd_ptr);
			if (pass)
				ant.calibModel = pcd_ptr->calibModel;
		}
	}
}

/** Write SITE/ANTENNA block
 */
void writeTropSiteAnt(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	Block block(out, "SITE/ANTENNA");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, antmap]	: theSinex.mapantennas)
	for (auto it = antmap.rbegin(); it != antmap.rend(); it++)
	{
		auto& [time, ant] = *it;

		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}
		if (ant.used == false)
			continue;

		tracepdeex(0, out, " %-9s %2s %4s %c %04d:%03d:%05d %04d:%03d:%05d %20s %-20s %-10s\n",
					ant.sitecode,
					ant.ptcode,
					ant.solnnum,
					ant.typecode,
					ant.start[0],
					ant.start[1],
					ant.start[2],
					ant.end[0],
					ant.end[1],
					ant.end[2],
					ant.type,
					ant.sn,
					ant.calibModel);
	}
}

/** Write SITE/ECCENTRICITY block
 */
void writeTropSiteEcc(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	Block block(out, "SITE/ECCENTRICITY");

	writeAsComments(out, theSinex.blockComments[block.blockName]);

	for (auto& [id, setMap] : theSinex.mapeccentricities)
	for (auto it = setMap.rbegin(); it != setMap.rend(); it++)
	{
		auto& [time, set] = *it;

		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}
		if (set.used == false)
			continue;

		tracepdeex(0, out, " %-9s %2s %4s %c %04d:%03d:%05d %04d:%03d:%05d %3s %8.4lf %8.4lf %8.4lf\n",
					set.sitecode,
					set.ptcode,
					set.solnnum,
					set.typecode,
					set.start[0],
					set.start[1],
					set.start[2],
					set.end[0],
					set.end[1],
					set.end[2],
					set.rs,
					set.ecc[2],
					set.ecc[1],
					set.ecc[0]);
	}
}

/** Write SITE/COORDINATES block
 */
void writeTropSiteCoord(
	ofstream&	out,		///< stream to write out
	string		markerName,	///< recID if individual rec used for soln, else blank
	string		filename)	///< filename of file to write out
{
	long int pos = theSinex.tropSiteCoordBodyFPosMap[filename];
	if (pos == 0)
	{
		out << "+SITE/COORDINATES" << "\n";

		writeAsComments(out, theSinex.blockComments["SITE/COORDINATES"]);

		pos = out.tellp();

		theSinex.tropSiteCoordBodyFPosMap[filename] = pos;
	}

	out.seekp(pos);	// Overwrite previous body entries

	for (auto& [id, entry] : theSinex.tropSiteCoordMapMap)
	{
		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}

		tracepdeex(0, out, " %-9s %2s %4s %c %04d:%03d:%05d %04d:%03d:%05d %12.3lf %12.3lf %12.3lf %6s %5s\n",
					id,
					theSinex.mapsiteids[id].ptcode,
					1,
					'P', //note: adjust if station is non-GNSS - see sinex_v201_appendix1_doc for other obs types (e.g. SLR)
					theSinex.solutionstartdate[0],
					theSinex.solutionstartdate[1],
					theSinex.solutionstartdate[2],
					theSinex.solutionenddate[0],
					theSinex.solutionenddate[1],
					theSinex.solutionenddate[2],
					entry[0],
					entry[1],
					entry[2],
					acsConfig.reference_system,
					acsConfig.analysis_agency);
	}

	out << "-SITE/COORDINATES" << "\n" << "\n";
}

/** Set troposphere solution data from filter
 */
void setTropSolFromFilter(
	KFState&	kfState)	///< KF state
{
	// Retrieve & accumulate KF & preprocessor values
	struct State
	{
		double x	= 0;
		double var	= 0;
	};

	map<string, map<string, State>> tropSumMap;	//for summing similar components - eg trop and trop_gm

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type != KF::TROP
			&&key.type != KF::TROP_GRAD)
		{
			continue;
		}

		string type;
		if		(key.type == KF::TROP)							type = "TRO"; //zenith
		else if	(key.type == KF::TROP_GRAD && key.num == 0)		type = "TGN"; //N gradient
		else if	(key.type == KF::TROP_GRAD && key.num == 1)		type = "TGE"; //E gradient

		string typeWet	= type + "WET";
		string typeTot	= type + "TOT";

		string id = theSinex.mapsiteids[key.str].sitecode;

		double x	= 0;
		double var	= 0;
		kfState.getKFValue(key, x, &var);

		double oldVar = tropSumMap[id][typeWet].var;
		double newVar = var + oldVar;						//Ref: https://en.wikipedia.org/wiki/Propagation_of_uncertainty#Example_formulae

		// Add on filter estimates
		if (key.type == KF::TROP) 		{	tropSumMap[id][typeTot].x += x;		tropSumMap[id][typeTot].var = newVar;	}
										{	tropSumMap[id][typeWet].x += x;		tropSumMap[id][typeWet].var = newVar;	}
	}

	auto& recOpts = acsConfig.getRecOpts("global");

	// Store accumulated trop values for writing out later
	for (auto& [id, entries] : tropSumMap)
	{
		SinexTropSol stationEntry;
		stationEntry.site	= id;
		stationEntry.yds	= theSinex.solutionenddate;

		for (auto& [type, entry] : entries)
		{
			double	units	= 1;
			bool	wet		= false;

			if		(type.substr(type.size() - 3) == "WET")		{	units = 1e3;	wet = true;	}
			else if	(type.substr(type.size() - 3) == "TOT")		{	units = 1e3;				}

			if	( wet
				&&type.substr(0,3) == "TRO")
			{
				auto it = theSinex.tropSiteCoordMapMap.find(id);
				if (it == theSinex.tropSiteCoordMapMap.end())
				{
					BOOST_LOG_TRIVIAL(error)
					<< "Error: theSinex.tropSiteCoordMapMap has no entry for " << id;

					continue;
				}

				VectorEcef ecef = theSinex.tropSiteCoordMapMap[id];

				VectorPos pos = ecef2pos(ecef);

				double modelledZhd = tropDryZTD(nullStream, recOpts.tropModel.models, kfState.time, pos);

				entry.x	-= modelledZhd;
			}

			stationEntry.solutions.push_back({type, 		 entry.x,		units,	8}); //type, value, units (multiplier), printing width
			stationEntry.solutions.push_back({"STDDEV",	sqrt(entry.var),	1e3,	8});
		}
		theSinex.tropSolList.push_back(stationEntry);
	}
}

/** Set troposphere solution header comment
 */
void setTropSolCommentList()
{
	// Adjust trop sol header fields
	std::ostringstream headerFields;

	if (theSinex.tropSolList.empty() == false)
	for (auto& entry : theSinex.tropSolList.front().solutions)
	{
		headerFields << " " << std::setw(entry.width) << entry.type;
	}

	theSinex.blockComments["TROP/SOLUTION"].clear();
	theSinex.blockComments["TROP/SOLUTION"].push_back("*STATION__ ____EPOCH_____" + headerFields.str());
}

/** Set troposphere solution data
 */
void setTropSol(
	KFState&	kfState)	///< KF state
{
	auto source = acsConfig.trop_sinex_data_sources.front();
	switch (source)
	{
		case E_Source::KALMAN:
		{
			setTropSolFromFilter(kfState);
			break;
		}
		default:
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Unrecognised troposphere delay source " << source;
		}
	}
	setTropSolCommentList();
}

/** Write TROP/SOLUTION block
 */
void writeTropSol(
	ofstream&	out,		///< stream to write out
	string		markerName,	///< recID if individual rec used for soln, else blank
	string		filename)	///< filename of file to write out
{
	long int pos = theSinex.tropSolFootFPosMap[filename];
	if (pos == 0)
	{
		out << "+TROP/SOLUTION" << "\n";
		writeAsComments(out, theSinex.blockComments["TROP/SOLUTION"]);

		pos = out.tellp();
		theSinex.tropSolFootFPosMap[filename] = pos;
	}

	out.seekp(pos); // Append body entries each epoch, overwriting previous footer

	for (auto& entry : theSinex.tropSolList)
	{
		if	( markerName != "MIX"
			&&entry.site != markerName)
		{
			continue;
		}

		tracepdeex(0, out, " %-9s %04d:%03d:%05d",
					entry.site,
					entry.yds[0],
					entry.yds[1],
					entry.yds[2]);

		for (auto& solution : entry.solutions)
		{
			out << std::fixed << std::setprecision(2);  // set number of decimal digits to 2
			out << " " << std::setw(solution.width) << solution.value * solution.units;
		}
		out << "\n";
	}

	pos = out.tellp();

	theSinex.tropSolList.clear();
	theSinex.tropSolFootFPosMap[filename] = pos;

	out << "-TROP/SOLUTION" << "\n" << "\n";
}

/** Write troposphere Sinex data to file
 */
void  writeTropSinexToFile(
	string	filename,			///< filename of file to write out
	string	markerName)			///< recID if individual rec used for soln, else blank
{
	ofstream fout(filename, std::fstream::in | std::fstream::out);
	if (!fout)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Could not open " << filename << " for writing trop sinex";

		return;
	}

	// Write header if required
	fout.seekp(0, fout.end);				// seek to end of file

	long int pos = fout.tellp();

	if (pos == 0)
	{
		writeTropHeader(fout);

		if (!theSinex.refstrings.			empty())	{	writeSnxReference	(fout);							}
		if (!theSinex.tropDesc.				isEmpty)	{	writeTropDesc		(fout);							}
		if (!theSinex.mapsiteids.			empty())	{	writeTropSiteId		(fout, markerName);				}
		if (!theSinex.mapreceivers.			empty())	{	writeTropSiteRec	(fout, markerName);				}
		if (!theSinex.mapantennas.			empty())	{	writeTropSiteAnt	(fout, markerName);				}
		if (!theSinex.mapeccentricities.	empty())	{	writeTropSiteEcc	(fout, markerName);				}
	}
	if (	!theSinex.tropSiteCoordMapMap.	empty())	{	writeTropSiteCoord	(fout, markerName, filename);	}
	if (	!theSinex.tropSolList.			empty())	{	writeTropSol		(fout, markerName, filename);	}

	fout << "%=ENDTRO" << "\n";
}

/** Output troposphere SINEX data
 */
void outputTropSinex(
	string					filename,	///< filename of file to write out
	GTime					time,		///< epoch of solution
	KFState&				kfState,	///< KF state
	string					markerName,	///< name of station to use ("MIX" for all)
	bool					isSmoothed)	///< if solution is smoothed (RTS or fixed-lag)
{
	theSinex.markerName = markerName.substr(0,4);
	sinexCheckAddGaReference(	acsConfig.trop_sinex_sol_type,
								acsConfig.analysis_software_version,
								true);

	KFState sinexSubstate = mergeFilters({&kfState}, {KF::ONE, KF::REC_POS, KF::REC_POS_RATE, KF::TROP, KF::TROP_GRAD});

	PTime startTime;
	startTime.bigTime = boost::posix_time::to_time_t(acsConfig.start_epoch);
	string dataAgc;
	string contents;
	updateSinexHeader(	acsConfig.analysis_agency,
						dataAgc,
						(GTime) startTime,
						time,
						acsConfig.trop_sinex_obs_code,
						acsConfig.trop_sinex_const_code,
						contents,
						sinexSubstate.x.rows() - 1,
						acsConfig.trop_sinex_version);

	for (auto& [key, index] : sinexSubstate.kfIndexMap)
	{
		if (key.type != KF::REC_POS)
			continue;

		sinexSubstate.getKFValue(key, theSinex.tropSiteCoordMapMap[key.str][key.num]);
	}

	setSiteAntCalib();

	setTropSol(sinexSubstate);

	setDescription(isSmoothed);

	replaceTimes(filename, acsConfig.start_epoch);

	resetCommentsToDefault();

	writeTropSinexToFile(filename, markerName);
}
