#include <boost/log/trivial.hpp>

#include "eigenIncluder.hpp"
#include "instrument.hpp"
#include "acsConfig.hpp"
#include "station.hpp"
#include "common.hpp"
#include "gTime.hpp"
#include "sinex.hpp"
#include "EGM96.h"

using std::endl;
using std::ofstream;


/** Reset comments to their default values
 */
void resetCommentsToDefault()
{
	if (!theSinex.tropDesc.				isEmpty)	{theSinex.tropDescCommentList.		clear(); 	theSinex.tropDescCommentList.		push_back("*_________KEYWORD_____________ __VALUE(S)_______________________________________");}								// TROP/DESCRIPTION
	if (!theSinex.map_siteids.			empty())	{theSinex.tropSiteIdCommentList.	clear();	theSinex.tropSiteIdCommentList.		push_back("*STATION__ PT __DOMES__ T _STATION_DESCRIPTION__ _LONGITUDE _LATITUDE_ _HGT_ELI_ HGT_GEOID");}					// SITE/ID
	if (!theSinex.map_receivers.		empty())	{theSinex.tropSiteRecCommentList.	clear(); 	theSinex.tropSiteRecCommentList.	push_back("*STATION__ PT SOLN T DATA_START____ DATA_END______ DESCRIPTION_________ S/N_________________ FIRMWARE___");}		// SITE/RECEIVER
	if (!theSinex.map_antennas.			empty())	{theSinex.tropSiteAntCommentList.	clear(); 	theSinex.tropSiteAntCommentList.	push_back("*STATION__ PT SOLN T DATA_START____ DATA_END______ DESCRIPTION_________ S/N_________________ PCV_MODEL_");}		// SITE/ANTENNA
	if (!theSinex.map_eccentricities.	empty())	{theSinex.tropSiteEccCommentList.	clear(); 	theSinex.tropSiteEccCommentList.	push_back("*                                                      _UP_____ _NORTH__ _EAST___");
																									theSinex.tropSiteEccCommentList.	push_back("*STATION__ PT SOLN T DATA_START____ DATA_END______ REF _MARKER->ARP(m)____________");}							// SITE/ECCENTRICITY
	if (!theSinex.tropSiteCoordMapMap.	empty())	{theSinex.tropSiteCoordCommentList.clear();		theSinex.tropSiteCoordCommentList.	push_back("*STATION__ PT SOLN T __DATA_START__ __DATA_END____ __STA_X_____ __STA_Y_____ __STA_Z_____ SYSTEM REMRK");}		// SITE/COORDINATES
}

/** Set marker name for file header
 */
void setSinexMarkerName(
	string	markerName)		///< 4-char rec ID for PPP, 'MIX' for network
{
	theSinex.markerName = markerName.substr(0,4);
}

/** Write out trop sinex file header
 */
void writeTropHeader(
	ofstream& out)		///< stream to write out
{
	tracepdeex(0, out, "%%=TRO %4.2lf %3s %04d:%03d:%05d %3s %04d:%03d:%05d %04d:%03d:%05d %c %4s\n",
				theSinex.ver,
				theSinex.create_agc,
				theSinex.filedate[0],
				theSinex.filedate[1],
				theSinex.filedate[2],
				theSinex.data_agc,
				theSinex.solution_start_date[0],
				theSinex.solution_start_date[1],
				theSinex.solution_start_date[2],
				theSinex.solution_end_date[0],
				theSinex.solution_end_date[1],
				theSinex.solution_end_date[2],
				theSinex.ObsCode,
				theSinex.markerName);
}

/** Set data for trop description block
 */
void setTropDesc(
	bool isSmoothed)		///< if solution is smoothed (RTS or fixed-lag)
{
	string tropEstMethod;
	if (isSmoothed)			tropEstMethod = "Smoother";
	else 					tropEstMethod = "Filter";

	theSinex.tropDesc.strings	["TROPO MODELING METHOD"]		= tropEstMethod;
	theSinex.tropDesc.strings	["TIME SYSTEM"]					= acsConfig.time_system;
	theSinex.tropDesc.strings	["OCEAN TIDE LOADING MODEL"]	= acsConfig.ocean_tide_loading_model;
	theSinex.tropDesc.strings	["ATMOSPH TIDE LOADING MODEL"]	= acsConfig.atmospheric_tide_loading_model;
	theSinex.tropDesc.strings	["GEOID MODEL"]					= acsConfig.geoid_model;
	theSinex.tropDesc.ints		["TROPO SAMPLING INTERVAL"]		= acsConfig.epoch_interval;
	theSinex.tropDesc.strings	["A PRIORI TROPOSPHERE"]		= acsConfig.model.trop.model._to_string();
	theSinex.tropDesc.strings	["TROPO MAPPING FUNCTION"]		= acsConfig.model.trop.model._to_string();
	theSinex.tropDesc.strings	["GRADS MAPPING FUNCTION"]		= acsConfig.gradient_mapping_function;
	theSinex.tropDesc.ints		["ELEVATION CUTOFF ANGLE"]		= acsConfig.elevation_mask*R2D;

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
int writeTropDesc(
	ofstream& out)		///< stream to write out
{
	out << "+TROP/DESCRIPTION" << endl;
	write_as_comments(out, theSinex.tropDescCommentList);

	for (auto& [key, entry] : theSinex.tropDesc.strings)	tracepdeex(0, out, " %-29s %22s\n",	key,	entry);
	for (auto& [key, entry] : theSinex.tropDesc.ints)		tracepdeex(0, out, " %-29s %22d\n",	key,	entry);
	for (auto& [key, entry] : theSinex.tropDesc.doubles)	tracepdeex(0, out, " %-29s %22f\n",	key,	entry);
	for (auto& [key, entry] : theSinex.tropDesc.vecStrings)
	{
															tracepdeex(0, out, " %-29s",		key);
		for (auto& str : entry)								tracepdeex(0, out, " %6s",			str);
															tracepdeex(0, out, "\n");
	}

	out << "-TROP/DESCRIPTION" << endl;
	return 0;
}

/** Write SITE/ID block
 */
int writeTropSiteId(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	out << "+SITE/ID" << endl;

	write_as_comments(out, theSinex.tropSiteIdCommentList);
	
	for (auto& [id, ssi] : theSinex.map_siteids)
	{
		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}

		if (ssi.used == false)
			continue;

		// Calc rec pos (ECEF)
		double lat	= ssi.lat_deg	+ ssi.lat_min /60.0	+ ssi.lat_sec /60.0/60.0;
		double lon	= ssi.long_deg	+ ssi.long_min/60.0	+ ssi.long_sec/60.0/60.0;
		double ht	= ssi.height;
		double pos[3] = {	lat*D2R,
							lon*D2R,
							ht };
		Vector3d rRec;
		pos2ecef(pos, rRec);

		// Calc ant offset (ECEF)
		// retrieve eccentricitiy - from sinexPerEpochPerStation()
		Sinex_stn_snx_t	stationSinex;
		int result = getstnsnx(id, theSinex.solution_start_date, stationSinex);
		if (result == E_SnxDataMissing::SITE_ID)	continue;	// Station not found in sinex file
		if (result == E_SnxDataMissing::ESTIMATE)	continue;	// Position not found in sinex file

		Vector3d& antdel	= stationSinex.ecc;
		Vector3d dr1;
		enu2ecef(pos, antdel, dr1);

		// Calc ant pos (ECEF), convert to lat/lon/ht
		rRec += dr1;
		ecef2pos(rRec, pos);
		lat	= pos[0]*R2D;
		lon	= pos[1]*R2D;
		ht	= pos[2];

		while (lon < 0)
			lon += 360.0;

		double offset = egm96_compute_altitude_offset(lat, lon);

		tracepdeex(0, out, " %-9s %2s %9s %c %22s %10.6lf %10.6lf %9.3lf %9.3lf\n",
					ssi.sitecode,
					ssi.ptcode,
					ssi.domes,
					ssi.typecode,
					ssi.desc,
					lon,
					lat,
					ht,
					ht + offset);
	}

	out << "-SITE/ID" << endl;

	return 0;
}

/** Write SITE/RECEIVER block
 */
int writeTropSiteRec(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	out << "+SITE/RECEIVER" << endl;

	write_as_comments(out, theSinex.tropSiteRecCommentList);

	for (auto& [id, timemap] : theSinex.map_receivers)
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

		if (receiver.recsn.		empty())	receiver.recsn		= "-----";
		if (receiver.recfirm.	empty())	receiver.recfirm	= "-----";

		tracepdeex(0, out, " %-9s %2s %4s %c %04d:%03d:%05d %04d:%03d:%05d %20s %-20s %s\n",
					receiver.sitecode	.c_str(),
					receiver.ptcode		.c_str(),
					receiver.solnid		.c_str(),
					receiver.typecode,
					receiver.recstart[0],
					receiver.recstart[1],
					receiver.recstart[2],
					receiver.recend[0],
					receiver.recend[1],
					receiver.recend[2],
					receiver.rectype	.c_str(),
					receiver.recsn		.c_str(),
					receiver.recfirm	.c_str());
	}

	out << "-SITE/RECEIVER" << endl;
	return 0;
}

/** Set antenna calibration model data for SITE/ANTENNA block
 */
void setTropSiteAntCalibModelsFromNav()
{
	string defaultStr = "-----";
	for (auto& [site, antmap]	: theSinex.map_antennas)
	for (auto it = antmap.rbegin(); it != antmap.rend(); it++)
	{
		auto& [time, ant] = *it;
		if	( ant.calibModel == ""
			||ant.calibModel == defaultStr)
		{
			ant.calibModel = defaultStr;
			PhaseCenterData* pcd_ptr;
			bool pass = findAntenna(ant.anttype, time, nav, F1, &pcd_ptr);
			if (pass)
				ant.calibModel = pcd_ptr->calibModel;
		}
	}
}

/** Write SITE/ANTENNA block
 */
int writeTropSiteAnt(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	out << "+SITE/ANTENNA" << endl;

	write_as_comments(out, theSinex.tropSiteAntCommentList);

	for (auto& [id, antmap]	: theSinex.map_antennas)
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
					ant.antstart[0],
					ant.antstart[1],
					ant.antstart[2],
					ant.antend[0],
					ant.antend[1],
					ant.antend[2],
					ant.anttype,
					ant.antsn,
					ant.calibModel);
	}

	out << "-SITE/ANTENNA" << endl;
	return 0;
}

/** Write SITE/ECCENTRICITY block
 */
int writeTropSiteEcc(
	ofstream&	out,		///< stream to write out
	string		markerName)	///< recID if individual rec used for soln, else blank
{
	out << "+SITE/ECCENTRICITY" << endl;

	write_as_comments(out, theSinex.tropSiteEccCommentList);

	for (auto& [id, setMap] : theSinex.map_eccentricities)
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
					set.eccstart[0],
					set.eccstart[1],
					set.eccstart[2],
					set.eccend[0],
					set.eccend[1],
					set.eccend[2],
					set.eccrs,
					set.ecc[2],
					set.ecc[1],
					set.ecc[0]);
	}

	out << "-SITE/ECCENTRICITY" << endl;
	return 0;
}

/** Set site coordinates from filter
 */
void setTropSiteCoordsFromFilter(
	map<string, Station>&	stationMap)	///< map of stations used in network
{
// 	Instrument	instrument(__FUNCTION__);

	// Merge rec & network filters
	list<KFState*> kfStatePointers;
	
	if (acsConfig.process_user)
	for (auto& [key, rec] : stationMap)
	{
		kfStatePointers.push_back(&rec.pppState);
	}
	
	theSinex.tropKFState = mergeFilters(kfStatePointers, true);
	
	for (auto& [key, index] : theSinex.tropKFState.kfIndexMap)
	{
		if (key.type != KF::REC_POS)
		{
			continue;
		}
		theSinex.tropKFState.getKFValue(key, theSinex.tropSiteCoordMapMap[key.str][key.num]);
	}
}

/** Write SITE/COORDINATES block
 */
int writeTropSiteCoord(
	ofstream&	out,		///< stream to write out
	string		markerName,	///< recID if individual rec used for soln, else blank
	bool		firstWrite)	///< first time file is being written to (writes header if true)
{
	if (firstWrite)
	{
		out << "+SITE/COORDINATES" << endl;
		write_as_comments(out, theSinex.tropSiteCoordCommentList);
		theSinex.tropSiteCoordBodyFPosMap[markerName] = out.tellp();
	}

	out.seekp(theSinex.tropSiteCoordBodyFPosMap[markerName]);	// Overwrite previous body entries

	for (auto& [id, entry] : theSinex.tropSiteCoordMapMap)
	{
		if	( markerName != "MIX"
			&&id != markerName)
		{
			continue;
		}

		tracepdeex(0, out, " %-9s %2s %4s %c %04d:%03d:%05d %04d:%03d:%05d %12.3lf %12.3lf %12.3lf %6s %5s\n",
					id,
					theSinex.map_siteids[id].ptcode,
					1,
					'P', //note: adjust if station is non-GNSS - see sinex_v201_appendix1_doc for other obs types (e.g. SLR)
					theSinex.solution_start_date[0],
					theSinex.solution_start_date[1],
					theSinex.solution_start_date[2],
					theSinex.solution_end_date[0],
					theSinex.solution_end_date[1],
					theSinex.solution_end_date[2],
					entry[0],
					entry[1],
					entry[2],
					acsConfig.reference_system,
					acsConfig.analysis_agency);
	}

	out << "-SITE/COORDINATES" << endl;
	return 0;
}

/** Set troposphere solution data from filter
 */
void setTropSolsFromFilter(
// 	bool	isUserMode,	///< True: user mode; false: network mode
	bool	isSmoothed)	///< if solution is smoothed (RTS or fixed-lag)
{
	// Retrieve & accumulate KF & preprocessor values
	struct State
	{
		double x	= 0;
		double var	= 0;
	};
	
	map<string, map<string, State>> tropSumMap;	//for summing similar components - eg trop and trop_gm
	
	for (auto& [key, index] : theSinex.tropKFState.kfIndexMap)
	{
		if	( key.type != KF::TROP
			&&key.type != KF::TROP_GM)
		{
			continue;
		}

		string type;
		if		(key.num == 0) type = "TRO"; //zenith
		else if	(key.num == 1) type = "TGN"; //N gradient
		else if	(key.num == 2) type = "TGE"; //E gradient
		
		string typeWet		= type + "WET";
		string typeTotal	= type + "TOT";

		string id = theSinex.map_siteids[key.str].sitecode;
		
		double x	= 0;
		double var	= 0;
		theSinex.tropKFState.getKFValue(key, x, &var);
		
		double oldVar = tropSumMap[id][typeWet].var;
		double newVar = var + oldVar;						//Ref: https://en.wikipedia.org/wiki/Propagation_of_uncertainty#Example_formulae

		switch (key.num)
		{
			case 0: //zenith
			{
				// Add on filter estimates
				tropSumMap[id][typeWet]		.x		+= x;
				tropSumMap[id][typeTotal]	.x		+= x;
				tropSumMap[id][typeWet]		.var	= newVar;
				tropSumMap[id][typeTotal]	.var	= newVar;
				break;
			}

			case 1: //N grad
			case 2: //E grad
			{
				tropSumMap[id][typeWet]	.x		+= x;
				tropSumMap[id][typeWet]	.var	= newVar;
				break;
			}

			default:
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Error: Unknown trop key.num " << key.num;
			}
		}
	}
	
	// Store accumulated trop values for writing out later
	for (auto& [id, entries] : tropSumMap)
	{
		SinexTropSol StationEntry;
		StationEntry.site = id;
		StationEntry.epoch[0] = theSinex.solution_end_date[0];
		StationEntry.epoch[1] = theSinex.solution_end_date[1];
		StationEntry.epoch[2] = theSinex.solution_end_date[2];

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
				
				auto& [key, coords]  = *it;
				
				double ecef[3] = {	coords[0],
									coords[1],
									coords[2]};
				double pos[3];
				ecef2pos(ecef, pos);
				
				double azel[3] = {0,1,0}; // not used in zhd calc, but tropacs has el>0 requirement
				double modelledZhd = tropacs(pos, azel);
				
				entry.x	-= modelledZhd;
			}
				
			StationEntry.solutions.push_back({type, 		 entry.x,		units,	8}); //type, value, units (multiplier), printing width
			StationEntry.solutions.push_back({"STDDEV",	sqrt(entry.var),	1e3,	8});
		}
		theSinex.tropSolList.push_back(StationEntry);
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
	
	theSinex.tropSolCommentList.clear();
	theSinex.tropSolCommentList.push_back("*STATION__ ____EPOCH_____" + headerFields.str());
}

/** Set troposphere solution data
 */
void setTropSols(
	bool	isSmoothed)	///< if solution is smoothed (RTS or fixed-lag)
{
	switch (acsConfig.trop_data_source)
	{
		case E_Ephemeris::KALMAN:
		{
			setTropSolsFromFilter(isSmoothed);
			break;
		}
		default:
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Unrecognised troposphere delay source " << acsConfig.trop_data_source;
		}
	}
	setTropSolCommentList();
}

/** Write TROP/SOLUTION block
 */
int writeTropSol(
	ofstream&	out,		///< stream to write out
	string		markerName,	///< recID if individual rec used for soln, else blank
	bool		firstWrite)	///< first time file is being written to (writes header if true)
{
	if (firstWrite)
	{
		out << "+TROP/SOLUTION" << endl;
		write_as_comments(out, theSinex.tropSolCommentList);
		theSinex.tropSolFootFPosMap[markerName] = out.tellp();
	}
	
	out.seekp(theSinex.tropSolFootFPosMap[markerName]); // Append body entries each epoch, overwriting previous footer
	
	for (auto& entry : theSinex.tropSolList)
	{
		if	( markerName != "MIX"
			&&entry.site != markerName)
		{
			continue;
		}

		tracepdeex(0, out, " %-9s %04d:%03d:%05d",
					entry.site,
					entry.epoch[0],
					entry.epoch[1],
					entry.epoch[2]);
		
		for (auto& solution : entry.solutions)
		{
			out << std::fixed << std::setprecision(2);  // set number of decimal digits to 2
			out << " " << std::setw(solution.width) << solution.value * solution.units;
		}
		out << endl;
	}
	
	theSinex.tropSolList.clear();
	theSinex.tropSolFootFPosMap[markerName] = out.tellp();

	out << "-TROP/SOLUTION" << endl;
	return 0;
}

/** Write troposphere Sinex data to file
 */
int  writeTropSinexToFile(
	string	filename,			///< filename of file to write out
	string	markerName)			///< recID if individual rec used for soln, else blank
{
	ofstream fout(filename, std::fstream::in | std::fstream::out);
	if (!fout)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: Could not open " << filename << " for writing trop sinex";
		
		return 1;
	}

	// Write header if required
	fout.seekp(0, fout.end);					// seek to end of file
	int failure = 0;
	bool firstWrite = (fout.tellp() == 0);	// file is empty if current position is 0
	if (firstWrite)
	{
		resetCommentsToDefault();

																	writeTropHeader		(fout);							write_pretty_line(fout);
		if (!theSinex.refstrings.			empty())	{failure += write_snx_reference	(fout);							write_pretty_line(fout);}
		if (!theSinex.tropDesc.				isEmpty)	{failure += writeTropDesc		(fout);							write_pretty_line(fout);}
		if (!theSinex.map_siteids.			empty())	{failure += writeTropSiteId		(fout, markerName);				write_pretty_line(fout);}
		if (!theSinex.map_receivers.		empty())	{failure += writeTropSiteRec	(fout, markerName);				write_pretty_line(fout);}
		if (!theSinex.map_antennas.			empty())	{failure += writeTropSiteAnt	(fout, markerName);				write_pretty_line(fout);}
		if (!theSinex.map_eccentricities.	empty())	{failure += writeTropSiteEcc	(fout, markerName);				write_pretty_line(fout);}
	}
	if (	!theSinex.tropSiteCoordMapMap.	empty())	{failure += writeTropSiteCoord	(fout, markerName, firstWrite);	write_pretty_line(fout);}
	if (	!theSinex.tropSolList.			empty())	{failure += writeTropSol		(fout, markerName, firstWrite);	write_pretty_line(fout);}
	
	fout << "%=ENDTRO" << endl;

	return failure;
}

void setFileRefData()
{
	theSinex.inputFiles.		clear();
	theSinex.acknowledgements.	clear();
	theSinex.inputHistory.		clear();
	sinex_check_add_ga_reference("Solution parameters", "1.1", true);
}

void setTropHeaderData(
	GTime&	time)		///< epoch of solution
{
	// Set creation time (default this to config. If not set, get it from the first station read)
	boost::posix_time::ptime start_epoch = acsConfig.start_epoch;
	GTime start_time;
	start_time.time = static_cast<int>(boost::posix_time::to_time_t(start_epoch));
	start_time.sec	= 0;
	double	ep[6];
	int		start[3];
	int		end[3];
	time2epoch(start_time, ep);
	epoch2yds(ep, start);
	time2epoch(time, ep);
	epoch2yds(ep, end);
	struct timeval tv;
	struct tm* tmbuf;
	gettimeofday(&tv, NULL);
	tmbuf = gmtime(&tv.tv_sec);
	int create_yds[3]; // create time for Sinex header
	create_yds[0]	= tmbuf->tm_year + 1900;
	create_yds[1]	= tmbuf->tm_yday;
	create_yds[2]	= tmbuf->tm_sec
					+ tmbuf->tm_min		* 60
					+ tmbuf->tm_hour	* 60 * 60;
	string data_agc;
	string contents;
	sinex_update_header(acsConfig.analysis_agency, create_yds, data_agc, start, end, 'P', ' ', contents, 2.00); //Change this if the trop sinex format gets updated
}

/** Export troposphere Sinex data
 */
void outputTropSinex(
	string					filename,	///< filename of file to write out
	GTime					time,		///< epoch of solution
	map<string, Station>&	stationMap,	///< map of stations used in network
	string					markerName,	///< name of station to use ("MIX" for all)
	bool					isSmoothed)	///< if solution is smoothed (RTS or fixed-lag)
{
// 	Instrument instrument(__FUNCTION__);
	
	setFileRefData();
	setTropHeaderData(time);
	replaceTimes(filename, acsConfig.start_epoch);
	
	setTropSiteCoordsFromFilter(stationMap);
	setTropSiteAntCalibModelsFromNav();
	
	setTropSols(isSmoothed);				// requires setTropKFState() & setTropSiteCoordsFromFilter to have been called
	setTropDesc(isSmoothed);				// requires setTropSols() to have been called
	setSinexMarkerName(markerName);
	writeTropSinexToFile(filename, markerName);
}
