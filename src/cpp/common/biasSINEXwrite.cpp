
// #pragma GCC optimize ("O0")

#include "biasSINEX.hpp"
#include "constants.hpp"
#include "GNSSambres.hpp"
#include "ppp.hpp"
#include "ionoModel.hpp"

map<KFKey, map<int, BiasEntry>> SINEXBiases_out;
long int	BottomOfFile = 0;
UYds 		StartTimeofFile;
string		lastBiasSINEXFile = "";

/* Adjust time reference for bias SINEX file (will be UTC by default) */
void convertTime(GTime &time, string system)
{
	if (acsConfig.bias_time_system == "UTC")
		return;
	
	if (acsConfig.bias_time_system == "R")
	{
		time += 10800.0;
		return;
	}
	
	double leap1 = leapSeconds(time);
	double leap2 = leapSeconds(time + leap1 + 1);
	time += leap2;
	
	if (acsConfig.bias_time_system == "C")
		time -= 14;
	
	if (acsConfig.bias_time_system == "TAI")
		time += 19;
	
	return;
}

/** Determine if the bias is an OSB or DSB given observation codes
*/
string	biasType(
	E_ObsCode	code1,		///< Base code of observation for the bias
	E_ObsCode	code2)		///< Secondary code of observation for the bias
{
	if		(code1 != +E_ObsCode::NONE && code2 != +E_ObsCode::NONE)	return "DSB";
	else if	(code1 != +E_ObsCode::NONE && code2 == +E_ObsCode::NONE)	return "OSB";
	else if	(code1 == +E_ObsCode::NONE && code2 != +E_ObsCode::NONE)	return "OSB";
	else																return "NONE";
}

/** Update bias SINEX first line
*/
void updateFirstLine(
	GTime		time, 				///< Time of bias to write
	Trace&		trace,			///< Trace to output to
	int 		numbias)			///< Number of biases to be written
{
	GTime now = timeGet();
	
	convertTime(now,acsConfig.bias_time_system);
	
	UYds  yds_now = now;
	UYds  ydsTime = now;

	trace.seekp(0);
	tracepdeex(0, trace, "%%=BIA 1.00 %3s %4d:%03d:%05d ", acsConfig.analysis_agency.c_str(), yds_now[0], yds_now[1], yds_now[2]); 
	tracepdeex(0, trace, "%3s %4d:%03d:%05d ",acsConfig.analysis_agency.c_str(), StartTimeofFile[0], StartTimeofFile[1], StartTimeofFile[2]);
	tracepdeex(0, trace, "%4d:%03d:%05d A %8d\n", ydsTime[0], ydsTime[1], ydsTime[2], numbias);
}

/** Write bias SINEX head for the first time
*/
void writeBSINEXHeader(
	GTime		time, 				///< Time of bias to write
	Trace&		trace,			///< Trace to output to
	double		updateInterval) 	///< Bias Update Interval (seconds)
{
	GTime tim =time;
	convertTime(tim,acsConfig.bias_time_system);
	StartTimeofFile = tim;
	updateFirstLine(time, trace, 0);
	
	tracepdeex(0, trace, "*-------------------------------------------------------------------------------\n");
	tracepdeex(0, trace, "+FILE/REFERENCE\n");
	tracepdeex(0, trace, " DESCRIPTION       %s, %s\n",acsConfig.analysis_agency.c_str(), acsConfig.analysis_center.c_str());
	tracepdeex(0, trace, " OUTPUT            OSB estimates for day %3d, %4d\n", StartTimeofFile[1], StartTimeofFile[0]);
	tracepdeex(0, trace, " CONTACT           %s\n", acsConfig.ac_contact.c_str());
	tracepdeex(0, trace, " SOFTWARE          %s\n", acsConfig.analysis_program.c_str());
	tracepdeex(0, trace, " INPUT             %s\n", acsConfig.rinex_comment.c_str());
	tracepdeex(0, trace, "-FILE/REFERENCE\n");
	
	
	tracepdeex(0, trace, "*-------------------------------------------------------------------------------\n");
	tracepdeex(0, trace, "+BIAS/DESCRIPTION\n");
	tracepdeex(0, trace, " OBSERVATION_SAMPLING                    %12.0f\n", acsConfig.epoch_interval);
	tracepdeex(0, trace, " PARAMETER_SPACING                       %12.0f\n", updateInterval);
	tracepdeex(0, trace, " DETERMINATION_METHOD                    COMBINED_ANALYSIS\n");
	tracepdeex(0, trace, " BIAS_MODE                               ABSOLUTE\n");
	tracepdeex(0, trace, " TIME_SYSTEM                             %s  \n",acsConfig.bias_time_system.c_str());
	
	E_Sys refConst = acsConfig.receiver_reference_clk;
	tracepdeex(0, trace, " RECEIVER_CLOCK_REFERENCE_GNSS           %c\n", refConst._to_string()[0]);
	
	for (auto& [sys,solve] : acsConfig.solve_amb_for)
	{
		if (!solve) 
			continue;
			
		char sysChar;
		switch (sys)
		{
			case E_Sys::GPS:	sysChar = 'G'; break;
			case E_Sys::GLO:	sysChar = 'R'; break;
			case E_Sys::GAL:	sysChar = 'E'; break;
			case E_Sys::QZS:	sysChar = 'J'; break;
			case E_Sys::BDS:	sysChar = 'C'; break;
			default:			continue;
		}
		
		E_ObsCode code1= acsConfig.clock_codesL1[sys];
		E_ObsCode code2= acsConfig.clock_codesL2[sys];
		tracepdeex(0, trace, " SATELLITE_CLOCK_REFERENCE_OBSERVABLES   %c  %s  %s\n", sysChar,code1._to_string(),code2._to_string());
	}
	tracepdeex(0, trace, "-BIAS/DESCRIPTION\n");
	
	
	tracepdeex(0, trace, "*-------------------------------------------------------------------------------\n");
	tracepdeex(0, trace, "+BIAS/SOLUTION\n");
	tracepdeex(0, trace, "*BIAS SVN_ PRN STATION__ OBS1 OBS2 BIAS_START____ BIAS_END______ UNIT __ESTIMATED_VALUE____ _STD_DEV___\n");
	
	BottomOfFile = trace.tellp();
}

/** print bias SINEX data line
*/
int writeBSINEXLine(
	GTime		time,
	BiasEntry&	bias,			///< Bias entry to write
	Trace&		trace)		///< Stream to output to
{
	string typstr = biasType(bias.cod1, bias.cod2);
	if	( typstr != "OSB")
		return 0;
	
	
	string sat = "   ";
	string svn = "    ";
	if (bias.Sat.prn == 0)
	{
		char sysChar = bias.Sat.sysChar();
		sat[0] = sysChar;
		svn[0] = sysChar;
	}
	else
	{
		sat = bias.Sat.id();
		svn = bias.Sat.svn();
	}
	
	
	string cod1 = code2str(bias.cod1, bias.measType);
	string cod2 = code2str(bias.cod2, bias.measType);
	
	bool newbotton = false;
	if (bias.posInOutFile < 0)
	{
		trace.seekp(BottomOfFile);
		bias.posInOutFile = BottomOfFile;
		newbotton = true;
	}
	else
		trace.seekp(bias.posInOutFile);
	
	tracepdeex(0, trace, " %-4s %4s %3s %-9s %-4s %-4s",
			typstr.c_str(), 
			svn.c_str(), 
			sat.c_str(), 
			bias.name.c_str(), 
			cod1.c_str(),
			cod2.c_str());
	
	GTime tim; 
	tim = bias.tini;
	convertTime(tim,acsConfig.bias_time_system);
	UYds tini = tim;
	
	tim = bias.tfin;
	convertTime(tim,acsConfig.bias_time_system);
	UYds tend = tim;
	
	tracepdeex(0, trace, " %4d:%03d:%05d %4d:%03d:%05d %-4s",
			tini[0],
			tini[1],
			tini[2], 
			tend[0],
			tend[1],
			tend[2], 
			"ns");
	
	tracepdeex(0, trace, " %21.5f",     bias.bias * (1E9/CLIGHT));
	tracepdeex(0, trace, " %11.6f", sqrt(bias.var) * (1E9/CLIGHT));
	
	tracepdeex(0, trace, "\n");
	
	if (newbotton)
		BottomOfFile = trace.tellp();
		
	return 1;
}

/** Adding/updating new bias entry
 */
int addBiasEntry(
	Trace&		trace,
	GTime		tini,
	GTime		tfin,
	KFKey		kfKey,
	E_MeasType	measType,
	double		bias,
	double		var)
{
	auto& biasMap = SINEXBiases_out[kfKey];
	int found = -1;

	for (auto& [ind, bias] : biasMap)
	{
		if (bias.tini 		!= tini)			continue;
		if (bias.measType 	!= measType)		continue;

		found = ind;
		break;
	}
	
	tracepdeex(3,trace,"\n Searched %s bias for %s %2d %s:  ",(measType==CODE)?"CODE ":"PHASE", kfKey.Sat.id().c_str(), kfKey.num, tini.to_string(0).c_str());
	
	if (found >= 0)
	{
		biasMap[found].bias = bias;
		
		if (var < 1e-12)
			var = 1e-12;
		
		biasMap[found].var = var;
		tracepdeex(4,trace," found at index %d: %.4f %.4f", found, bias, var);
	}
	else
	{
		found = biasMap.size();
		BiasEntry entry;
		entry.name		= kfKey.str;
		entry.Sat		= kfKey.Sat;
		entry.cod1		= E_ObsCode::_from_integral(kfKey.num);
		entry.cod2		= E_ObsCode::NONE;
		
		entry.measType	= measType;
		entry.tini		= tini;
		entry.tfin		= tfin;

		entry.bias	= bias;
		entry.var	= var;
		entry.slop	= 0;
		entry.slpv	= 0;
		entry.posInOutFile = -1;
		biasMap[found] = entry;
		
		tracepdeex(4,trace," not found, stored at index %d:  %.4f %.4f", found, bias, var);
	}
	return found;
}

/** Store bias output to write into bias SINEX files
*/
void updateBiasOutput(
	Trace&		trace,			
	GTime		time,			///< Time of bias update 
	StationMap&	stationMap,		///< stations for which to output receiver biases
	E_MeasType	measType)
{
	int nstore = 0;
	double bias;
	double bvar;
	
	double updateRate = 0;
	if (measType == E_MeasType::CODE)	updateRate = acsConfig.ambrOpts.code_output_interval;
	if (measType == E_MeasType::PHAS)	updateRate = acsConfig.ambrOpts.phase_output_interval;
	
	if (updateRate <= 0)
		return;
	
	if (updateRate < 30)
		updateRate = 30;
		
	GWeek	week	= time;
	GTow	tow		= time;
	
	tow = updateRate * floor(tow / updateRate);
	GTime tini = gpst2time (week, tow);
	GTime tfin = tini + updateRate;
	
	KFKey key;
	if (measType == E_MeasType::CODE)	key.type = KF::CODE_BIAS;
	if (measType == E_MeasType::PHAS)	key.type = KF::PHASE_BIAS;
			
	for (E_Sys sys : E_Sys::_values())
	{
		if (acsConfig.process_sys[sys] == false)
			continue;
		
		auto sats = getSysSats(sys);
		
		for (auto& Sat : sats)
		for (auto& obsCode : acsConfig.code_priorities[sys])
		if (queryBiasOutput(trace, time, Sat, "", obsCode, bias, bvar, measType))
		{
			key.Sat 	= Sat;
			key.str 	= "";
			key.num		= obsCode;
			
			if (bias != 0)
				addBiasEntry( trace, tini, tfin, key, measType, bias, bvar);
			else if (key.type == KF::CODE_BIAS
				 && (obsCode == acsConfig.clock_codesL1[Sat.sys]
				  || obsCode == acsConfig.clock_codesL2[Sat.sys]))
			{
				bvar = 1e-12;
				addBiasEntry( trace, tini, tfin, key, measType, bias, bvar);
			}
				
		}
		
		if (acsConfig.ambrOpts.output_rec_bias == false)
			continue;
	
		SatSys sat0;
		sat0.sys = sys;
		sat0.prn = 0;
	
		for (auto& [id, rec] : stationMap)
		for (auto& obsCode : acsConfig.code_priorities[sys])
		if (queryBiasOutput(trace, time, sat0, id, obsCode, bias, bvar, measType))
		{
			key.Sat 	= sat0;
			key.str 	= id;
			key.num		= obsCode;
			
			if (bias != 0)
				addBiasEntry( trace, tini, tfin, key, measType, bias, bvar);
		}
	
	}
}

/** Write stored bias output to SINEX file
 *  Return number of written biases (-1 if file not found)
*/
int writeBiasSinex(
	Trace&		trace,			///< Trace to output to
	GTime		time,			///< Time of bias to write
	string		biasfile,		///< File to write
	StationMap&	stationMap)		///< stations for which to output receiver biases
{ 
	tracepdeex(3,trace,"Writing bias SINEX into: %s %s\n", biasfile.c_str(), time.to_string(0).c_str());
	
	std::ofstream outputStream(biasfile, std::fstream::in | std::fstream::out);
	if (!outputStream)
	{
		tracepdeex(2, trace, "ERROR: cannot open bias SINEX output: %s\n", biasfile.c_str());
		return -1;
	} 
	
	if	(  acsConfig.ambrOpts.code_output_interval	<= 0
		&& acsConfig.ambrOpts.phase_output_interval	<= 0)
	{
		return 0;
	}
	
	if (biasfile != lastBiasSINEXFile)
	{
		tracepdeex(3, trace, "\nStarting new bias SINEX file: %s\n", biasfile.c_str());
		
		double updt1;
		double updt2;
		
		if (acsConfig.ambrOpts.code_output_interval > acsConfig.ambrOpts.phase_output_interval)
		{
			updt1 = acsConfig.ambrOpts.code_output_interval;
			updt2 = acsConfig.ambrOpts.phase_output_interval;
		}
		else
		{
			updt1 = acsConfig.ambrOpts.phase_output_interval;
			updt2 = acsConfig.ambrOpts.code_output_interval;
		}
		
		if (updt2==0) 
			updt2 = updt1;
		
		// int week;
		// double tow = time2gpst(time, &week);
		// tow = updt1 * floor (tow / updt1);
		// GTime time0 = gpst2time (week, tow);
		
		GTime time0 = time.floorTime((int)updt1);
		
		SINEXBiases_out.clear();
		
		writeBSINEXHeader(time0, outputStream, updt2);	
		
		lastBiasSINEXFile = biasfile;
	}
	
	if (acsConfig.ambrOpts.code_output_interval		> 0) 		updateBiasOutput(trace, time, stationMap, CODE);
	if (acsConfig.ambrOpts.phase_output_interval	> 0) 		updateBiasOutput(trace, time, stationMap, PHAS);
	
	int numbias=0;
	for (auto& [key, biasMap] : SINEXBiases_out)
	for (auto& [ind, bias]	 : biasMap)
	{
		if	(biasType(bias.cod1, bias.cod2) != "OSB")
			continue;
		
		numbias++;
	}
	
	updateFirstLine(time, outputStream, numbias);
	
	for (auto& [Key, biasMap] : SINEXBiases_out)
	for (auto& [ind, bias]    : biasMap)
	{
		if ((time-bias.tfin) > DTTOL)
			continue;
		
		if (bias.measType!=CODE)
			continue;
		
		tracepdeex(5,trace,"\n CODE bias for %s %2d %s ... at pos %d", bias.Sat.id().c_str(), bias.cod1._to_string(), ind, bias.posInOutFile);
		
		writeBSINEXLine(time, bias, outputStream);
	}
	
	for (auto& [Key, biasMap] : SINEXBiases_out)
	for (auto& [ind, bias]    : biasMap)
	{
		if ((time-bias.tfin) > DTTOL)
			continue;
		
		if (bias.measType==CODE)
			continue;
		
		tracepdeex(5,trace,"\n PHASE bias for %s %2d %s ... at pos %d", bias.Sat.id().c_str(), bias.cod1._to_string(), ind, bias.posInOutFile);
		
		writeBSINEXLine(time, bias, outputStream);
	}
	
	outputStream.seekp(BottomOfFile);
	
	tracepdeex(0, outputStream, "-BIAS/SOLUTION\n%%=ENDBIA");
	
	return numbias;
}

/** Find and combine biases from multiple sources: 
bias inputs, WLNL biases from Ginan 1.0, UC biases from Ginan 2.0 
and DCB from ionosphere modules
*/
bool queryBiasOutput(
	Trace&		trace, 
	GTime		time,
	SatSys		Sat,
	string		Rec,
	E_ObsCode	obsCode, 
	double& 	bias, 
	double& 	variance,
	E_MeasType	type)
{
	bias = 0;
	variance = 0;
	E_FType ftyp = code2Freq[Sat.sys][obsCode];
	
	tracepdeex(2,trace,"\n Searching %s bias for %s %s %s:  ",(type==CODE)?"CODE ":"PHASE", Sat.id().c_str(), obsCode._to_string(), time.to_string(0).c_str());
	
	if (acsConfig.process_ppp)					/* Ginan 2.x	*/
	{
		double pppBias;
		double pppVar;
		
		if (!queryBiasUC(trace, time, Sat, Rec, obsCode, pppBias, pppVar, type))
			return false;
		
		tracepdeex(2,trace,"found UC %.4f %.4e", bias, variance);
		bias		+= pppBias;
		variance	+= pppVar;
	}
	else if (acsConfig.process_network)			/* Ginan 1.x */
	{
		double extBias;
		double extVar;
		getBiasSinex(trace, time, Sat.id(), Sat, obsCode, type, extBias, extVar);
		
		bias		+= extBias;
		variance	+= extVar;
	
		if (bias!=0)
			tracepdeex(4,trace,"found a-priory");
	
		if	(  type == PHAS
			&& acsConfig.ambrOpts.NLmode != +E_ARmode::OFF)
		{
			double WLNLbias;
			double WLNLvar;
			
			if (!queryBiasWLNL(trace, Sat, Rec, ftyp, WLNLbias, WLNLvar))
				return false;
			
			tracepdeex(4,trace," plus WLNL");
			
			bias		+= WLNLbias;
			variance	+= WLNLvar;
		}
	}
	
	/* Ionosphere DCB */
	if	(  type == CODE
		&& acsConfig.process_ionosphere)
	{
		double dcbBias;
		double dcbVar;
		
		if (!queryBiasDCB(trace, Sat, Rec, ftyp, dcbBias, dcbVar))
			return false;
		
		tracepdeex(4,trace," plus DCB");
		
		bias		+= dcbBias;
		variance	+= dcbVar;
	}
	
	return true;
}
