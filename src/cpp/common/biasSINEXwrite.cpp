#include "biasSINEX.hpp"

int abs_strt[3] = {};
int abs_nbia = 0;
int abs_updt = 0;
int rel_strt[3] = {};
int rel_nbia = 0;
int rel_updt = 0;

string cod2str(
	E_ObsCode	code, 
	E_MeasType	opt)
{
	if (code == +E_ObsCode::NONE)
		return "";

	string outstr = code._to_string();
	
	char head;
	if (opt == PHAS)	head='L';	
	if (opt == CODE)	head='C';
	
	outstr[0] = head;
	
	return outstr;
}

static void printExponent(
	Trace&	trace, 
	double	d,
	bool	sigma)
{
	/* If someone knows how to make C++ print with just one digit as exponent... */
	int		exponent	= 0;
	double	base   		= 0;
	
	if (d != 0) 
	{
		exponent	= (int) floor( log10( fabs(d) ) );
		base		= d * pow(10, -1 * exponent);
	}

	if (sigma)	tracepdeex(0, trace, " %8.6fE%+01d",	base, exponent);
	else		tracepdeex(0, trace, " %18.15fE%+01d",	base, exponent);
}

/* convert time to yds (doy in bias SINEX starts at 1) */
void sinex_time(
	GTime time, 
	int *yds) 
{
	double ep[6], ep0[6] = {2000, 1, 1, 0, 0, 0};
	time2epoch(time, ep);
	
	ep0[0] = ep[0];
	int toy = (int) timediff(time, epoch2time(ep0));

	yds[0] = (int) ep[0];
	yds[1] = toy / 86400 + 1;		//doy[0]=toy/86400;
	yds[2] = toy % 86400;
}

/* Write bias SINES head */
void write_bSINEX_head(
	GTime	time, 
	Trace&	trace,
	int *	yds0,
	int 	numbiases,
	string	mode,
	int		updatetime, 
	string	type)
{
	int yds[3];
	GTime now = timeget();
	sinex_time(now,yds);

	tracepdeex(0, trace, "%%=BIA 1.00 %3s %4d:%03d:%05d %3s %4d:%03d:%05d", acsConfig.analysis_agency.c_str(), yds[0], yds[1], yds[2], acsConfig.analysis_agency.c_str(), yds0[0], yds0[1], yds0[2]);

	sinex_time(time, yds);
	tracepdeex(0, trace, " %4d:%03d:%05d %8d\n", yds[0], yds[1], yds[2], numbiases);
	tracepdeex(0, trace, "*-------------------------------------------------------------------------------\n");
	tracepdeex(0, trace, "+FILE/REFERENCE\n");
	tracepdeex(0, trace, " DESCRIPTION       AUS, Position Australia, Geoscience Australia\n");			//todo aaron, get name from config
	tracepdeex(0, trace, " OUTPUT            AUS %s estimates for day %3d, %4d\n", mode.c_str(), yds0[1], yds0[0]);
	tracepdeex(0, trace, " CONTACT           ken.harima@ga.gov.au\n");
	tracepdeex(0, trace, " SOFTWARE          Ginan\n");
	tracepdeex(0, trace, " INPUT             Daily 30-sec observations from IGS stations\n");
	tracepdeex(0, trace, "-FILE/REFERENCE\n");
	tracepdeex(0, trace, "*-------------------------------------------------------------------------------\n");
	tracepdeex(0, trace, "+BIAS/DESCRIPTION\n");
	tracepdeex(0, trace, " OBSERVATION_SAMPLING                    %12.0f\n", acsConfig.epoch_interval);
	tracepdeex(0, trace, " PARAMETER_SPACING                       %12d\n", updatetime);
	tracepdeex(0, trace, " DETERMINATION_METHO                     CLOCK_ANALYSIS\n");
	tracepdeex(0, trace, " BIAS_MODE                               %s\n", type.c_str());
	tracepdeex(0, trace, " TIME_SYSTEM                             G  \n");
	//tracepdeex(0, trace, " RECEIVER_CLOCK_REFERENCE_GNSS           G  \n");
	//tracepdeex(0, trace, " SATELLITE_CLOCK_REFERENCE_OBSERVABLES   G  L1C  L2W\n");
	tracepdeex(0, trace, "-BIAS/DESCRIPTION\n");
	tracepdeex(0, trace, "*-------------------------------------------------------------------------------\n");
	tracepdeex(0, trace, "+BIAS/SOLUTION\n");
	tracepdeex(0, trace, "*BIAS SVN_ PRN STATION__ OBS1 OBS2 BIAS_START____ BIAS_END______ UNIT __ESTIMATED_VALUE____ _STD_DEV___\n");
}

/* print bias SINEX data line */
int write_bSINEX_line(
	SinexBias	bias,
	Trace&		trace)
{
	string typstr;
	if		(bias.biasType == +E_BiasType::OSB)		typstr = "OSB ";
	else if	(bias.biasType == +E_BiasType::DSB)		typstr = "DSB ";
	else 
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
	
	string cod1 = cod2str(bias.cod1, bias.measType);
	string cod2 = cod2str(bias.cod2, bias.measType);
	
	tracepdeex(0, trace, " %4s %4s %3s %-9s %-4s %-4s",
			typstr.c_str(), 
			svn.c_str(), 
			sat.c_str(), 
			bias.name.c_str(), 
			cod1.c_str(),
			cod2.c_str());
	
	int	tini[3];
	int tend[3];
	sinex_time(bias.tini, tini);
	sinex_time(bias.tfin, tend);
	
	tracepdeex(0, trace, " %4d:%03d:%05d %4d:%03d:%05d %4s",
			tini[0],
			tini[1],
			tini[2], 
			tend[0],
			tend[1],
			tend[2], 
			"ns");
	
	printExponent(trace,      bias.bias * (1E9/CLIGHT),	false);
	printExponent(trace, sqrt(bias.var) * (1E9/CLIGHT),	true);
	
	tracepdeex(0, trace, "\n");
	return 1;
}

/* -------------- write stored bias output to SINEX file -----------------------
* args     :       Trace   trace       O       debug trace
*                  Gtime   time        I       last epoc
*                  int     opt         I       bias I/O options
* return   :       number of written biases (-1 if file not found)
* ---------------------------------------------------------------------------*/
int write_bias_SINEX(
	Trace&		trace,
	GTime		time,
	string&		biasfile,
	bias_io_opt	opt)
{ 
	tracepdeex(2,trace,"Writing bias SINEX into: %s\n", biasfile);
	
	std::ofstream outputStream(biasfile);
	if (!outputStream)
	{
		tracepdeex(2, trace, "ERROR: cannot open bias SINEX output: %s\n", biasfile);
		return -1;
	} 
	
	int nwrt=0;
	
	if		(opt.OSB_biases)	write_bSINEX_head(time, outputStream, abs_strt, abs_nbia, "OSB", abs_updt, "ABSOLUTE");
	else if	(opt.DSB_biases)	write_bSINEX_head(time, outputStream, rel_strt, rel_nbia, "DSB", rel_updt, "RELATIVE");
	else
	{
		tracepdeex(2, trace, "ERROR: unsupported bias format\n");
		return 0;
	}
	
	for (auto& idObsObsBiasMap			: SINEXBiases_out)
	for (auto& [id,		obsObsBiasMap]	: idObsObsBiasMap)
	for (auto& [code1,	obsBiasMap]		: obsObsBiasMap)
	for (auto& [code2,	biasMap]		: obsBiasMap)
	for (auto& [time,	bias]			: biasMap)
	{	
		if (bias.name.empty() == false			&& opt.REC_biases == false)		continue;	//dont output this because we dont want receivers
		if (bias.name.empty()					&& opt.SAT_biases == false)		continue;	//dont output this because we dont want satellites
		if (bias.biasType == +E_BiasType::OSB	&& opt.OSB_biases == false)		continue;	//dont output this because we dont want OSB biases
		if (bias.biasType == +E_BiasType::DSB	&& opt.DSB_biases == false)		continue;	//dont output this because we dont want DSB biases
			
		nwrt += write_bSINEX_line(bias, trace);
	} 
		
	tracepdeex(0, outputStream, "-BIAS/SOLUTION\n%%=ENDBIA");
	
	return nwrt;
}


/**************************************************************************************************************************/
/**************************************************************************************************************************/
/**************************************************************************************************************************/

/* ----- store bias output to write into bias SINEX files ----------------------
* args     :       Trace   	trace       O       debug trace
					Gtime   	time        I       bias time
*                  double  	bias        I       bias value
*                  double  	variance    I       bias variance
*                  double  	updat       I       update interval (seconds)         to do, Ken: this does not support update intervals of more than a day consider extending this (for WL for example)
* return   :       0 not found, 1 found
* ---------------------------------------------------------------------------*/
void outp_bias(
	Trace&		trace,
	GTime		time,
	E_BiasType	type, 
	string		receiver, 
	SatSys		Sat, 
	E_ObsCode	code1,
	E_ObsCode	code2, 
	double		bias, 
	double		variance,
	double		updateInterval, 
	E_MeasType	measType)
{
	int mxcode = E_ObsCode::NUM_CODES;
	
	if		(type == +E_BiasType::OSB)
	{
		if (abs_updt <= 0) 
			abs_updt = (int)(updateInterval + 0.5);
		
		if (abs_strt[0] == 0)
		{
			sinex_time(time, abs_strt);
			tracepdeex(2,trace,"\nInitializing absolute biases %04d:%03d:%05d", abs_strt[0], abs_strt[1], abs_strt[2]);
		} 
	} 
	else if	(type == +E_BiasType::DSB)
	{
		if (rel_updt <= 0) 
			rel_updt = (int)(updateInterval + 0.5);
		
		if (rel_strt[0] == 0)
		{
			sinex_time(time, rel_strt);
			tracepdeex(2,trace,"\nInitializing relative biases %04d:%03d:%05d", abs_strt[0], abs_strt[1], abs_strt[2]);
		} 
	}
	else
	{
		tracepdeex(2,trace, "ERROR: unsupported bias format\n");
		return;
	}
	
	tracepdeex(2,trace,"\nLoading bias for sat %s %s %d %d ", Sat.id().c_str(), Sat.svn().c_str(), code1, code2);
	
	SinexBias entry;
	int week;
	double tow = time2gpst(time, &week);
	tow = updateInterval * floor(tow / updateInterval);
	
	entry.tini		= gpst2time(week, tow);
	entry.tfin		= gpst2time(week, tow + updateInterval);
	entry.cod1		= code1;
	entry.cod2		= code2;
	entry.measType	= measType;
	
	if		(type == +E_BiasType::OSB)		abs_nbia++;
	else if	(type == +E_BiasType::DSB)		rel_nbia++;
	
	entry.bias	= bias;
	entry.var	= variance;
	entry.slop	= 0;
	entry.slpv	= 0;
	entry.name	= receiver;
	entry.Sat	= Sat;
	
	string id; 
	if (receiver.empty() == false)
	{
		//this seems to be a receiver
		id = receiver;
		id += Sat.sysChar();
	}
	else
	{
		id = Sat.id();
	}
	
	SINEXBiases_out[measType][id][entry.cod1][entry.cod2][entry.tini] = entry;
	
	
	tracepdeex(2, trace,"... new entry %4d %4d", abs_nbia, rel_nbia);
	
	tracepdeex(2, trace,"\n");
}

