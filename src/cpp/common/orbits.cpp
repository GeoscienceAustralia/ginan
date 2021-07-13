
#include <algorithm>
#include <chrono>
#include <string>
#include <ctime>

using std::chrono::system_clock;
using std::chrono::time_point;
using std::string;

#include "peaCommitVersion.h"
#include "streamTrace.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "constants.h"
#include "preceph.hpp"
#include "algebra.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "enums.h"

#include "eigenIncluder.hpp"



/* split a string data into data -----------------------------------------------
* args     :       char *p        			I       string pointer
*        			double *data            O       stored data
* ---------------------------------------------------------------------------*/
void strspt(char* p, double* data)
{
	int j = 0;

	char* q = strtok(p, " ");

	while (q != NULL)
	{
		sscanf(q, "%lf", &data[j]);
		j++;
		q = strtok(NULL, " ");
	}

	return;
}

/* read orbit file header ------------------------------------------------------
* args     :       FILE *fp                I       orbit file pointer
*                  orbpod_t *orbpod        O       orbit info
*
* return   :       1-successful, 0-failure
* ---------------------------------------------------------------------------*/
int readorbit_h(FILE* fp, orbpod_t& orbpod)
{
	char buff[5120], *p = NULL;

	while (fgets(buff, sizeof(buff), fp) != NULL)
	{
		if (strstr(buff, "#INFO ") == buff)
		{
			if (strstr(buff, "Satellite ICS") == nullptr)
			{
				orbpod.infoList.push_back(buff + 9);
			}
		}
		if		(strstr(buff, "Epoch Start")			&& (p = strrchr(buff, ':'))) 	{	sscanf(p + 1, "%lf %lf",	&orbpod.startEpoch[0],	&orbpod.startEpoch[1]);	}
		else if (strstr(buff, "Epoch End")				&& (p = strrchr(buff, ':')))	{	sscanf(p + 1, "%lf %lf",	&orbpod.endEpoch[0],	&orbpod.endEpoch[1]);	}
		else if (strstr(buff, "Tabular interval")		&& (p = strrchr(buff, ':'))) 	{	sscanf(p + 1, "%d",			&orbpod.nint);									}
		else if (strstr(buff, "Number of Satellites")	&& (p = strrchr(buff, ':')))	{	sscanf(p + 1, "%d",			&orbpod.numSats);								}
		else if (strstr(buff, "Number of Epochs")		&& (p = strrchr(buff, ':')))
		{
			sscanf(p + 1, "%d", &orbpod.numEpochs);

			/* check whether ts, te and nint agree with ne */
			if	(  orbpod.startEpoch[0]	!= 0
				&& orbpod.endEpoch[0]	!= 0
				&& orbpod.nint			!= 0)
			{
				int calculatedEpochs =	( orbpod.endEpoch	[0]	* 86400	+ orbpod.endEpoch	[1]
										- orbpod.startEpoch	[0]	* 86400	- orbpod.startEpoch	[1]) / orbpod.nint + 1;

				if (calculatedEpochs != orbpod.numEpochs)
				{
					fprintf(stdout,
							"Warning: epoch number %d doesn't match ne=%d, ts=%lf, te=%lf and tint=%d",
							calculatedEpochs,
							orbpod.numEpochs,
							orbpod.startEpoch[0],
							orbpod.endEpoch[0],
							orbpod.nint);
				}
			}
		}
		else if (strstr(buff, "Satellite ICS"))
		{
			return 1;
		}
	}

	return 1;
}

/* read orbit file body --------------------------------------------------------
* args     :       FILE *fp                I       orbit file pointer
*                  orbpod_t *orbpod        O       orbit info
* ---------------------------------------------------------------------------*/
void readorbit_b(FILE* fp, orbpod_t& orbpod)
{
	char 	buff[5120];
	char*	p = NULL;
	double	data[128];

	while (fgets(buff, sizeof(buff), fp) != NULL)
	{
		/* check GPS initial condition */
		if (strstr(buff, "#IC_INFO "))
		{
			vector<string> tokens;

			char* delimiters = " \r\n";
			char* token_ptr = strtok(buff, delimiters);
			while (token_ptr != nullptr)
			{
				tokens.push_back(token_ptr);
				token_ptr = strtok(nullptr, delimiters);
			}

			SatSys Sat;
			for (int i = 0; i < tokens.size(); i++)
			{
				auto token = tokens[i];
				if (token == "PRN:")
				{
					Sat = SatSys(tokens[i+1].c_str());
					continue;
				}

				if (token == "SVN:")
				{
					string svn = tokens[i+1];
					Sat.setSvn(svn);
					continue;
				}

				if (token == "BLK_TYP:")
				{
					string blockType = tokens[i+1];
					Sat.setBlockType(blockType);
					continue;
				}

				if (token == "SRP:")
				{
					auto& satOrbit = orbpod.satOrbitMap[Sat];
					satOrbit.srpModel[0] = tokens[i+1];
					satOrbit.srpModel[1] = tokens[i+2];
					continue;
				}

				if (token == "MASS:")
				{
					auto& satOrbit = orbpod.satOrbitMap[Sat];
					satOrbit.mass = stoi(tokens[i+1]);
					continue;
				}

				if (token == "Nparam:")
				{
					auto& satOrbit = orbpod.satOrbitMap[Sat];
					satOrbit.numUnknowns = stoi(tokens[i+1]);

					for (int j = 0; j < satOrbit.numUnknowns; j++)
					{
						if (i + 3 + j > tokens.size())
						{
							printf("\nError loading orbit parameters\n");
							return;
						}
						satOrbit.parameterNames.push_back(tokens[i + 3 + j]);
					}
				}
			}

			continue;
		}

		if (strstr(buff, "#IC_XYZ  "))
		{
			SatSys Sat = SatSys(buff + 9);

			SatOrbit&		satOrbit		= orbpod.satOrbitMap[Sat];
			InitialOrbit&	initialOrbit	= satOrbit.initialOrbit;

			initialOrbit.initialConds.resize(satOrbit.numUnknowns);

			p = strrchr(buff, 'F');
			p++;
			strspt(p, data);

			for (int j = 0; j < 2;						j++)	initialOrbit.t0[j]				= data[j];
			for (int j = 0; j < satOrbit.numUnknowns;	j++)	initialOrbit.initialConds[j]	= data[j + 2];

			continue;
		}

		if (strstr(buff, "End_of_Header"))
		{
			continue;
		}

		if	(  buff[0] == 'G'
			|| buff[0] == 'R'
			|| buff[0] == 'C'
			|| buff[0] == 'E'
			|| buff[0] == 'J')
		{
			SatSys Sat = SatSys(buff);
			auto& satOrbit = orbpod.satOrbitMap[Sat];

			p = buff + 5;
			strspt(p, data);

			OrbitInfo orbitInfo;
			for (int j = 0; j < 2; j++)
			{
				/* MJD and sec of day */
				orbitInfo.ti[j] = data[j];
			}

			orbitInfo.ti[0] += orbitInfo.ti[1] / 24 / 3600;

			double ep[6];
			jd2ymdhms(orbitInfo.ti[0] + JD2MJD, ep);

			orbitInfo.time		= epoch2time(ep);

			orbitInfo.partials.resize(satOrbit.numUnknowns, 3);

			for (int j = 0; j < 6; j++)
			{
				/* pos, vel and their partials */
				orbitInfo.xcrf[j]		= data[j + 2];
				orbitInfo.xtrf[j]		= data[j + 8];
			}

			int k = 0;
			for (int i = 0; i < satOrbit.numUnknowns;	i++)
			for (int j = 0; j < 3; 						j++)
			{
				orbitInfo.partials(i,j)	= data[14 + k];
				k++;
			}

			satOrbit.orbitInfoList[orbitInfo.time] = orbitInfo;
		}
	}
}

/* read orbit file -------------------------------------------------------------
* args     :       const char *file              I       orbit file path
*                  orbpod_t *orbpod        O       orbit info
*
* return   :       1-successful, 0-failure
* ---------------------------------------------------------------------------*/
int readorbit(
	string file,
	orbpod_t&	orbpod)
{
	FILE* fp = fopen(file.c_str(), "r");

	if (fp == NULL)
	{
		fprintf(stdout, "Warning: opening %s failed\n", file.c_str());
		return 0;
	}

	/* read orbit file header part */
	bool pass = readorbit_h(fp, orbpod);
	if (pass == false)
		return 0;

	/* read orbit file body part */
	readorbit_b(fp, orbpod);

	return 1;
}

/* output orbit header file -----------------------------------------------------
* args     :       char *file              I       orbit file path
*                  orbpod_t *orbpod        I       orbit info
*                  double *xp              I       estimated orbit for output
*                  int n                   I       number of orbital unknowns
* ----------------------------------------------------------------------------*/
void outputOrbit(
	KFState&	kfState)
{
	if (acsConfig.orbfiles.size() == 0)
	{
		return;
	}

	FILE* fp = fopen((acsConfig.orbfiles.front() + "_pea").c_str(), "w");
	if (fp == nullptr)
	{
		fprintf(stdout, "Warning: output orbit file opening error!\n");
		return;
	}

	orbpod_t& orbpod = nav.orbpod;

	std::time_t end_time = system_clock::to_time_t(system_clock::now());

	fprintf(fp, "#INFO    Orbit estimated by PEA\n");
	fprintf(fp, "#INFO    Generated from: PEA (v%s) at %s\n", PEA_COMMIT_VERSION, std::ctime(&end_time));
	for (auto& str : orbpod.infoList)
	{
		fprintf(fp, "#PODINFO %s", str.c_str());
	}
// 	auto first = orbpod.satOrbitMap.begin()->second;
// 	fprintf(fp, "#INFO    Epoch initial conditions:        %20d %20.12f\n", (int)	first.initialOrbit.t0[0],	first.initialOrbit.t0[1]); /* (int)orbpod->ts[0],orbpod->ts[1]); */
// 	fprintf(fp, "#INFO    Epoch Start:                     %20d %20.12f\n", (int)	orbpod.startEpoch[0],		orbpod.startEpoch[1]);
// 	fprintf(fp, "#INFO    Epoch End:                       %20d %20.12f\n", (int)	orbpod.endEpoch[0],			orbpod.endEpoch[1]);
// 	fprintf(fp, "#INFO    Tabular interval (sec):           %3d\n",					orbpod.nint);
// 	fprintf(fp, "#INFO    Number of Epochs:                 %3d\n",					orbpod.numEpochs);
// 	fprintf(fp, "#INFO    Modeling information:\n");
// 	fprintf(fp, "#INFO    EOP file:\n");
// 	fprintf(fp, "#INFO    Number of Satellites:             %3d\n",					orbpod.numSats);
	fprintf(fp, "#INFO    Satellite ICS:\n");

	for (auto& [Sat, satOrbit]	: orbpod.satOrbitMap)
	{
		if (acsConfig.process_sys[Sat.sys] == false)
		{
			continue;
		}

		auto sat = Sat;
		auto& satOpts = acsConfig.getSatOpts(sat);

		vector<double> adjustments(satOrbit.numUnknowns, 0);

		bool pass = true;
		for (int i = 0; i < satOrbit.numUnknowns; i++)
		{
			//get relevant states from kfState

			string name = satOrbit.parameterNames[i];
			KFKey kfKey = {KF::ORBIT_PTS, Sat, std::to_string(100 + i).substr(1) + "_" + name};

			pass &= kfState.getKFValue(kfKey, adjustments[i]);
		}

		if (pass == false)
		{
			fprintf(fp, "#IC_INFO PRN: %s SVN: %s ----> UNHEALTHY <---- \n",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str());
		}

		auto& initial = satOrbit.initialOrbit;

		fprintf(fp, "#IC_INFO PRN: %s SVN: %s BLK_TYP: %-10s MASS: %10.4f SRP: %6s %6s Nparam:  %d -",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				satOrbit.mass,
				satOrbit.srpModel[0].c_str(),
				satOrbit.srpModel[1].c_str(),
				satOrbit.numUnknowns);

		for (int i = 0; i < satOrbit.numUnknowns; i++)
		{
			fprintf(fp, " %s", satOrbit.parameterNames[i].c_str());
		}
		fprintf(fp, "\n");

		/* output adjustments */
		fprintf(fp, "#IC_dXYZ      %s %s %-10s         %14d %14.6f",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				(int)	initial.t0[0],
						initial.t0[1]);

		for (int j = 0; j < satOrbit.numUnknowns; j++)
		{
			fprintf(fp, " %26.16e ", adjustments[j]);
		}
		fprintf(fp, "\n");

		/* output adjustments */
		fprintf(fp, "#IC_XYZ_APR   %s %s %-10s         %14d %14.6f",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				(int)	initial.t0[0],
						initial.t0[1]);

		for (int j = 0; j < satOrbit.numUnknowns; j++)
		{
			InitialState init = initialStateFromConfig(satOpts.orb, j);
			fprintf(fp, " %10.5e ", init.P);
		}
		fprintf(fp, "\n");

		/* output adjusted ICs */
		fprintf(fp, "#IC_XYZ       %s %s %-10s ICRF    %14d %14.6f",
				Sat.id().c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				(int)	initial.t0[0],
						initial.t0[1]);

		for (int j = 0; j < satOrbit.numUnknowns; j++)
		{
			fprintf(fp, " %26.16e ",  adjustments[j] + initial.initialConds[j]);
		}
		fprintf(fp, "\n");
	}

	fclose(fp);
}

int orbPartials(
	Trace&		trace,
	GTime		time,
	Obs&		obs,
	MatrixXd&	interpPartials)
{
#define INTERPLEN 5
#define INTERPCOUNT (2*INTERPLEN + 1)

	if (obs.satOrb_ptr == nullptr)
	{
		tracepde(1, trace, "no orbit %s sat=%2d\n",			time.to_string(0).c_str(), obs.Sat);
		return 0;
	}

	SatOrbit&	satOrbit = *obs.satOrb_ptr;
	auto&		orbitList = satOrbit.orbitInfoList;
	if	( timediff(time, orbitList.begin()	->first)	< -MAXDTE
		||timediff(time, orbitList.rbegin()	->first)	> +MAXDTE)
	{
		tracepde(1, trace, "no orbit %s sat=%2d\n",			time.to_string(0).c_str(), obs.Sat);
		return 0;
	}

	if	(orbitList.size() < INTERPCOUNT)
	{
		tracepde(1, trace, "not enough orbits %s sat=%2d\n",	time.to_string(0).c_str(), obs.Sat);
		return 0;
	}

	interpPartials.resize(satOrbit.numUnknowns, 3);

	//prepare max, min, and start iterators, all some distance from the ends of the list (map)
	auto min_it		= orbitList.begin();						std::advance(min_it,	+INTERPLEN);
	auto max_it		= orbitList.end();							std::advance(max_it,	-INTERPLEN - 1);
	auto start_it	= orbitList.lower_bound(time);

	if (start_it->first < min_it->first)	start_it = min_it;
	if (start_it->first > max_it->first)	start_it = max_it;

	std::advance(start_it,	-INTERPLEN);

	for (int row = 0; row < satOrbit.numUnknowns;	row++)
	for (int col = 0; col < 3;						col++)
	{
		auto orbit_it = start_it;

		//get interpolation parameters
		double p[INTERPCOUNT];
		double t[INTERPCOUNT];

		for (int i = 0; i < INTERPCOUNT; i++, orbit_it++)
		{
			OrbitInfo& orbit = orbit_it->second;

			t[i] = timediff(orbit.time, time);
			p[i] = orbit.partials(row, col);
		}

		interpPartials(row, col) = interppol(t, p, INTERPCOUNT);
	}

	return 1;
}
