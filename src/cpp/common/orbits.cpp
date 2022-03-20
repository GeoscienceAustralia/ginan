
// #pragma GCC optimize ("O0")

#include <algorithm>
#include <iostream>
#include <fstream>    
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
#include "constants.hpp"
#include "ephemeris.hpp"
#include "algebra.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "common.hpp"
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

	while (q)
	{
		sscanf(q, "%lf", &data[j]);
		j++;
		q = strtok(nullptr, " ");
	}

	return;
}

/** read orbit file from POD
 */
int readorbit(
	string		file)			///< POD filename to read
{
	FILE* fp = fopen(file.c_str(), "r");

	if (fp == nullptr)
	{
		BOOST_LOG_TRIVIAL(error) << "Warning: opening " << file << " failed";
		return 0;
	}
	
	char 	buff[5120];
	char*	p = nullptr;
	double	data[128];
	
	while (fgets(buff, sizeof(buff), fp))
	{
		if	(  strstr(buff, "#INFO ")			== buff
			|| strstr(buff, "#INFO_ERP ")		== buff)	{		nav.podInfoList.push_back(((string) "#POD") + (buff + 1));			continue;	}
		if	(  strstr(buff, "#IC_PULSE_INFO ")	== buff)	{		nav.podInfoList.push_back(((string) "#"   ) + (buff + 1));			continue;	}
		
// 		if (strstr(buff, "Epoch Start")				&& (p = strrchr(buff, ':'))) 	{	sscanf(p + 1, "%lf %lf",	&orbpod.startEpoch[0],	&orbpod.startEpoch[1]);		continue;}
// 		if (strstr(buff, "Epoch End")				&& (p = strrchr(buff, ':')))	{	sscanf(p + 1, "%lf %lf",	&orbpod.endEpoch[0],	&orbpod.endEpoch[1]);		continue;}
// 		if (strstr(buff, "Tabular interval")		&& (p = strrchr(buff, ':'))) 	{	sscanf(p + 1, "%d",			&orbpod.nint);										continue;}
// 		if (strstr(buff, "Number of Satellites")	&& (p = strrchr(buff, ':')))	{	sscanf(p + 1, "%d",			&orbpod.numSats);									continue;}
// 		
// 		if (strstr(buff, "Number of Epochs")		&& (p = strrchr(buff, ':')))
// 		{
// 			sscanf(p + 1, "%d", &orbpod.numEpochs);
// 
// 			/* check whether ts, te and nint agree with ne */
// 			if	(  orbpod.startEpoch[0]	!= 0
// 				&& orbpod.endEpoch[0]	!= 0
// 				&& orbpod.nint			!= 0
// 				)
// 			{
// 				int calculatedEpochs =	( orbpod.endEpoch	[0]	* 86400	+ orbpod.endEpoch	[1]
// 										- orbpod.startEpoch	[0]	* 86400	- orbpod.startEpoch	[1]) / orbpod.nint + 1;
// 
// 				if (calculatedEpochs != orbpod.numEpochs)
// 				{
// 					BOOST_LOG_TRIVIAL(warning) 
// 					<< "Warning: epoch number "	<< calculatedEpochs
// 					<< " doesn't match ne="		<< orbpod.numEpochs
// 					<< ", ts=" 					<< orbpod.startEpoch[0]
// 					<< ", te="					<< orbpod.endEpoch[0]
// 					<< " and tint="				<< orbpod.nint
// 					;
// 				}
// 			}
// 			
// 			continue;
// 		}
		
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
					auto& satOrbit = nav.satNavMap[Sat].satOrbit;
					satOrbit.srpModel[0] = tokens[i+1];
					satOrbit.srpModel[1] = tokens[i+2];
					continue;
				}

				if (token == "MASS:")
				{
					auto& satOrbit = nav.satNavMap[Sat].satOrbit;
					satOrbit.mass = stoi(tokens[i+1]);
					continue;
				}

				if (token == "Nparam:")
				{
					auto& satOrbit = nav.satNavMap[Sat].satOrbit;
					satOrbit.numUnknowns = stoi(tokens[i+1]);

					for (int j = 0; j < satOrbit.numUnknowns; j++)
					{
						if (i + 3 + j > tokens.size())
						{
							printf("\nError loading orbit parameters\n");
							return 0;
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

			SatOrbit&		satOrbit		= nav.satNavMap[Sat].satOrbit;
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
			auto& satOrbit = nav.satNavMap[Sat].satOrbit;

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

			satOrbit.orbitInfoMap[orbitInfo.time] = orbitInfo;
		}
	}
	
	fclose(fp);

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
	if (acsConfig.orbfiles.empty())
	{
		return;
	}

	std::ofstream orbitFile(acsConfig.orbfiles.front() + "_pea");
	if (!orbitFile)
	{
		BOOST_LOG_TRIVIAL(error) << "Warning: output orbit file opening error!\n";
		return;
	}

	double ep[6];
	time2epoch(kfState.time, ep);
	double jd	= ymdhms2jd(ep);
	double mjd	= jd - JD2MJD;
	
	std::time_t end_time = system_clock::to_time_t(system_clock::now());

	tracepdeex(0, orbitFile, "#INFO    Orbit estimated by PEA\n");
	tracepdeex(0, orbitFile, "#INFO    Generated from: PEA (v%s) at %s\n", GINAN_COMMIT_VERSION, std::ctime(&end_time));
	
	for (auto& str : nav.podInfoList)
	{
		tracepdeex(0, orbitFile, "%s", str.c_str());
	}
	
	{
		double eopAdjustments[3]	= {};
		double eopVariances[3]		= {};

		bool pass = true;
		int i = 0;
		for (string type : {"XP", "YP", "UT1"})
		{
			KFKey eopKey = {KF::EOP,	{},	type};
			pass &= kfState.getKFValue(eopKey, eopAdjustments[i], &eopVariances[i]);
			i++;
		}

		tracepdeex(0, orbitFile, "#ERP_d0         MJD XP(arcsec)     YP(arcsec)     UT1-UTC(sec):    ");
		if (pass == false)		{	tracepdeex(0, orbitFile, " ----> UNESTIMATED <---- \n");																							}	
		else					{	tracepdeex(0, orbitFile, " %.6f %26.16e %26.16e %26.16e\n", mjd, eopAdjustments[0] / 1000, eopAdjustments[1] / 1000, eopAdjustments[2] / 1000);		}

		tracepdeex(0, orbitFile, "#ERP_sigma      MJD XP(arcsec)     YP(arcsec)     UT1-UTC(sec):    ");
		if (pass == false)		{	tracepdeex(0, orbitFile, " ----> UNESTIMATED <---- \n");																										}	
		else					{	tracepdeex(0, orbitFile, " %.6f %26.16e %26.16e %26.16e\n", mjd, sqrt(eopVariances[0]) / 1000, sqrt(eopVariances[1]) / 1000, sqrt(eopVariances[2]) / 1000);		}
	}
	
	{
		double eopRateAdjustments[3]	= {};
		double eopRateVariances[3]		= {};

		bool pass = true;
		int i = 0;
		for (string type : {"XP", "YP", "UT1"})
		{
			KFKey eopKey = {KF::EOP_RATE,	{},	type};
			pass &= kfState.getKFValue(eopKey, eopRateAdjustments[i], &eopRateVariances[i]);
			i++;
		}
		
		tracepdeex(0, orbitFile, "#ERP_RATE_d0    MJD XP(arcsec/day) YP(arcsec/day) UT1-UTC(sec/day):");
		if (pass == false)		{	tracepdeex(0, orbitFile, " ----> UNESTIMATED <---- \n");																										}	
		else					{	tracepdeex(0, orbitFile, " %.6f %26.16e %26.16e %26.16e\n", mjd, eopRateAdjustments[0] / 1000, eopRateAdjustments[1] / 1000, eopRateAdjustments[2] / 1000);		}

		tracepdeex(0, orbitFile, "#ERP_RATE_sigma MJD XP(arcsec/day) YP(arcsec/day) UT1-UTC(sec/day):");
		if (pass == false)		{	tracepdeex(0, orbitFile, " ----> UNESTIMATED <---- \n");																													}	
		else					{	tracepdeex(0, orbitFile, " %.6f %26.16e %26.16e %26.16e\n", mjd, sqrt(eopRateVariances[0]) / 1000, sqrt(eopRateVariances[1]) / 1000, sqrt(eopRateVariances[2]) / 1000);		}
	}

	tracepdeex(0, orbitFile, "#INFO    Satellite ICS:\n");

	for (auto& [SatId, satNav] : nav.satNavMap)
	{
		SatSys Sat;
		Sat.fromHash(SatId);
		
		auto& satOrbit = satNav.satOrbit;
		
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
			tracepdeex(0, orbitFile, "#IC_INFO PRN: %s SVN: %s ----> UNHEALTHY <---- \n",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str());
		}

		auto& initial = satOrbit.initialOrbit;

		tracepdeex(0, orbitFile, "#IC_INFO PRN: %s SVN: %s BLK_TYP: %-10s MASS: %10.4f SRP: %6s %6s Nparam:  %d -",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				satOrbit.mass,
				satOrbit.srpModel[0].c_str(),
				satOrbit.srpModel[1].c_str(),
				satOrbit.numUnknowns);

		for (int i = 0; i < satOrbit.numUnknowns; i++)
		{
			tracepdeex(0, orbitFile, " %s", satOrbit.parameterNames[i].c_str());
		}
		tracepdeex(0, orbitFile, "\n");

		/* output adjustments */
		tracepdeex(0, orbitFile, "#IC_dXYZ      %s %s %-10s         %14d %14.6f",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				(int)	initial.t0[0],
						initial.t0[1]);

		for (int j = 0; j < satOrbit.numUnknowns; j++)
		{
			tracepdeex(0, orbitFile, " %26.16e ", adjustments[j]);
		}
		tracepdeex(0, orbitFile, "\n");

		/* output adjustments */
		tracepdeex(0, orbitFile, "#IC_XYZ_APR   %s %s %-10s         %14d %14.6f",
				Sat.id()			.c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				(int)	initial.t0[0],
						initial.t0[1]);

		for (int j = 0; j < satOrbit.numUnknowns; j++)
		{
			InitialState init = initialStateFromConfig(satOpts.orb, j);
			tracepdeex(0, orbitFile, " %10.5e ", init.P);
		}
		tracepdeex(0, orbitFile, "\n");

		/* output adjusted ICs */
		tracepdeex(0, orbitFile, "#IC_XYZ       %s %s %-10s ICRF    %14d %14.6f",
				Sat.id().c_str(),
				Sat.svn()			.c_str(),
				Sat.blockType()		.c_str(),
				(int)	initial.t0[0],
						initial.t0[1]);

		for (int j = 0; j < satOrbit.numUnknowns; j++)
		{
			tracepdeex(0, orbitFile, " %26.16e ",  adjustments[j] + initial.initialConds[j]);
		}
		tracepdeex(0, orbitFile, "\n");
	}
}

int orbPartials(
	Trace&		trace,
	GTime		time,
	SatSys		Sat,
	MatrixXd&	interpPartials)
{
#define INTERPCOUNT (2)
#define FORWARDCOUNT	((INTERPCOUNT -0)/2)
#define REVERSECOUNT	((INTERPCOUNT -1)/2)
	
	interpPartials = Vector3d::Zero().transpose();
	
	SatOrbit&	satOrbit		= nav.satNavMap[Sat].satOrbit;
	auto&		orbitInfoMap	= satOrbit.orbitInfoMap;
	
	if	(orbitInfoMap.size() < INTERPCOUNT)
	{
		tracepde(1, trace, "not enough orbits %s sat=%s\n",	time.to_string(0).c_str(), Sat.id().c_str());
		return 0;
	}

	if	(  time - orbitInfoMap.begin()	->first	> +MAXDTE
		|| time - orbitInfoMap.rbegin()	->first	< -MAXDTE)
	{
		tracepde(1, trace, "no orbit %s sat=%s\n",			time.to_string(0).c_str(), Sat.id().c_str());
		return 0;
	}

	interpPartials.resize(satOrbit.numUnknowns, 3);
	
	//prepare max, min, and start iterators, all some distance from the ends of the list (map)
	auto min_it		= orbitInfoMap.begin();						std::advance(min_it,	+FORWARDCOUNT);
	auto max_it		= orbitInfoMap.end();						std::advance(max_it,	-(REVERSECOUNT+1));	//+1 to escape end()
	auto start_it	= orbitInfoMap.lower_bound(time);

	//clip start to the min/max iterators - so it doesnt walk past the ends while advancing
	if (start_it->first > min_it->first)	start_it = min_it;
	if (start_it->first < max_it->first)	start_it = max_it;

	std::advance(start_it,	REVERSECOUNT);
	
// 	printf("time : %ld\nmin  : %ld\nmax  : %ld\nstart: %ld\n", time.time, min_it->first.time, max_it->first.time, start_it->first.time);

	for (int row = 0; row < satOrbit.numUnknowns;	row++)
	for (int col = 0; col < 3;						col++)
	{
		auto orbit_it = start_it;

		//get interpolation parameters
		double p[INTERPCOUNT];
		double t[INTERPCOUNT];

		for (int i = 0; i < INTERPCOUNT; i++, orbit_it--)
		{
			OrbitInfo& orbit = orbit_it->second;

			t[i] = orbit.time - time;
			p[i] = orbit.partials(row, col);
		}

		interpPartials(row, col) = interppol(t, p, INTERPCOUNT);
	}

	return 1;
}


void readegm(
	string 		strEGMCoeFile, 
	EGMCoef& 	egmCoe)
{
	// ifstream egmCoeFile;
    int j = 0, n = 360;
    double val = 0;
	MatrixXd smn = MatrixXd::Zero(361, 361);
	MatrixXd cmn = MatrixXd::Zero(361, 361);

	boost::filesystem::ifstream fileHandler(strEGMCoeFile);

	if (!fileHandler)
	{
		BOOST_LOG_TRIVIAL(error) << "Warning: opening " << strEGMCoeFile << " failed";
		return;
	}

	double tmp;
	while (j <= n){
		for (int i = 0; i <= j; i++){
			fileHandler >> tmp;
			fileHandler >> tmp;
			fileHandler >> tmp;
			cmn(j,i) = tmp;
			// std::cout << std::setw(4) << j << std::setw(4) << i << std::setw(10) << cmn(j,i);
			fileHandler >> tmp;
			smn(j,i) = tmp;
			fileHandler >> tmp;
			fileHandler >> tmp;			
			// std::cout << std::setw(10) << smn(j,i) << std::endl;
		}
		j++;
	}


	egmCoe.smn = smn;
	egmCoe.cmn = cmn;

}


void inertial2Keplers(
			Trace&		trace,
	const	Vector3d&	r,
	const	Vector3d&	v,
			VectorXd&	keplers)
{
// 	std::cout << "\nIN: " << r.transpose() << " " << v.transpose();
	keplers = VectorXd::Zero(KEPLER::NUM);
	
	Vector3d e_r = r.normalized();
	
	//Calculate orbital momentum vector (perpendicular to both position and velocity)
	Vector3d L = r.cross(v);
	
	//Obtain the eccentricity vector
	Vector3d e	= v.cross(L) / MU_GPS - e_r;
	
// 	std::cout << e.transpose() << std::endl;
	double L_x = L(0);
	double L_y = L(1);
	double L_z = L(2);
	
	L.normalize();
	
	//Determine the vector n pointing towards the ascending node (point on arc crossing xy plane?)
	Vector3d n0 = Vector3d(0,0,1).cross(L);
	if (n0.norm() < 0.0001)
	{
		trace << "\n fixingKKKKKKK";
		n0 = Vector3d(1,0,0);
	}
	n0.normalize();

	
	//get another handy vector
	Vector3d n1 = L.cross(n0).normalized();
	
	
	double e_X = e.dot(n0);
	double e_Y = e.dot(n1);
	

	//Determine the orbit eccentricity, which is simply the magnitude of the eccentricity vector e,
	double e_ = e.norm();
	
	if (e_ < 0.000001)
	{
		//if its too small, point it at a valid place and use a normalised version of it.
		e = n0;
		trace << "\n fixingBBB";
	}
	e.normalize();

	
	//Determine the true anomaly (angle between e and r)
	double nu;
	if (r.dot(v) >= 0)	nu = 		+ acos(e.dot(e_r));
	else				nu = 2*PI	- acos(e.dot(e_r));
	
	//Determine the eccentric anomaly
	double E = 2 * atan2(	sqrt(1-e_) * sin(nu/2),
							sqrt(1+e_) * cos(nu/2));
	
	
	//Compute the mean anomaly with help of Kepler’s Equation from the eccentric anomaly E and the eccentricity e
	double M = E - e_ * sin(E);
	
	if (isnan(nu))
	{
		
		std::cout << "\n " << e.transpose() << " " << e_r.transpose() << "\n";
		std::cout << "nu is nan\n";
	}
	if (isnan(e_))
	{
		std::cout << "e_ is nan\n";
	}
	if (isnan(E))
	{
		std::cout << "E is nan\n";
	}
	if (isnan(M))
	{
		std::cout << "M is nan\n";
	}
	
	keplers(KEPLER::LX)	= L_x / MOMENTUM_SCALE;
	keplers(KEPLER::LY)	= L_y / MOMENTUM_SCALE;
	keplers(KEPLER::LZ)	= L_z / MOMENTUM_SCALE;
	keplers(KEPLER::EU)	= e_X;
	keplers(KEPLER::EV)	= e_Y;
	keplers(KEPLER::M )	= M;
}




#define RTOL_KEPLER			1E-14		///< relative tolerance for Kepler equation
#define MAX_ITER_KEPLER		30			///< max number of iteration of Kelpler

void keplers2inertial(
	VectorXd&	keplers0, 
	Vector3d&	pos,
	double&		dM)
{
	Trace& trace = std::cout;
	
	Vector3d L		= Vector3d::Zero();
	Vector2d eee	= Vector2d::Zero();
	
	L[0]		= keplers0[KEPLER::LX] * MOMENTUM_SCALE;
	L[1]		= keplers0[KEPLER::LY] * MOMENTUM_SCALE;
	L[2]		= keplers0[KEPLER::LZ] * MOMENTUM_SCALE;
	eee[0]		= keplers0[KEPLER::EU];
	eee[1]		= keplers0[KEPLER::EV];
	double M	= keplers0[KEPLER::M ];
	
	
	double e_ = eee.norm();
	

	double E		= M;
	double Eprev	= 0;
	int n;
	for (n = 0; n < MAX_ITER_KEPLER; n++)
	{
		std::cout << "\nE: " << n << " " << E << " " << Eprev;
		Eprev = E;
		E -= (E - e_ * sin(E) - M) / (1 - e_ * cos(E));
		
		if (fabs(E - Eprev) < RTOL_KEPLER)
		{
			break;
		}
	}

	if (n >= MAX_ITER_KEPLER)
	{
		std::cout << "iteratios";
		return;
	}


	double nu	= 2 * atan2(	sqrt(1 + e_) * sin(E/2), 
								sqrt(1 - e_) * cos(E/2));
	
			
	if (isnan(nu))
	{
		std::cout << "nu is NAN\n";
	}
	double r	= L.squaredNorm() / MU_GPS / (1 + e_ * cos(nu));
	
	
	
	if (isnan(r))
	{
		std::cout << "r is NAN\n";
	}
	
	L.normalize();
	
	//Determine the vector n pointing towards the ascending node (point on arc crossing xy plane?)
	Vector3d n0 = Vector3d(0,0,1).cross(L);
	if (n0.norm() < 0.0001)
	{
		trace << "\n fixingKKKKKKK";
		n0 = Vector3d(1,0,0);
	}	
	n0.normalize();

	//get another handy vector
	Vector3d n1 = L.cross(n0).normalized();
	
	Vector3d e = eee[0] * n0 + eee[1] * n1;
	
	if (e.norm() < 0.000001)
	{
		//if its too small, point it at a valid place and use a normalised version of it.
		e = n0;
		trace << "\n fixingBBB";
	}
	e.normalize();
	
	double x = r * cos(nu);
	double y = r * sin(nu);
	
	
	double cos_w = n0.dot(e);
	double cos_i = L(2);
	double cos_W = n0(0);
	if		(cos_w > +1)		cos_w = +1;
	else if (cos_w < -1)		cos_w = -1;
	if		(cos_i > +1)		cos_i = +1;
	else if (cos_i < -1)		cos_i = -1;
	if		(cos_W > +1)		cos_W = +1;
	else if (cos_W < -1)		cos_W = -1;
	
	
	double sin_w = sqrt(1 - SQR(cos_w));
	double sin_i = sqrt(1 - SQR(cos_i));
	double sin_W = sqrt(1 - SQR(cos_W));
	
	if (isnan(sin_w))
	{
		std::cout << "sin_w is NAN\n";
	}
	if (isnan(sin_i))
	{
		std::cout << "sin_i is NAN\n";
	}
	if (isnan(sin_W))
	{
		std::cout << "sin_W is NAN\n";
	}
	pos[0] = x * (cos_w * cos_W 	- sin_w * cos_i * sin_W	)	- y * (cos_w * cos_i * sin_W	+ sin_w * cos_W);
	pos[1] = x * (cos_w * sin_W 	+ sin_w * cos_i * cos_W	)	+ y * (cos_w * cos_i * cos_W	- sin_w * sin_W);
	pos[2] = x * (					+ sin_w * sin_i			)	+ y * (cos_w * sin_i);
	
	double A = r / (1 - e_ * cos(E));
	
	dM = sqrt(MU_GPS /A/A/A);
}

void getKeplerPartials(
	VectorXd&	keplers0,
	MatrixXd&	partials)
{
	partials = MatrixXd::Zero(3, 6);
	
	double deltas[6] =
	{
		1, 1, 1, 0.00001, 0.00001, PI/180/1000
	};
	
	//get base position
	Vector3d pos0;
	double dummy;
	keplers2inertial(keplers0, pos0, dummy);
	
	for (int i = 0; i < 6; i++)
	{
		VectorXd keplers1 = keplers0;
		
		keplers1[i] += deltas[i];
		
		
		Vector3d pos1;
		keplers2inertial(keplers1, pos1, dummy);
		
		partials.col(i) = (pos1 - pos0) / deltas[i];
	}
}


void getKeplerInversePartials(
	Vector3d&	pos,
	Vector3d&	vel,
	MatrixXd&	partials)
{
	double deltas[6] =
	{
		20000, 20000, 20000, 10, 10, 10
	};
	
	partials = MatrixXd::Zero(6, 6);

	//get base keplers
	VectorXd keplers0;
	inertial2Keplers(std::cout, pos, vel, keplers0);
		
	for (int i = 0; i < 6; i++)
	{
		Vector3d pos1 = pos;
		Vector3d vel1 = vel;
		
		if (i < 3)		pos1[i]		+= deltas[i];
		else			vel1[i-3]	+= deltas[i];
		
		VectorXd keplers1;
		
		inertial2Keplers(std::cout, pos1, vel1, keplers1);
		
		partials.col(i) = (keplers1 - keplers0) / deltas[i];
	}
}



