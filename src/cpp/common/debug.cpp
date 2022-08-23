
#pragma GCC optimize ("O0")

#include <iostream>
#include <random>

#include "eigenIncluder.hpp"
#include "common.hpp"

#include "minimumConstraints.hpp"
#include "rtsSmoothing.hpp"
#include "observations.hpp"
#include "algebraTrace.hpp"
#include "streamTrace.hpp"
#include "linearCombo.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "debug.hpp"
#include "sinex.hpp"
#include "enums.h"

std::random_device					randoDev;
std::mt19937						randoGen(randoDev());
std::normal_distribution<double>	rando(0, 15);

void minimumTest(
	Trace&					trace)
{
	GTime gtime;
	gtime++;
	trace << std::endl;
	map<string, Vector3d> pointMap;
	Vector3d p;

	pointMap["STN0"] = {+1,	0,	0};
	pointMap["STN1"] = {-1, 	0,	0};
	pointMap["STN2"] = {0, 	+1,	0};
	pointMap["STN3"] = {0, 	-1,	0};
	pointMap["STN4"] = {0,		0,		+1};
	pointMap["STN5"] = {0,		0,		-1};
// 	pointMap["STN6"] = {1/sqrt(3),		1/sqrt(3),		1/sqrt(3)};
// 	pointMap["DONT"] = {-1/sqrt(2),	-1/sqrt(2),		0};
// 
// 	pointMap["STN4"] *= 0.997;
// 	pointMap["STN5"] *= 0.998;
// 	pointMap["STN6"] *= 0.999;
	
	
	
	KFState kfStateStations;

	map<string, Station> stationMap;
	
	
	//generate fake station data
	{
		KFMeasEntryList stationEntries;
		for (auto& [id, a] : pointMap)
		{
			a *= 5000000 * sqrt(2);
			a += Vector3d(0.0000001,0.0000001,0.0000001);
			
			auto& rec = stationMap[id];
			rec.id = id;
// 			a += Vector3d{0,1000,0};

			for (int i = 0; i < 3; i++)
			{
				rec.snx.pos[i] = a[i];
				rec.aprioriVar(i) = 1;
			}

			double angz = 0.000000001 * 500/2063;
			Matrix3d rotz;
			rotz <<
			+cos(angz),	+sin(angz),	0,
			-sin(angz),	+cos(angz),	0,
			0,			0,			1;
			
			double angx = 0.000000003 * 400 / 6188;
			Matrix3d rotx;
			rotx <<
			1,			0,			0,			
			0,			+cos(angx),	+sin(angx),	
			0,			-sin(angx),	+cos(angx);	
			
			Vector3d p = a;
			p = rotz * p;
			p = rotx * p;
			p += Vector3d{0.001,0.0025,0.003};
// 			p = p * 1.0000001;
// 			trace << "\t" << p(0) << "\t" << p(1) << "\t" << p(2) << std::endl;
			
			rec.aprioriPos = a;
			double ep[6];
			time2epoch(gtime, ep);
			epoch2yds(ep, rec.aprioriTime);
			
			for (int i = 0; i < 3; i++)
			{
				KFMeasEntry meas(&kfStateStations);
			
				KFKey kfKey;
				kfKey.type		= KF::REC_POS;
				kfKey.str		= id;
				kfKey.rec_ptr	= &rec;
				kfKey.num		= i;
				
				meas.addDsgnEntry(kfKey,	1);
				
				double val = p(i) - a(i);
				val +=  stationEntries.size()/1000.0;
				meas.setValue(val);
			
// 				if (stationEntries.size() < 3)
// 					meas.setNoise(5000);
// 				else
					meas.setNoise(100);
					
			
				stationEntries.push_back(meas);
			}
		}

		KFMeasEntry dummyMeas(&kfStateStations);
		dummyMeas.setValue(1);
		dummyMeas.setNoise(100);
// 		dummyMeas.addDsgnEntry({KF::IONOSPHERIC, 	{}, "1", 3},	1);
// 		dummyMeas.addDsgnEntry({KF::REC_POS,		{}, "2", 2},	1);
		stationEntries.push_back(dummyMeas);


		//add process noise to existing states as per their initialisations.
		kfStateStations.stateTransition(trace, gtime);

		KFMeas combinedMeas = kfStateStations.combineKFMeasList(stationEntries);

		/* network parameter estimation */
		if (kfStateStations.lsqRequired)
		{
			kfStateStations.lsqRequired = false;
			trace << std::endl << "-------DOING LEAST SQUARES--------";
			kfStateStations.leastSquareInitStates(trace, combinedMeas);
		}
	}

	kfStateStations.P(3,3) = 400;
	
// 	kfStateStations.P(10,13) = 30;
// 	kfStateStations.P(13,10) = 30;
// 	kfStateStations.P(1,9) = 30;
// 	kfStateStations.P(9,1) = 30;
	kfStateStations.outputStates(trace);
	sinexPostProcessing(kfStateStations.time, stationMap, kfStateStations);
	
// 	getStationsFromSinex(kfStateStations, stationMap);
	
	mincon(trace, kfStateStations);
	
	for (auto [kfKey, index] : kfStateStations.kfIndexMap)
	{
		if (kfKey.type != KF::REC_POS)
		{
			continue;
		}
		if (kfKey.num != 0)
		{
			continue;
		}
		
		Vector3d point1 = kfStateStations.x.segment(index, 3) + pointMap["STN" + std::to_string(index/3)];
		
		for (auto [kfKey, index] : kfStateStations.kfIndexMap)
		{
			if (kfKey.type != KF::REC_POS)
			{
				continue;
			}
			if (kfKey.num != 0)
			{
				continue;
			}
			
			Vector3d point2 = kfStateStations.x.segment(index, 3) + pointMap["STN" + std::to_string(index/3)];
			
			std::cout << std::endl << "Distance:  " << ((point1-point2).norm() - 10000000);
		}
	}
// 	kfStateStations.outputStates(trace);
}

#if 0
#endif

#include "sinex.hpp"
#include "streamTrace.hpp"
#if 0
void outputMeas(
			Trace&		trace,   	///< Trace to output to
			GTime 		time,
			KFMeas		meas)
{
	trace << std::endl << "+MEAS" << std::endl;
	
	tracepdeex(2, trace, "#\t%19s\t%15s\t%15s\t%15s\n", "Time", "Observed", "Meas Noise", "Design Mat");

	for (int i = 0; i < meas.H.rows(); i++)
	{
		tracepdeex(2, trace, "*\t%19s\t%15.4f\t%15.9f", time.to_string(0).c_str(), meas.Y(i), meas.R(i, i));

		for (int j = 0; j < meas.H.cols(); j++)		tracepdeex(2, trace, "\t%15.5f", meas.H(i, j));
		tracepdeex(2, trace, "\n");
	}
	trace << "-MEAS" << std::endl;
}
#endif
// modified from testClockParams() to test chi^2 computation
// std::random_device					randoDev;
// std::mt19937						randoGen(randoDev());
// std::normal_distribution<double>	rando(0, 8);

#if 0
void testOutlierDetection()
{
	GTime gtime;
	gtime++;
	KFState kfState;

	kfState.sigma_check		= false;
	kfState.w_test			= true;
	kfState.chi_square_test	= true;
	kfState.chi_square_mode	= E_ChiSqMode::INNOVATION;
	kfState.sigma_threshold	= 3;
	kfState.max_prefit_remv = 1;

	for (int i = 0; i < 100; i++)
	{
		KFMeasEntryList kfMeasEntryList0;

		for (int j = 0; j < 2; j++)
		{
			KFMeasEntry	codeMeas(&kfState);
			if (i%30 == 20 && j == 1)	codeMeas.setValue(i + rando(randoDev) + 60);	//inject an outlier of 60m
			else						codeMeas.setValue(i + rando(randoDev));
			codeMeas.setNoise(100);

			KFKey recClockKey		= {KF::REC_SYS_BIAS};
			KFKey recClockRateKey	= {KF::REC_SYS_BIAS_RATE};

			InitialState recClkInit		= {0,	SQR(30),	SQR(1)};
			InitialState recClkRateInit	= {0,	SQR(20),	SQR(0.01)};

			codeMeas.addDsgnEntry(recClockKey,						+1,	recClkInit);
			kfState.setKFTransRate(recClockKey, recClockRateKey,	+1,	recClkRateInit);

			kfMeasEntryList0.push_back(codeMeas);
		}

		//inject model failures
		{
			KFKey recClockKey	= {KF::REC_SYS_BIAS};
			if (i == 30)	kfState.setKFValue(recClockKey, 80);
			if (i == 90)	kfState.setKFValue(recClockKey, 50);
		}

		kfState.stateTransition(std::cout, gtime++);

// 		kfState.consolidateKFState();

		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList0);

		std::cout << std::endl << "Epoch #: " << i << std::endl;

		std::cout << std::endl << "Pre-fit:" << std::endl;
		kfState.outputStates(std::cout);
// 		outputMeas(std::cout, gtime, combinedMeas);

		kfState.filterKalman(std::cout, combinedMeas, false);

		std::cout << std::endl << "Post-fit:" << std::endl;
		kfState.outputStates(std::cout);
	}
//	exit(0);
}
#endif


#include "rinexNavWrite.hpp"
#include "acsConfig.hpp"

void doDebugs()
{
// 	minimumTest(std::cout);
// 	exit(0);
}

