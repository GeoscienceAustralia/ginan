
#pragma GCC optimize ("O0")



#include <iostream>
#include <random>


#include "minimumConstraints.hpp"
#include "eigenIncluder.hpp"
#include "rtsSmoothing.hpp"
#include "observations.hpp"
#include "algebraTrace.hpp"
#include "linearCombo.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "debug.hpp"
#include "sinex.hpp"
#include "trace.hpp"
#include "enums.h"

std::random_device					randoDev;
std::mt19937						randoGen(randoDev());
std::normal_distribution<double>	rando(0, 15);

void minconglob(
	Trace&		trace,
	KFState&	kfStateStations,
	bool		commentSinex = false);

void minimumTest(
	Trace&					trace)
{
	GTime gtime;
	gtime++;
	trace << "\n";
	map<string, Vector3dInit> pointMap;
	Vector3d p;

	if (0)
	{
		pointMap["G10"] = {+1,	0,	0};
		pointMap["G11"] = {-1, 	0,	0};
		pointMap["G12"] = {0, 	+1,	0};
		pointMap["G13"] = {0, 	-1,	0};
		pointMap["G14"] = {0,	0,	+1};
		pointMap["G15"] = {0,	0,	-1};
	}
	else
	{
		pointMap["G01"] = {+1,	0,	0};
		pointMap["G02"] = {+2,	0,	0};
		pointMap["G03"] = {+3,	0,	0.0001};
		pointMap["G04"] = {+4,	0.0002,	0};
		pointMap["G05"] = {+5,	0,	0};
		pointMap["G06"] = {+6,	0,	0};
	}

	ERPValues erpv = getErp(nav.erp, gtime);

	FrameSwapper frameSwapper(gtime, erpv);

// 	pointMap["STN6"] = {1/sqrt(3),		1/sqrt(3),		1/sqrt(3)};
// 	pointMap["DONT"] = {-1/sqrt(2),	-1/sqrt(2),		0};
//
// 	pointMap["STN4"] *= 0.997;
// 	pointMap["STN5"] *= 0.998;
// 	pointMap["STN6"] *= 0.999;


	acsConfig.output_residuals = true;
	KFState kfStateStations;
	kfStateStations.output_residuals = true;

	map<string, Receiver> receiverMap;


	KFKey offSetKey;
	offSetKey.type = KF::CODE_BIAS;

	//generate fake station/orbit data
	{
		KFMeasEntryList stationEntries;
		for (auto orbit : {/*true, */false})
		for (auto [id, a] : pointMap)
		{
			if (orbit)	a *= 26000000;		// * sqrt(2);
			else		a *= 6000000;		// * sqrt(2);
// 			a += Vector3d(0.0000001,0.0000001,0.0000001);

			auto& satNav	= nav.satNavMap[SatSys(id.c_str())];
			auto& rec		= receiverMap[id + "S"];

			if (orbit)
			{
				VectorEcef ecef = a;
				satNav.aprioriPos = frameSwapper(ecef);
			}
			else
			{
				rec.id = id + "S";

				for (int i = 0; i < 3; i++)
				{
					rec.snx.pos[i] = a[i];
					rec.aprioriVar(i) = 1;
				}

				rec.minconApriori = a;
			}

			double angz = 0.000000001 * 5000/2063;
			Matrix3d rotz;
			rotz <<
			+cos(angz),	+sin(angz),	0,
			-sin(angz),	+cos(angz),	0,
			0,			0,			1;

			double angx = 0.000000003 * 4000 / 6188;
			Matrix3d rotx;
			rotx <<
			1,			0,			0,
			0,			+cos(angx),	+sin(angx),
			0,			-sin(angx),	+cos(angx);

			Vector3d p = a;
// 			p = rotz * p;
// 			p = rotx * p;
			p += Vector3d{0.001,0.0025,0.003};

// 			p = p * 1.0000001;
// 			trace << "\t" << p(0) << "\t" << p(1) << "\t" << p(2) << "\n";



			VectorEcef	posEcef	= p;
			VectorEci	posEci	= frameSwapper(posEcef);

			Vector3d v = posEci.cross(Vector3d::UnitZ())/10000 + stationEntries.size() * Vector3d::UnitZ()*10;

// 			posEci += v * 0.0000034;
			for (int i = 0; i < 3; i++)
			{
				KFMeasEntry meas(&kfStateStations);

				auto& rec = receiverMap[id + "S"];
				KFKey kfKey;
				if (orbit)			{kfKey.type		= KF::ORBIT;	kfKey.Sat		= SatSys(id.c_str());	}
				else				{kfKey.type		= KF::REC_POS;	kfKey.str		= id + "S";				}

				kfKey.rec_ptr	= &rec;
				kfKey.num		= i;

				meas.addDsgnEntry(kfKey,	1);


				meas.addDsgnEntry(offSetKey, 1, {.x = 0, .P = 200});

				if (orbit)
					kfStateStations.addKFState((kfKey.num +=3, kfKey), {.x=v(i), .P=1});

				kfKey.type = KF::REC_SYS_BIAS;

				meas.addDsgnEntry(kfKey,	1, {.x = 0, .P = 0.2});

				if (orbit)	meas.setValue(posEci	(i));
				else		meas.setValue(posEcef	(i));

				if (stationEntries.size() < 3)
					meas.setNoise(5);
				else
					meas.setNoise(0.001);

				stationEntries.push_back(meas);
			}
		}

		if (1)
		{
			KFMeasEntry dummyMeas(&kfStateStations);
			dummyMeas.setValue(1);
			dummyMeas.setNoise(1);
			dummyMeas.addDsgnEntry(offSetKey,	1);
			stationEntries.push_back(dummyMeas);
		}

		//add process noise to existing states as per their initialisations.
		kfStateStations.stateTransition(trace, gtime);

		KFMeas combinedMeas(kfStateStations, stationEntries);

		/* network parameter estimation */
		if (kfStateStations.lsqRequired)
		{
			trace << "\n" << "-------DOING LEAST SQUARES--------";
			kfStateStations.leastSquareInitStates(trace, combinedMeas, true);
		}

		kfStateStations.filterKalman(trace, combinedMeas, "", true);

	}

// 	kfStateStations.P(3,3) = 0.4;

// 	kfStateStations.P(40,44) = 1.9;
// 	kfStateStations.P(44,40) = 1.9;
// 	kfStateStations.P(44,44) = 2;
// 	kfStateStations.P(40,40) = 2;
//
// 	kfStateStations.P(13,45) = 1.9;
// 	kfStateStations.P(45,30) = 1.9;			//this fails rigidity test when the orbits constrained is false
// 	kfStateStations.P(13,13) = 2;
// 	kfStateStations.P(45,45) = 2;

// 	kfStateStations.P(1,9) = 30;
// 	kfStateStations.P(9,1) = 30;
	kfStateStations.outputStates(trace);
// 	sinexPostProcessing(kfStateStations.time, receiverMap, kfStateStations);

	// 	kfStateStations.outputCorrelations(trace);

			trace << "\n" << "-------NEW --------";
	{
		auto state = kfStateStations;
		mincon(trace, state);
		state.outputStates(trace, "CONSTRAINED NEW");
	}

	for (auto type : {KF::REC_POS, KF::ORBIT})
	for (auto [kfKey, index] : kfStateStations.kfIndexMap)
	{
		if (kfKey.type != type)
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
			if (kfKey.type != type)
			{
				continue;
			}
			if (kfKey.num != 0)
			{
				continue;
			}

			Vector3d point2 = kfStateStations.x.segment(index, 3) + pointMap["STN" + std::to_string(index/3)];

			double exp;
			if (type == KF::REC_POS)	exp =  6000000 * 2;
			else						exp = 26000000 * 2;
// 			std::cout << "\n" << "Distance:  " << ((point1-point2).norm() - exp);
		}
	}
// 	kfStateStations.outputStates(trace);

// 	kfStateStations.outputCorrelations(trace);
}

#if 0
#include "sinex.hpp"
#if 0
void outputMeas(
			Trace&		trace,   	///< Trace to output to
			GTime 		time,
			KFMeas		meas)
{
	trace << "\n" << "+MEAS" << "\n";

	tracepdeex(2, trace, "#\t%19s\t%15s\t%15s\t%15s\n", "Time", "Observed", "Meas Noise", "Design Mat");

	for (int i = 0; i < meas.H.rows(); i++)
	{
		tracepdeex(2, trace, "*\t%19s\t%15.4f\t%15.9f", time.to_string(0).c_str(), meas.Y(i), meas.R(i, i));

		for (int j = 0; j < meas.H.cols(); j++)		tracepdeex(2, trace, "\t%15.5f", meas.H(i, j));
		tracepdeex(2, trace, "\n");
	}
	trace << "-MEAS" << "\n";
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

	kfState.prefitOpts.sigma_check			= false;
	kfState.prefitOpts.max_iterations 		= 1;
	kfState.prefitOpts.w_test				= true;
	kfState.chiSquareTest.enable			= true;
	kfState.chiSquareTest.mode				= E_ChiSqMode::INNOVATION;
	kfState.chiSquareTest.sigma_threshold	= 3;

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

		std::cout << "\n" << "Epoch #: " << i << "\n";

		std::cout << "\n" << "Pre-fit:" << "\n";
		kfState.outputStates(std::cout);
// 		outputMeas(std::cout, gtime, combinedMeas);

		kfState.filterKalman(std::cout, combinedMeas, false);

		std::cout << "\n" << "Post-fit:" << "\n";
		kfState.outputStates(std::cout);
	}
//	exit(0);
}
#endif


#include "acsConfig.hpp"
#include "mongoWrite.hpp"

void rtsBump()
{
	KFState kfState;
	kfState.rts_basename = "therfe";
	kfState.rts_lag = -1;

	InitialState sinInit;
	sinInit.P = 10000;
	sinInit.Q = 1;

	InitialState ambInit;
	ambInit.P = 10000;

	GTime time;
	time += 60;

	for (int i = 0; i < 1000; i++)
	{
		double amb = 0;
		if (i < 10)			amb = 10;
		else 				amb = 0;

		double sine = sin(i/100.0);

		double cose = cos(i/100.0);

		KFMeasEntryList kfMeasEntryList;

		KFKey sinKey;
		KFKey ambKey;

		sinKey.type	= KF::TROP;
		ambKey.type	= KF::AMBIGUITY;

		{
			KFMeasEntry measEntry(&kfState, {0,{},"two"});

			measEntry.addDsgnEntry(sinKey,	1, sinInit);
			measEntry.addDsgnEntry(ambKey,	1, ambInit);

			measEntry.setValue(amb + sine);
			measEntry.setNoise(1);

			kfMeasEntryList.push_back(measEntry);
		}
		if (i > 100)
		{
			KFMeasEntry measEntry(&kfState, {0,{},"100"});

			measEntry.addDsgnEntry(sinKey,	1, sinInit);

			measEntry.setValue(sine);
			measEntry.setNoise(400);

			kfMeasEntryList.push_back(measEntry);
		}

// 		if (i > 100)
		{
			KFMeasEntry measEntry(&kfState, {0,{},"sin"});
			sinKey.num = 1;
			measEntry.addDsgnEntry(sinKey,	1, sinInit);

			measEntry.setValue(sine);
			measEntry.setNoise(400);

			kfMeasEntryList.push_back(measEntry);
		}

		kfState.output_residuals = true;

		kfState.stateTransition(std::cout, time);

		kfState.outputStates(std::cout);

		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList, time);

		kfState.filterKalman(std::cout, combinedMeas);

		kfState.outputStates(std::cout);

		if (acsConfig.output_mongo_states)
		{
			mongoStates(kfState);
		}

		time++;
	}

	rtsSmoothing(kfState);
}




#include "streamSerial.hpp"
#include "streamFile.hpp"
#include "streamRtcm.hpp"
#include "streamObs.hpp"
#include "streamUbx.hpp"

#endif

#include "mongoWrite.hpp"


//#include "orbitProp.hpp"
//extern map<string, OrbitPropagator>	orbitPropagatorMap;

/** Compare the orbital states created by pseudo-linear state transitions with the original values.
 * The pseudo-linear state transition in the filter (STM + adjustment) is mathematically equivalent to setting a state value directly,
 * but numerical precision in a computer does not allow 100% correspondence - this checks its mostly working
// */
//void checkOrbits(
//	Trace&				trace,
//	KFState&			kfState)
//{
//
//
//	//get current inertial states from the kfState
//	for (auto& [kfKey, index] : kfState.kfIndexMap)
//	{
//		if	( kfKey.type	!= KF::ORBIT
//			||kfKey.num		!= 0)
//		{
//			continue;
//		}
//
//		auto id = kfKey.str;
//
//		auto& orbitPropagator = orbitPropagatorMap[id];
//
//		KFState subState = getOrbitFromState(trace, id, kfState);
//
//		Vector6d inertialState;
//		inertialState << orbitPropagator.states.pos, orbitPropagator.states.vel;
//
//		Vector6d deltaState	= inertialState
//							- subState.x.head(6);
//
//		trace << "\norbitPropagator.base.inertialState"		<< inertialState		.transpose().format(HeavyFmt);
//		trace << "\nsubState.x.head(6)                "		<< subState.x.head(6)	.transpose().format(HeavyFmt);
//		trace << "\ndeltaState                        "		<< deltaState			.transpose().format(HeavyFmt);
//
// 		MatrixXd transition = orbitPropagator.states.posVelSTM;
//
// 		//Convert the absolute transition matrix to an identity matrix (already populated elsewhere) and stm-per-time matrix
// 		transition -= MatrixXd::Identity(transition.rows(), transition.cols());
//
// 		MatrixXd thingy = transition * subState.P * transition.transpose();
//
// 	// 				std::cout << "\ntransition\n" << transition << "\n";
// 	// 				std::cout << "\nthingy\n" << thingy << "\n";
// // 		if (0)
// 		for (auto& [key1, index1] : subState.kfIndexMap)
// 		for (auto& [key2, index2] : subState.kfIndexMap)
// 		{
// 			int index1a = kfState.kfIndexMap[key1];
// 			int index2a = kfState.kfIndexMap[key2];
//
// 			kfState.P(index1a, index2a) += thingy(index1, index2);
// 	// 				kfState.addKFState(key, init);
// 		}
//	}


// 	MatrixXd transition = orbitPropagator.states.posVelSTM;
//
// 	//Convert the absolute transition matrix to an identity matrix (already populated elsewhere) and stm-per-time matrix
// 	transition -= MatrixXd::Identity(transition.rows(), transition.cols());
// 	transition /= 900;
//
// 	MatrixXd thingy = transition * subState.P * transition.transpose();
//
// 		std::cout << "\ntransition\n" << transition << "\n";
// 		std::cout << "\nthingy\n" << thingy << "\n";
// 	for (auto& [key1, index1] : subState.kfIndexMap)
// 	for (auto& [key2, index2] : subState.kfIndexMap)
// 	{
// 		int index1a = kfState.kfIndexMap[key1];
// 		int index2a = kfState.kfIndexMap[key2];
//
// 		kfState.P(index1a, index2a) += thingy(index1, index2);
// // 				kfState.addKFState(key, init);
// 	}
//
//}

// 			//apply required state based accelerations to the matrices before the state transitions
// 			if (0)
// 			for (auto& [accKey, subIndex] : subState.kfIndexMap)
// 			{
// 				Vector3d acceleration;
//
// 				switch (accKey.type)
// 				{
// 					default:
// 					{
// 						//nothing to be done except for state types specified below
// 						continue;
// 					}
// 					case KF::SRP_SCALE:		{	acceleration = outputsMap["accSrp"];	break;	}	//this should be whatever was used in the propagator above, for this srp component only
// 				}
//
// 				std::cout << "\n" << "add acceleration: " << acceleration.transpose() << "\n";
// 				//add state transition elements for each component
// 				for (int i = 0; i < 3; i++)
// 				{
// 					KFKey posKey	= kfKey;
// 					posKey.num		= i;
//
// 					KFKey velKey	= posKey;
// 					velKey.type		= KF::ORBIT;
// 					velKey.num		= i + 3;
//
// 					kfState	.setAccelerator(posKey, velKey, accKey, acceleration(i));
// 					subState.setAccelerator(posKey, velKey, accKey, acceleration(i));
// 				}
// 			}


void timecheck()
{
	PTime	now		= timeGet();
	GTime	gNow	= now;
	UtcTime uNow	= gNow;

	std::cout << "now:   " << now.	bigTime << "\n";
	std::cout << "gNow:  " << gNow.	bigTime << "\n";
	std::cout << "uNow:  " << uNow.	bigTime << "\n";
}

void debugTime()
{
	auto	utcNow	= boost::posix_time::microsec_clock::universal_time();
	GTime	gTime	= timeGet();

	std::cout << std::setprecision(1) << std::fixed << "\n";
	std::cout << "universal_time():   " << utcNow << "\n";
	std::cout << "timeGet():          " << gTime.bigTime << " " << gTime.to_string(1) << "\n";

	PTime	pTime	= gTime;
	auto	bTime	= boost::posix_time::from_time_t((time_t)pTime.bigTime);
	UtcTime utcTime	= gTime;
	GEpoch	gEpoch	= gTime;
	UYds	uYds	= gTime;

	GWeek	gWeek	= gTime;
	GTow	gTow	= gTime;

	BWeek	bWeek	= gTime;
	BTow	bTow	= gTime;

	RTod	rTod	= gTime;

	std::cout << std::setfill('0') << "\n";
	std::cout << "GTime to anything:"   << "\n";
	std::cout << "GTime to PTime:     " << pTime  .bigTime << " " << bTime                << "\n";
	std::cout << "GTime to UtcTime:   " << utcTime.bigTime << " " << utcTime.to_string(1) << "\n";
	std::cout << "GTime to GEpoch:    " << (int)gEpoch.year << "-" << std::setw(2) << (int)gEpoch.month << "-" << std::setw(2) << (int)gEpoch.day << " " << std::setw(2) << (int)gEpoch.hour << ":" << std::setw(2) << (int)gEpoch.min << ":" << std::setw(4) << gEpoch.sec << "\n";
	std::cout << "GTime to UYds:      " << (int)uYds.year << " " << (int)uYds.doy << " " << uYds.sod << "\n";
	std::cout << "GTime to GWeek:     " << gWeek.val << "\n";
	std::cout << "GTime to GTow:      " << gTow. val << "\n";
	std::cout << "GTime to BWeek:     " << bWeek.val << "\n";
	std::cout << "GTime to BTow:      " << bTow. val << "\n";
	std::cout << "GTime to RTod:      " << rTod. val << "\n";


	GTime	gTimeP	= pTime;
	GTime	gTimeU	= utcTime;
	GTime	gTimeE	= gEpoch;
	GTime	gTimeY	= uYds;

	std::cout << "\n";
	std::cout << "anything to GTime:"   << "\n";
	std::cout << "PTime   to GTime:   " << gTimeP.bigTime << " " << gTimeP.to_string(1) << "\n";
	std::cout << "UtcTime to GTime:   " << gTimeU.bigTime << " " << gTimeU.to_string(1) << "\n";
	std::cout << "GEpoch  to GTime:   " << gTimeE.bigTime << " " << gTimeE.to_string(1) << "\n";
	std::cout << "UYds    to GTime:   " << gTimeY.bigTime << " " << gTimeY.to_string(1) << "\n";

	GTime	gTimeG	= GTime(gWeek, gTow);
	GTime	gTimeB	= GTime(bWeek, bTow);

	std::cout << "GTime(GWeek, GTow): " << gTimeG.bigTime << " " << gTimeG.to_string(1) << "\n";
	std::cout << "GTime(BWeek, BTow): " << gTimeB.bigTime << " " << gTimeB.to_string(1) << "\n";

	GTime	nearTime= timeGet();
			gTimeG	= GTime(gTow, nearTime);
			gTimeB	= GTime(bTow, nearTime);
	GTime	gTimeR	= GTime(rTod, nearTime);

	std::cout << "GTime(GTow, GTime): " << gTimeG.bigTime << " " << gTimeG.to_string(1) << "\n";
	std::cout << "GTime(BTow, GTime): " << gTimeB.bigTime << " " << gTimeB.to_string(1) << "\n";
	std::cout << "GTime(RTod, GTime): " << gTimeR.bigTime << " " << gTimeR.to_string(1) << "\n";


	double ep[6];

	std::cout << "\n";
	std::cout << "GTime to epoch to GTime:"   << "\n";
	time2epoch(gTime, ep, E_TimeSys::GPST);						std::cout << "GTime to GPST     epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochGPS	= epoch2time(ep, E_TimeSys::GPST);		std::cout << "GPST     epoch to GTime: " << gTimeEpochGPS.to_string(1) << "\n";
	time2epoch(gTime, ep, E_TimeSys::GLONASST);					std::cout << "GTime to GLONASST epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochGLO	= epoch2time(ep, E_TimeSys::GLONASST);	std::cout << "GLONASST epoch to GTime: " << gTimeEpochGLO.to_string(1) << "\n";
	time2epoch(gTime, ep, E_TimeSys::GST);						std::cout << "GTime to GST      epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochGST	= epoch2time(ep, E_TimeSys::GST);		std::cout << "GST      epoch to GTime: " << gTimeEpochGST.to_string(1) << "\n";
	time2epoch(gTime, ep, E_TimeSys::BDT);						std::cout << "GTime to BDT      epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochBDT	= epoch2time(ep, E_TimeSys::BDT);		std::cout << "BDT      epoch to GTime: " << gTimeEpochBDT.to_string(1) << "\n";
	time2epoch(gTime, ep, E_TimeSys::QZSST);					std::cout << "GTime to QZSST    epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochQZS	= epoch2time(ep, E_TimeSys::QZSST);		std::cout << "QZSST    epoch to GTime: " << gTimeEpochQZS.to_string(1) << "\n";
	time2epoch(gTime, ep, E_TimeSys::TAI);						std::cout << "GTime to TAI      epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochTAI	= epoch2time(ep, E_TimeSys::TAI);		std::cout << "TAI      epoch to GTime: " << gTimeEpochTAI.to_string(1) << "\n";
	time2epoch(gTime, ep, E_TimeSys::UTC);						std::cout << "GTime to UTC      epoch: " << (int)ep[0] << "-" << std::setw(2) << (int)ep[1] << "-" << std::setw(2) << (int)ep[2] << " " << std::setw(2) << (int)ep[3] << ":" << std::setw(2) << (int)ep[4] << ":" << std::setw(4) << ep[5] << "\n";
	GTime gTimeEpochUTC	= epoch2time(ep, E_TimeSys::UTC);		std::cout << "UTC      epoch to GTime: " << gTimeEpochUTC.to_string(1) << "\n";


	double yds[6];

	std::cout << "\n";
	std::cout << "GTime to yds to GTime:"   << "\n";
	time2yds(gTime, yds, E_TimeSys::GPST);						std::cout << "GTime to GPST     yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsGPS	= yds2time(yds, E_TimeSys::GPST);		std::cout << "GPST     yds to GTime: " << gTimeYdsGPS.to_string(1) << "\n";
	time2yds(gTime, yds, E_TimeSys::GLONASST);					std::cout << "GTime to GLONASST yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsGLO	= yds2time(yds, E_TimeSys::GLONASST);	std::cout << "GLONASST yds to GTime: " << gTimeYdsGLO.to_string(1) << "\n";
	time2yds(gTime, yds, E_TimeSys::GST);						std::cout << "GTime to GST      yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsGST	= yds2time(yds, E_TimeSys::GST);		std::cout << "GST      yds to GTime: " << gTimeYdsGST.to_string(1) << "\n";
	time2yds(gTime, yds, E_TimeSys::BDT);						std::cout << "GTime to BDT      yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsBDT	= yds2time(yds, E_TimeSys::BDT);		std::cout << "BDT      yds to GTime: " << gTimeYdsBDT.to_string(1) << "\n";
	time2yds(gTime, yds, E_TimeSys::QZSST);						std::cout << "GTime to QZSST    yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsQZS	= yds2time(yds, E_TimeSys::QZSST);		std::cout << "QZSST    yds to GTime: " << gTimeYdsQZS.to_string(1) << "\n";
	time2yds(gTime, yds, E_TimeSys::TAI);						std::cout << "GTime to TAI      yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsTAI	= yds2time(yds, E_TimeSys::TAI);		std::cout << "TAI      yds to GTime: " << gTimeYdsTAI.to_string(1) << "\n";
	time2yds(gTime, yds, E_TimeSys::UTC);						std::cout << "GTime to UTC      yds: " << (int)yds[0] << " " << (int)yds[1] << " " << yds[2] << "\n";
	GTime gTimeYdsUTC	= yds2time(yds, E_TimeSys::UTC);		std::cout << "UTC      yds to GTime: " << gTimeYdsUTC.to_string(1) << "\n";


	GTow	nearGTow= 1;
			nearTime= GTime(gWeek, nearGTow);
			gTow	= 604800 - 1.1;
			gTimeG	= GTime(gTow, nearTime);

	std::cout << std::setfill(' ') << "\n";
	std::cout << "Week/Day roll-over:" << "\n";
	std::cout << "nearGTow: " << std::setw(8) << nearGTow.val << "\tnearTime:              " << nearTime.to_string(1) << "\n";
	std::cout << "gTow:     " << std::setw(8) << gTow.    val << "\tGTime(gTow, nearTime): " << gTimeG.  to_string(1) << "\n";

			nearGTow= 604800 - 1;
			nearTime= GTime(gWeek, nearGTow);
			gTow	= 1.1;
			gTimeG	= GTime(gTow, nearTime);

	std::cout << "nearGTow: " << std::setw(8) << nearGTow.val << "\tnearTime:              " << nearTime.to_string(1) << "\n";
	std::cout << "gTow:     " << std::setw(8) << gTow.    val << "\tGTime(gTow, nearTime): " << gTimeG.  to_string(1) << "\n";

	BTow	nearBTow= 1;
			nearTime= GTime(bWeek, nearBTow);
			bTow	= 604800 - 1.1;
			gTimeB	= GTime(bTow, nearTime);

	std::cout << "nearBTow: " << std::setw(8) << nearBTow.val << "\tnearTime:              " << nearTime.to_string(1) << "\n";
	std::cout << "bTow:     " << std::setw(8) << bTow.    val << "\tGTime(bTow, nearTime): " << gTimeB.  to_string(1) << "\n";

			nearBTow= 604800 - 1;
			nearTime= GTime(bWeek, nearBTow);
			bTow	= 1.1;
			gTimeB	= GTime(bTow, nearTime);

	std::cout << "nearBTow: " << std::setw(8) << nearBTow.val << "\tnearTime:              " << nearTime.to_string(1) << "\n";
	std::cout << "bTow:     " << std::setw(8) << bTow.    val << "\tGTime(bTow, nearTime): " << gTimeB.  to_string(1) << "\n";

	UYds	nearYds	= uYds;
			nearYds.sod	= 86400 - 10800 + 1;
			nearTime= nearYds;
	RTod	nearTod	= nearTime;
			rTod	= 86400 - 1.1;
			gTimeR	= GTime(rTod,  nearTime);

	std::cout << "nearSod:  " << std::setw(8) << nearYds. sod << "\tnearTime:              " << nearTime.to_string(1) << "\tnearTod:  " << nearTod. val << "\n";
	std::cout << "rTod:     " << std::setw(8) << rTod.    val << "\tGTime(rTod, nearTime): " << gTimeR.  to_string(1) << "\n";

			nearYds.sod	= 86400 - 10800 - 1;
			nearTime= nearYds;
			nearTod	= nearTime;
			rTod	= 1.1;
			gTimeR	= GTime(rTod,  nearTime);

	std::cout << "nearSod:  " << std::setw(8) << nearYds. sod << "\tnearTime:              " << nearTime.to_string(1) << "\tnearTod:  " << nearTod. val << "\n";
	std::cout << "rTod:     " << std::setw(8) << rTod.    val << "\tGTime(rTod, nearTime): " << gTimeR.  to_string(1) << "\n";

			gEpoch	= {2017, 1, 1, 0, 0, 17.9};
			gTimeE	= gEpoch;
			utcTime	= gTimeE;
			gTimeU	= utcTime;

	std::cout << "\n";
	std::cout << "Leap second roll-over:" << "\n";
	std::cout << "GTime:              " << gTimeE. bigTime << " " << gTimeE .to_string(1) << "\n";
	std::cout << "GTime to UtcTime:   " << utcTime.bigTime << " " << utcTime.to_string(1) << "\n";
	std::cout << "UtcTime to GTime:   " << gTimeU. bigTime << " " << gTimeU .to_string(1) << "\n";
}

#include "coordinates.hpp"
#include "iers2010.hpp"

const GTime j2000TT		= GEpoch{2000, E_Month::JAN, 1,		11,	58,	55.816	+ GPS_SUB_UTC_2000};	// defined in utc 11:58:55.816
void rotationTest()
{
	GTime time = GEpoch{2019, E_Month::FEB, 6,		0,	0,	0};

	Vector3d nowPosI = Vector3d::Zero();
	nowPosI(0) = 20000000;
	Vector3d lastPosE = nowPosI;

	MjDateUt1	lastmjDate;

// 	change to inherit long double, then require to_double() for conversions, deny otherwise.

	//do a day's worth of 20 second increment tests
	for (; time < GEpoch{2019, E_Month::FEB, 9,		0,	0,	0}; time += 10)
	{
		Matrix3d i2tMatrix	= Matrix3d::Identity();

		//convert to terrestrial
		ERPValues erpv = getErp(nav.erp, time);
		eci2ecef(time, erpv, i2tMatrix);

		MjDateUt1	mjDate	(time, erpv.ut1Utc);

		Vector3d rSatEcef	= i2tMatrix * nowPosI;
		Vector3d deltaP		= rSatEcef - lastPosE;

		long double deltamjdtt = mjDate.val - lastmjDate.val;

		printf("\n%s - {%15.6f} [%20.32e %30.23e %30.23e] %30.23e",
			   time.to_string().c_str(),
			   rSatEcef.dot(deltaP),
			   (double)mjDate.val,
			   (double)deltamjdtt,
			   (double)(mjDate.val),
			   rSatEcef.dot(lastPosE)
  			);

		lastPosE		= rSatEcef;

// 		std::cout << rSatEcef.dot(lastPosE);
		lastmjDate	= mjDate;
	}
}

void longDoubleTest()
{
	long double a;
	std::cout << "\n" << "This system uses " << sizeof(a) * 8 << " bits for long doubles. (Hopefully that number is 128...)" << "\n";
}

#include "rtcmEncoder.hpp"
#include "ssr.hpp"
/*
void debugSSR(GTime t0, GTime targetTime, E_Sys sys, SsrOutMap& ssrOutMap)
{
	int			iodPos;
	int			iodEph;
	int			iodClk;
	GTime		ephValidStart;
	GTime		ephValidStop;
	GTime		clkValidStart;
	GTime		clkValidStop;
	Vector3d	dPos[2];
	double		dClk[3] = {};
	double		refClk = 0;
	bool		refClkFound = false;
	bool		posDeltaPass[2];
	bool		clkDeltaPass[2];
	Vector3d	dPosDiff;
	double		dClkDiff[2];
	GTime		ephTime = t0;

	for (auto& Sat : getSysSats(sys))
	{
		GObs obs;
		obs.Sat = Sat;
		obs.satNav_ptr = &nav.satNavMap[Sat];

		dPos[0] = Vector3d::Zero();
		dPos[1] = Vector3d::Zero();

		posDeltaPass[1] = ssrPosDelta(t0, ephTime, obs, obs.satNav_ptr->receivedSSR,	dPos[1], iodPos, iodEph,	ephValidStart, ephValidStop);
		clkDeltaPass[1] = ssrClkDelta(t0, ephTime, obs, obs.satNav_ptr->receivedSSR,	dClk[1], iodClk,			clkValidStart, clkValidStop);

		if	( posDeltaPass[0] && clkDeltaPass[0]
			&&posDeltaPass[1] && clkDeltaPass[1])
		{
			if	( refClkFound == false
				&&abs(dClk[0]) < 1E-6)
			{
				refClk = dClk[1];
				refClkFound = true;
			}

			dClk[2]		= dClk[1] - refClk;

			dPosDiff	= dPos[0] - dPos[1];
			dClkDiff[0]	= dClk[0] - dClk[1];
			dClkDiff[1]	= dClk[0] - dClk[2];

			std::cout << std::setprecision(4) << std::fixed;
			std::cout	<< "Debugging ssr: "
						<< "\tnow time (GPST): "					<< timeGet().to_string(1)
						<< "\ttarget time (GPST): "					<< targetTime.to_string(1)
						<< "\tt0 (GPST): "							<< t0.to_string(1)
						<< "\tsat: "								<< Sat.id()
						<< "\tdecoded: "			<< std::setw(7)	<< dPos[1] .transpose()	<< "\t" << std::setw(8) << dClk[1]		<< "\t" << std::setw(8) << dClk[2]
						<< "\tencoded: "			<< std::setw(7)	<< dPos[0] .transpose()	<< "\t" << std::setw(8) << dClk[0]
						<< "\tencoded-decoded: "	<< std::setw(7)	<< dPosDiff.transpose()	<< "\t" << std::setw(8) << dClkDiff[1]
													<< "\n";
		}
	}

	std::cout << std::setprecision(4) << std::fixed;
	for (auto& [sat, ssrOut] : ssrOutMap)
	{
		std::cout	<< "Straddle clks: "
					<< "\tnow time (GPST): "				<< timeGet().to_string(1)
					<< "\ttarget time (GPST): "				<< targetTime.to_string(1)
					<< "\tt0 (GPST): " 						<< t0.to_string(1)
					<< "\tsat: "							<< sat.id()
					<< "\ttime[0]: "						<< ssrOut.clkInput.vals[0].time.to_string(1)
					<< "\tiode[0]: "	<< std::setw( 3)	<< ssrOut.clkInput.vals[0].iode
					<< "\tbrdc[0]: "	<< std::setw(12)	<< ssrOut.clkInput.vals[0].brdcClk
					<< "\tprec[0]: "	<< std::setw(12)	<< ssrOut.clkInput.vals[0].precClk
					<< "\ttime[1]: "						<< ssrOut.clkInput.vals[1].time.to_string(1)
					<< "\tiode[1]: "	<< std::setw( 3)	<< ssrOut.clkInput.vals[1].iode
					<< "\tbrdc[1]: "	<< std::setw(12)	<< ssrOut.clkInput.vals[1].brdcClk
					<< "\tprec[1]: "	<< std::setw(12)	<< ssrOut.clkInput.vals[1].precClk
										<< "\n";
	}
}*/

void reflector()
{
	Vector3d face[3];
	for (int i = 0; i < 3; i++)
	{
		face[i] = Vector3d::Zero();
		face[i](i) = 1;
	}

	double absorbtion	[3] = {};
	double specularity	[3] = {};

	absorbtion	[0] = 1;
	specularity	[1] = 1;
// 	specularity	[2] = 0;

	for (int x : {0, 1})
	for (int y : {0, 1})
	for (int z : {0, 1})
	{
		Vector3d source;
		source(0) = x;
		source(1) = y;
		source(2) = z;

		source.normalize();

		Vector3d totalMomentum = Vector3d::Zero();

		double totalFrontalarea = 0;

		for (int i = 0; i < 3; i++)
		{
			Vector3d correctFace = face[i];

			if (correctFace.dot(source) < 0)
			{
				correctFace *= -1;
			}

			double frontalArea = 1 * source.dot(face[i]);
			totalFrontalarea += frontalArea;

			Vector3d incoming	= frontalArea * source;
			Vector3d reflected	= -frontalArea * (source - 2 * (source.dot(correctFace)) * correctFace) * specularity[i];
			Vector3d emissive	= frontalArea * correctFace * (1-specularity[i]) * 0.7;

			Vector3d outgoing	= (1 - absorbtion[i]) * (reflected + emissive);

			Vector3d momentum	= (incoming + outgoing);


// 			std::cout << incoming.transpose() << "\n";
// 			std::cout << reflected.transpose() << "\n";
// 			std::cout << emissive.transpose() << "\n";
			totalMomentum += momentum;
		}
		totalMomentum /= totalFrontalarea;
		printf("%10.4f %10.4f %10.4f -> %10.4f %10.4f %10.4f \n", source(0), source(1), source(2), totalMomentum(0), totalMomentum(1), totalMomentum(2));
	}

}

// void Spawn()
// {
// 	int pid = fork();
//
// 	std::cout << pid << "\n";
// 	if (pid)
// 	{
// 		return;
// 		std::cout << "returning\n";
// 	}
// 	while (pid < 10000)
// 		std::cout << pid++ << "\n";
//
// }

#include "streamParser.hpp"
#include "streamFile.hpp"
#include "rinex.hpp"
#include "coordinates.hpp"
#include "erp.hpp"

#include "geomagField.hpp"

void debugIGRF()
{
	std::cout << "\nDebugging IGRF:" << "\n";

	// // test 1 - file reading
	// std::cout << "  n  m         g         h" << "\n";
	// for (auto& [year, igrfMF] : igrfMFMap)
	// {
	// 	std::cout << igrfMF.year << ":" << "\n";
	// 	for (int i = 0; i <= igrfMF.maxDegree; i++)
	// 	for (int j = 0; j <= i; j++)
	// 	{
	// 		std::cout	<< std::setprecision(2) << std::fixed;
	// 		std::cout	<< " " << std::setw(2) << i
	// 					<< " " << std::setw(2) << j
	// 					<< " " << std::setw(9) << igrfMF.gnm(i, j)
	// 					<< " " << std::setw(9) << igrfMF.hnm(i, j)
	// 					<< "\n";
	// 	}
	// }

	// {
	// 	std::cout << igrfSV.year << "-" << igrfSV.yearEnd << ":" << "\n";
	// 	for (int i = 0; i <= igrfSV.maxDegree; i++)
	// 	for (int j = 0; j <= i; j++)
	// 	{
	// 		std::cout	<< std::setprecision(2) << std::fixed;
	// 		std::cout	<< " " << std::setw(2) << i
	// 					<< " " << std::setw(2) << j
	// 					<< " " << std::setw(9) << igrfSV.gnm(i, j)
	// 					<< " " << std::setw(9) << igrfSV.hnm(i, j)
	// 					<< "\n";
	// 	}
	// }

	// // test 2 - get coefficients
	// {
	// 	std::cout << "                    ";
	// 	for (int i = 0; i <= 13; i++)
	// 	for (int j = 0; j <= i;  j++)
	// 	{
	// 		std::cout	<< "   g " << std::setw(2) << i << "," << std::setw(2) << j
	// 					<< "   h " << std::setw(2) << i << "," << std::setw(2) << j;
	// 	}
	// 	std::cout << "\n";
	// }

	// for (int year = 1981; year <= 2026; year++)
	// {
	// 	GEpoch ep	= {year, 1, 1, 0, 0, 0.0};
	// 	GTime time	= ep;

	// 	GeomagMainField igrfMF;
	// 	bool pass = getSHCoef(time, igrfMF);

	// 	if (!pass)
	// 		return;

	// 	std::cout << time.to_string() << ":";
	// 	for (int i = 0; i <= igrfMF.maxDegree; i++)
	// 	for (int j = 0; j <= i; j++)
	// 	{
	// 		std::cout	<< std::setprecision(2) << std::fixed;
	// 		std::cout	<< " " << std::setw(9) << igrfMF.gnm(i, j)
	// 					<< " " << std::setw(9) << igrfMF.hnm(i, j);
	// 	}
	// 	std::cout << "\n";
	// }

	// test 3.1 - time series
	{
		Vector3d r = {-4.05205271694605e+06, 4.21283598092691e+06, -2.54510460797403e+06};	// Cartesian - ALIC
		VectorPos pos = ecef2pos(r);
		pos[0] = asin(r.z()/r.norm());
		pos[1] = atan2(r.y(), r.x());
		pos[2] = r.norm();

		std::cout	<< std::setprecision(5) << std::fixed;
		std::cout	<< "\n\tGeocentric pos: " << pos[0]*R2D << " " << pos[1]*R2D << " " << pos[2]/1000 << "\n";

		for (int year = 1981; year <= 2023; year++)
		{
			GEpoch ep = {year, 1, 1, 0, 0, 0.0};
			GTime time = ep;

			Vector3d intensity = getGeomagIntensity(time, pos);

			std::cout	<< std::setprecision(5) << std::fixed;
			std::cout	<< "\tyear: " << ep.year;
			std::cout	<< std::setprecision(1) << std::fixed;
			std::cout	<< "\tX: " << std::setw(8) << intensity.x()
						<< "\tY: " << std::setw(8) << intensity.y()
						<< "\tZ: " << std::setw(8) << intensity.z()
						<< "\n";
		}
	}

	// test 3.2 - grid (including singularity)
	{
		GEpoch ep = {2019, 7, 18, 0, 0, 0.0};
		GTime time = ep;
		double year = decimalYear(time);

		std::cout	<< std::setprecision(5) << std::fixed;
		std::cout	<< "\n\tyear: " << year << "\n";

		for (int lat =  -90; lat <=  90; lat += 10)
		for (int lon = -180; lon <= 180; lon += 20)
		{
			VectorPos	pos = Vector3d(lat*D2R, lon*D2R, 6371000);

			Vector3d intensity = getGeomagIntensity(time, pos);

			std::cout	<< std::setprecision(1) << std::fixed;
			std::cout	<< "\tGeocentric pos: " << std::setw(5) << pos[0]*R2D << " " << std::setw(6) << pos[1]*R2D << " " << std::setw(4) << pos[2]/1000;
			std::cout	<< std::setprecision(1) << std::fixed;
			std::cout	<< "\tX: " << std::setw(8) << intensity.x()
						<< "\tY: " << std::setw(8) << intensity.y()
						<< "\tZ: " << std::setw(8) << intensity.z()
						<< "\n";
		}
	}
}

#include "attitude.hpp"
#include "planets.hpp"

void debugAttitude()
{
	// GPS
	SatSys Sat(E_Sys::GPS, 1);
	GEpoch ep = {2023, 8, 28, 0, 0, 0};
// 	GEpoch ep = {2019, 07, 18, 0, 0, 0};
	int nEpoch = 288;
	double interval = 300;

	// // GRACE C
	// SatSys Sat(E_Sys::LEO, 65);
	// // GEpoch ep = {2019, 02, 13, 0, 0, 0};
	// GEpoch ep = {2022, 01, 01, 0, 0, 0};
	// int nEpoch = 8640;
	// double interval = 10;

	// // GRACE D
	// SatSys Sat(E_Sys::LEO, 65);
	// GEpoch ep = {2022, 01, 01, 0, 0, 0};
	// int nEpoch = 8640;
	// double interval = 10;

	// // COSMIC2 - 1
	// SatSys Sat(E_Sys::LEO, 80);
	// GEpoch ep = {2022, 12, 31, 23, 39, 43};
	// int nEpoch = 2261;
	// double interval = 1;

	// SPIRE
// 	SatSys Sat(E_Sys::LEO, 99);
// 	GEpoch ep = {2023, 01, 01, 9, 59, 46};
// 	int nEpoch = 5853;
// 	double interval = 1;

// 	GObs obs;
// 	obs.Sat = Sat;
// 	obs.time = ep;
// 	obs.satNav_ptr = &nav.satNavMap[Sat];
	// // SPIRE
	// SatSys Sat(E_Sys::LEO, 99);
	// GEpoch ep = {2023, 01, 01, 9, 59, 46};
	// int nEpoch = 5853;
	// double interval = 1;

	GTime time = ep;

	SatPos satPos;
	satPos.Sat = Sat;

	Receiver rec;
	rec.id = Sat.id();

	auto& satOpts = acsConfig.getSatOpts(Sat);
	auto& recOpts = acsConfig.getRecOpts(rec.id);

	auto& satNav = nav.satNavMap[Sat];
	satNav.antBoresight	= satOpts.antenna_boresight;
	satNav.antAzimuth	= satOpts.antenna_azimuth;
	satPos.satNav_ptr	= &satNav;

	AttStatus attStatus = {};
	VectorEcef rSat;
	VectorEcef rSun;
	VectorEcef eSun;

	printf("\n");
	printf("Debugging satellite attitude:\n");
	for (int i=0; i<nEpoch; i++)
	{
		int	week = GWeek(time);
		double tow = GTow(time);

		// for GNSS satellites
		satPos.posTime = time;
		satpos(nullStream, time, time, satPos, satOpts.posModel.sources, E_OffsetType::COM, nav);
		updateSatAtts(satPos);
		rSat = satPos.rSatCom;
		attStatus = satPos.satNav_ptr->attStatus;

		// // for LEO satellites
		// recAtt(rec, time, recOpts.rec_attitude.sources);
		// attStatus = rec.attStatus;

		planetPosEcef(time, E_ThirdBody::SUN, rSun);
		eSun = rSun.normalized();

		printf("%d %8.1f\t%13.3f %13.3f %13.3f\t%9.6f %9.6f %9.6f\t%9.6f %9.6f %9.6f\t%9.6f %9.6f %9.6f\t%9.6f %9.6f %9.6f\t%9.6f %9.6f %9.6f\t%9.6f %9.6f %9.6f\t%9.6f %9.6f %9.6f\n",
				week, tow,
				rSat			.x(),	rSat			.y(),	rSat			.z(), 	// for GNSS satellites
				// rec.pos			.x(),	rec.pos			.y(),	rec.pos			.z(), 	// (not ready) for LEO satellites
				attStatus.eXBody.x(),	attStatus.eXBody.y(),	attStatus.eXBody.z(),
				attStatus.eYBody.x(),	attStatus.eYBody.y(),	attStatus.eYBody.z(),
				attStatus.eZBody.x(),	attStatus.eZBody.y(),	attStatus.eZBody.z(),
				attStatus.eXAnt	.x(),	attStatus.eXAnt	.y(),	attStatus.eXAnt	.z(),
				attStatus.eYAnt	.x(),	attStatus.eYAnt	.y(),	attStatus.eYAnt	.z(),
				attStatus.eZAnt	.x(),	attStatus.eZAnt	.y(),	attStatus.eZAnt	.z(),
				eSun			.x(),	eSun			.y(),	eSun			.z());

		time += interval;
	}
}


struct Thing
{

};

/** This function calls nothing
 */
void debugErp()
{
	Thing thing;
}

#include <fstream>
#include "tides.hpp"

using iers2010::hisp::ntin;

void debugBlq()
{
	string id = "ALIC";

	Receiver rec;
	rec.id = id;

	for (auto& blqfile : acsConfig.ocean_tide_loading_blq_files)
	{
		bool found = readBlq(blqfile, rec, E_LoadingType::OCEAN);
	}

	for (auto& blqfile : acsConfig.atmos_tide_loading_blq_files)
	{
		bool found = readBlq(blqfile, rec, E_LoadingType::ATMOSPHERIC);
	}

	std::cout << std::fixed;

	std::cout << "\nDebugging OTL BLQ: " << rec.id << std::fixed << "\n";
	for (auto& [wave, disp] : rec.otlDisplacement)
	{
		std::cout << wave._to_string() << ":";
		for (int i = 0; i < 3; i++)		std::cout << "\t" << std::setprecision(5) << std::setw(9) << disp.amplitude[i];
		for (int i = 0; i < 3; i++)		std::cout << "\t" << std::setprecision(1) << std::setw(9) << disp.phase[i];
		std::cout << "\n";
	}

	std::cout << "\nDebugging ATL BLQ: " << rec.id << std::fixed << "\n";
	for (auto& [wave, disp] : rec.atlDisplacement)
	{
		std::cout << wave._to_string() << ":";
		for (int i = 0; i < 3; i++)		std::cout << "\t" << std::setprecision(5) << std::setw(9) << disp.amplitude[i];
		for (int i = 0; i < 3; i++)		std::cout << "\t" << std::setprecision(1) << std::setw(9) << disp.phase[i];
		std::cout << "\n";
	}
}

map<string, map<double, VectorEnu>> readRefOtlDisp(string file)
{
	map<string, map<double, VectorEnu>> dispMap;

	std::ifstream fileStream(file);
	if (!fileStream)
	{
		return dispMap;
	}

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		if (line[0] == '*')
			continue;

		char* buff = &line[0];

		string		id = line.substr(0, 4);
		char		dummy[5];
		double		mjd;
		double		v[3];
		int found = sscanf(buff, "%4s %lf %lf %lf %lf", dummy, &mjd, &v[0], &v[1], &v[2]);

		if (found != 5)
			continue;

		VectorEnu denu;
		denu[0] = -v[2];
		denu[1] = -v[1];
		denu[2] =  v[0];

		dispMap[id][mjd] = denu;
	}

	return dispMap;
}

void debugTideOcean()
{
	std::cout << "\nDebugging OTL:" << "\n";

	string filename = "testData/oload.test";
	auto dispRefMap = readRefOtlDisp(filename);

	for (auto& [id, dispTimeMap] : dispRefMap)
	{
		Receiver rec;
		rec.id = id;

		for (auto& blqfile : acsConfig.ocean_tide_loading_blq_files)
		{
			bool found = readBlq(blqfile, rec, E_LoadingType::OCEAN);
		}

		if (rec.otlDisplacement.empty())
			return;

		for (auto& [mjdval, denuRef] : dispTimeMap)
		{
			MjDateUtc mjdUtc;
			mjdUtc.val = mjdval;

			GTime time = GTime(mjdUtc);
			ERPValues erpv = getErp(nav.erp, time);
			MjDateUt1 mjdUt1(time, erpv.ut1Utc);

			// VectorEnu denu = tideOceanLoad (std::cout, mjdUt1, rec.otlDisplacement);
			VectorEnu denu = tideOceanLoadAdjusted(std::cout, time, mjdUt1, rec.otlDisplacement);
			VectorEnu diff = denu - denuRef;

			std::cout	<< std::setprecision( 7)	<< std::fixed
						<< "\t"						<< id
						<< "\t"						<< mjdUtc.to_double()
						<< "\t"	<< std::setw(10)	<< denuRef.e()
						<< "\t"	<< std::setw(10)	<< denuRef.n()
						<< "\t"	<< std::setw(10)	<< denuRef.u()
						<< "\t"	<< std::setw(10)	<< denu.e()
						<< "\t"	<< std::setw(10)	<< denu.n()
						<< "\t"	<< std::setw(10)	<< denu.u()
						<< "\t"	<< std::setw(10)	<< diff.e()
						<< "\t"	<< std::setw(10)	<< diff.n()
						<< "\t"	<< std::setw(10)	<< diff.u()
						<< "\n";
		}
		std::cout << "\n";
	}
}

void debugHardisp()
{
	/// Read in ocean loading coefficients from stdin
	std::cout << "\nDebugging OTL Hardisp:" << "\n";

	string filename = "testData/oload.test";
	auto dispRefMap = readRefOtlDisp(filename);

	for (auto& [id, dispTimeMap] : dispRefMap)
	{
		Receiver rec;
		rec.id = id;

		for (auto& blqfile : acsConfig.ocean_tide_loading_blq_files)
		{
			bool found = readBlq(blqfile, rec, E_LoadingType::OCEAN);
		}

		if (rec.otlDisplacement.empty())
			return;

		for (auto& [mjdval, denuRef] : dispTimeMap)
		{
			MjDateUtc mjdUtc;
			mjdUtc.val = mjdval;

			GTime time = GTime(mjdUtc);

			VectorEnu denu = tideOceanLoadHardisp(std::cout, time, rec.otlDisplacement);
			VectorEnu diff = denu - denuRef;

			std::cout	<< std::setprecision( 7)	<< std::fixed
						<< "\t"						<< id
						<< "\t"						<< mjdUtc.to_double()
						<< "\t"	<< std::setw(10)	<< denuRef.e()
						<< "\t"	<< std::setw(10)	<< denuRef.n()
						<< "\t"	<< std::setw(10)	<< denuRef.u()
						<< "\t"	<< std::setw(10)	<< denu.e()
						<< "\t"	<< std::setw(10)	<< denu.n()
						<< "\t"	<< std::setw(10)	<< denu.u()
						<< "\t"	<< std::setw(10)	<< diff.e()
						<< "\t"	<< std::setw(10)	<< diff.n()
						<< "\t"	<< std::setw(10)	<< diff.u()
						<< "\n";
		}
		std::cout << "\n";
	}
}

map<double, VectorEnu> readRefAtlDisp(string file)
{
	map<double, VectorEnu> dispMap;

	std::ifstream fileStream(file);
	if (!fileStream)
	{
		return dispMap;
	}

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		if (line.substr(1, 2) == "$$")
			continue;

		char* buff = &line[0];

		char		dummy[5];
		double		mjd;
		double		v[3];
		int found = sscanf(buff, "%lf %lf %lf %lf", &mjd, &v[0], &v[1], &v[2]);

		if (found != 4)
			continue;

		VectorEnu denu;
		denu[0] = v[2] * 1E-3;
		denu[1] = v[1] * 1E-3;
		denu[2] = v[0] * 1E-3;

		dispMap[mjd] = denu;
	}

	return dispMap;
}

map<string, map<double, VectorEnu>> readRefAplDisp(string file)
{
	map<string, map<double, VectorEnu>> dispMap;

	std::ifstream fileStream(file);
	if (!fileStream)
	{
		return dispMap;
	}

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		if (line[0] == '!')
			continue;

		char* buff = &line[0];

		string		id = line.substr(0, 4);
		char		dummy[5];
		double		mjd;
		double		v[3];
		int found = sscanf(buff, "%4s %lf %lf %lf %lf", dummy, &mjd, &v[0], &v[1], &v[2]);

		if (found != 5)
			continue;

		VectorEnu denu;
		denu[0] = v[1];
		denu[1] = v[2];
		denu[2] = v[0];

		dispMap[id][mjd] = denu;
	}

	return dispMap;
}

void debugTideAtmos()
{
	std::cout << "\nDebugging ATL:" << "\n";

	string id = "ALIC";
	Receiver rec;
	rec.id = id;

	for (auto& blqfile : acsConfig.atmos_tide_loading_blq_files)
	{
		bool found = readBlq(blqfile, rec, E_LoadingType::ATMOSPHERIC);
	}

	if (rec.atlDisplacement.empty())
		return;

	// Test 1 - single station, multiple days
	string filename = "testData/grdintrp.dat";
	auto dispRefMap = readRefAtlDisp(filename);

	double mjdval = 58682;
	for (auto& [mjdval, denuRef] : dispRefMap)
	{
		MjDateUt1 mjdUt1;
		mjdUt1.val = mjdval;


		VectorEnu denu = tideAtmosLoad(std::cout, mjdUt1, rec.atlDisplacement);
		VectorEnu diff = denu - denuRef;

		std::cout	<< std::setprecision( 7)	<< std::fixed
					<< "\t"						<< id
					<< "\t"						<< mjdUt1.to_double()
					<< "\t"	<< std::setw(10)	<< denuRef.e()
					<< "\t"	<< std::setw(10)	<< denuRef.n()
					<< "\t"	<< std::setw(10)	<< denuRef.u()
					<< "\t"	<< std::setw(10)	<< denu.e()
					<< "\t"	<< std::setw(10)	<< denu.n()
					<< "\t"	<< std::setw(10)	<< denu.u()
					<< "\t"	<< std::setw(10)	<< diff.e()
					<< "\t"	<< std::setw(10)	<< diff.n()
					<< "\t"	<< std::setw(10)	<< diff.u()
					<< "\n";
	}

	// // Test 2 - single station, multiple days (one year)
	// string filename = "testData/y2019.apl_g.txt";
	// auto dispRefMap = readRefAplDisp(filename);

	// auto dispTimeMap = dispRefMap[id];
	// for (auto& [mjdval, denuRef] : dispTimeMap)
	// {
	// 	MjDateUt1 mjdUt1;
	// 	mjdUt1.val = mjdval;

	// 	VectorEnu denu = tideAtmosLoad(std::cout, mjdUt1, rec.atlDisplacement);
	// 	VectorEnu diff = denu - denuRef;

	// 	std::cout	<< std::setprecision( 7)	<< std::fixed
	// 				<< "\t"						<< id
	// 				<< "\t"						<< mjdUt1.to_double()
	// 				<< "\t"	<< std::setw(10)	<< denuRef.e()
	// 				<< "\t"	<< std::setw(10)	<< denuRef.n()
	// 				<< "\t"	<< std::setw(10)	<< denuRef.u()
	// 				<< "\t"	<< std::setw(10)	<< denu.e()
	// 				<< "\t"	<< std::setw(10)	<< denu.n()
	// 				<< "\t"	<< std::setw(10)	<< denu.u()
	// 				<< "\t"	<< std::setw(10)	<< diff.e()
	// 				<< "\t"	<< std::setw(10)	<< diff.n()
	// 				<< "\t"	<< std::setw(10)	<< diff.u()
	// 				<< "\n";
	// }

	// // Test 3 - multiple stations, single day
	// string filename = "testData/2019199.apl_g.txt";
	// auto dispRefMap = readRefAplDisp(filename);

	// for (auto& [id, dispTimeMap]	: dispRefMap)
	// for (auto& [mjdval, denuRef]	: dispTimeMap)
	// {
	// 	Receiver rec;
	// 	rec.id = id;

	// 	for (auto& blqfile : acsConfig.atmos_tide_loading_blq_files)
	// 	{
	// 		bool found = readBlq(blqfile, rec, E_LoadingType::ATMOSPHERIC);
	// 	}

	// 	if (rec.atlDisplacement.empty())
	// 		continue;

	// 	MjDateUt1 mjdUt1;
	// 	mjdUt1.val = mjdval;

	// 	VectorEnu denu = tideAtmosLoad(std::cout, mjdUt1, rec.atlDisplacement);
	// 	VectorEnu diff = denu - denuRef;

	// 	std::cout	<< std::setprecision( 7)	<< std::fixed
	// 				<< "\t"						<< id
	// 				<< "\t"						<< mjdUt1.to_double()
	// 				<< "\t"	<< std::setw(10)	<< denuRef.e()
	// 				<< "\t"	<< std::setw(10)	<< denuRef.n()
	// 				<< "\t"	<< std::setw(10)	<< denuRef.u()
	// 				<< "\t"	<< std::setw(10)	<< denu.e()
	// 				<< "\t"	<< std::setw(10)	<< denu.n()
	// 				<< "\t"	<< std::setw(10)	<< denu.u()
	// 				<< "\t"	<< std::setw(10)	<< diff.e()
	// 				<< "\t"	<< std::setw(10)	<< diff.n()
	// 				<< "\t"	<< std::setw(10)	<< diff.u()
	// 				<< "\n";
	// }
}

void debugTideSolid()
{
	std::cout << "\nDebugging solid Earth tide:" << "\n";

	// Test cases from DEHANTTIDEINEL.F
	// Note that the last test case should be incorrect
	std::vector<GEpoch> ep =
	{
		{2009,  4, 13,  0,  0,  0},
		{2012,  7, 13,  0,  0,  0},
		{2015,  7, 15,  0,  0,  0},
		{2017,  1, 15,  0,  0,  0},
		{2019,  7, 18,  4, 59, 12}
	};

	std::vector<Vector3d> recPos =
	{
		{ 4075578.385,    931852.890,   4801570.154},
		{ 1112189.660,  -4842955.026,   3985352.284},
		{ 1112200.5696, -4842957.8511,  3985345.9122},
		{ 1112152.8166, -4842857.5435,  3985496.1783},
		{ 2587384.1007872052, -1043033.5652423096,  5716564.3449383173}
	};

	std::vector<Vector3d> rSun =
	{
		{ 137859926952.0150,      54228127881.4350,      23509422341.6960},
		{ -54537460436.2357,     130244288385.2790,      56463429031.5996},
		{ 100210282451.6279,     103055630398.3160,      56855096480.4475},
		{   8382471154.1312895,   10512408445.356153,    -5360583240.3763866},
		{ -40911673203.204002,   135847503359.17343,     54660205331.259735}
	};

	std::vector<Vector3d> rMoon =
	{
		{-179996231.920342,     -312468450.131567,     -169288918.592160},
		{ 300396716.912,         243238281.451,         120548075.939},
		{ 369817604.4348,          1897917.5258,        120804980.8284},
		{ 380934092.93550891,      2871428.1904491195,   79015680.553570181},
		{ 202952994.54523405,   -319652979.70541549,   -135514223.45872557}
	};

	std::vector<Vector3d> dxyzRef =
	{
		{ 0.07700420357108125891,  0.06304056321824967613,  0.05516568152597246810},
		{-0.02036831479592075833,  0.05658254776225972449, -0.07597679676871742227},
		{ 0.00509570869172363845,  0.08286630259835287000, -0.06366349254041896170},
		{ 0.00509570869172363840,  0.08286630259835287000, -0.06366349254041896200},
		{-0.05560417990980600500,  0.02320056584074919900, -0.12297592439382479000}
	};

	for (int i = 0; i < 5; i++)
	{
		GTime time = epoch2time(ep[i].data(), E_TimeSys::UTC);

		ERPValues erpv = getErp(nav.erp, time);
		MjDateUt1 mjdUt1(time, erpv.ut1Utc);

		VectorPos pos;
		pos.lat() = asin(recPos[i].z() / recPos[i].norm());
		pos.lon() = atan2(recPos[i].y(), recPos[i].x());

		// Vector3d dxyz = tideSolidEarth(std::cout, time, mjdUt1, rSun[i], rMoon[i], pos);
		Vector3d dxyz = tideSolidEarthDehant(std::cout, time, rSun[i], rMoon[i], recPos[i]);
		Vector3d diff = dxyz - dxyzRef[i];

		std::cout	<< std::setprecision( 7)	<< std::fixed
					<< "\t"						<< mjdUt1.to_double()
					<< "\t"	<< std::setw(10)	<< dxyzRef[i].x()
					<< "\t"	<< std::setw(10)	<< dxyzRef[i].y()
					<< "\t"	<< std::setw(10)	<< dxyzRef[i].z()
					<< "\t"	<< std::setw(10)	<< dxyz.x()
					<< "\t"	<< std::setw(10)	<< dxyz.y()
					<< "\t"	<< std::setw(10)	<< dxyz.z()
					<< "\t"	<< std::setw(10)	<< diff.x()
					<< "\t"	<< std::setw(10)	<< diff.y()
					<< "\t"	<< std::setw(10)	<< diff.z()
					<< "\n";
	}
}

map<double, Vector3d> readRefSPoleDisp(string file)
{
	map<double, Vector3d> dispMap;

	std::ifstream fileStream(file);
	if (!fileStream)
	{
		return dispMap;
	}

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		char* buff = &line[0];

		double		mjd;
		double		lat;
		double		lon;
		double		dr;
		int found = sscanf(buff, "%lf,%lf,%lf,%lf", &mjd, &lat, &lon, &dr);

		if (found != 4)
			continue;

		Vector3d disp;
		disp[0] = lat;
		disp[1] = lon;
		disp[2] = dr;

		dispMap[mjd] = disp;
	}

	return dispMap;
}

void debugTideSolidPole()
{
	std::cout << "\nDebugging solid Earth pole tide:" << "\n";

	string filename = "testData/test_pole_tide.csv";
	auto dispRefMap = readRefSPoleDisp(filename);

	for (auto& [mjdval, dispRef] : dispRefMap)
	{
		VectorPos pos;
		pos.lat() = dispRef(0) * D2R;
		pos.lon() = dispRef(1) * D2R;

		MjDateUtc mjdUtc;
		mjdUtc.val = mjdval;
		GTime time = GTime(mjdUtc);

		ERPValues erpv = getErp(nav.erp, time);
		MjDateUt1 mjdUt1(time, erpv.ut1Utc);

		VectorEnu denu = tideSolidPole(std::cout, mjdUt1, pos, erpv);
		double diff = denu.u() - dispRef(2);

		std::cout	<< std::setprecision( 7)	<< std::fixed
					<< "\t"						<< mjdUtc.to_double();
		std::cout	<< std::setprecision( 7)	<< std::scientific
					<< "\t"	<< std::setw(10)	<< dispRef(2)
					<< "\t"	<< std::setw(10)	<< denu.u()
					<< "\t"	<< std::setw(10)	<< diff
					<< "\n";
	}
}

map<double, VectorEnu> readRefOPoleDisp(string file)
{
	map<double, VectorEnu> dispMap;

	std::ifstream fileStream(file);
	if (!fileStream)
	{
		return dispMap;
	}

	while (fileStream)
	{
		string line;
		getline(fileStream, line);

		char* buff = &line[0];

		double mjd;
		double v[9];
		int found = sscanf(buff, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &mjd, &v[0], &v[1], &v[2], &v[3], &v[4], &v[5], &v[6], &v[7], &v[8]);

		if (found != 10)
			continue;

		VectorEnu denu;
		denu[0] = v[8];
		denu[1] = v[7];
		denu[2] = v[6];

		dispMap[mjd] = denu;
	}

	return dispMap;
}

void debugTideOceanPole()
{
	std::cout << "\nDebugging ocean pole tide:" << "\n";

	string filename = acsConfig.inputs_root + "tables/opoleloadcoefcmcor.txt";
	readOceanPoleCoeff(filename);

	// // Grid retrieval test
	// {
	// 	MjDateUtc mjdUtc;
	// 	mjdUtc.val = 52640;
	// 	GTime time = GTime(mjdUtc);

	// 	ERPValues erpv = getErp(nav.erp, time);
	// 	MjDateUt1 mjdUt1(time, erpv.ut1Utc);

	// 	vector<VectorPos> vecPos;
	// 	VectorPos pos;
	// 	pos.lat() = -90    * D2R;	pos.lon() =   0    * D2R;	vecPos.push_back(pos);
	// 	pos.lat() = -89.85 * D2R;	pos.lon() = 180    * D2R;	vecPos.push_back(pos);
	// 	pos.lat() = -89.85 * D2R;	pos.lon() = 359.85 * D2R;	vecPos.push_back(pos);
	// 	pos.lat() =   0    * D2R;	pos.lon() =   0    * D2R;	vecPos.push_back(pos);
	// 	pos.lat() =   0    * D2R;	pos.lon() = 180    * D2R;	vecPos.push_back(pos);
	// 	pos.lat() =   0    * D2R;	pos.lon() = 359.85 * D2R;	vecPos.push_back(pos);
	// 	pos.lat() = +89.85 * D2R;	pos.lon() =   0    * D2R;	vecPos.push_back(pos);
	// 	pos.lat() = +89.75 * D2R;	pos.lon() = 180    * D2R;	vecPos.push_back(pos);
	// 	pos.lat() = +90    * D2R;	pos.lon() = 359.85 * D2R;	vecPos.push_back(pos);

	// 	for (auto& pos : vecPos)	VectorEnu denu = tideOceanPole(std::cout, mjdUt1, pos, erpv);
	// }

	// Test case in opoleloadcmcor.test, note that the mean pole model used is different from IERS 2010 Conventions
	filename = "testData/opoleloadcmcor.test";
	auto dispRefMap = readRefOPoleDisp(filename);

	VectorPos pos;
	pos.lat() = -43.75 * D2R;
	pos.lon() = 232.25 * D2R;
	for (auto& [mjdval, denuRef] : dispRefMap)
	{
		MjDateUtc mjdUtc;
		mjdUtc.val = mjdval;
		GTime time = GTime(mjdUtc);

		ERPValues erpv = getErp(nav.erp, time);
		// MjDateUt1 mjdUt1(time, erpv.ut1Utc);
		MjDateUt1 mjdUt1(time, 0);

		VectorEnu denu = tideOceanPole(std::cout, mjdUt1, pos, erpv);
		VectorEnu diff = denu - denuRef;

		std::cout	<< std::setprecision( 7)	<< std::fixed
					<< "\t"						<< mjdUtc.to_double()
					<< std::setprecision( 7)	<< std::scientific
					<< "\t"	<< std::setw(10)	<< denuRef.e()
					<< "\t"	<< std::setw(10)	<< denuRef.n()
					<< "\t"	<< std::setw(10)	<< denuRef.u()
					<< "\t"	<< std::setw(10)	<< denu.e()
					<< "\t"	<< std::setw(10)	<< denu.n()
					<< "\t"	<< std::setw(10)	<< denu.u()
					<< "\t"	<< std::setw(10)	<< diff.e()
					<< "\t"	<< std::setw(10)	<< diff.n()
					<< "\t"	<< std::setw(10)	<< diff.u()
					<< "\n";
	}
}

void alternatePostfits(
	Trace&		trace,
	KFMeas&		kfMeas,
	KFState&	kfState);

void infiniteTest()
{
	KFState kfState;

	GTime time;
	time += 60;

// 	for (int i = 0; i < 1000; i++)
	{
		KFMeasEntryList kfMeasEntryList;

		KFKey ionoKey;
		KFKey ambKey;

		ionoKey.type = KF::IONO_STEC;
		ambKey.type = KF::AMBIGUITY;

		kfState.advanced_postfits = true;

		InitialState ionoInit;
		ionoInit.P = 100;
		ionoInit.Q = -1;

		InitialState ambInit;
		ambInit.P = 100;

		{
			KFKey obsKey;
			obsKey.num	= 1;
			obsKey.type	= KF::PHAS_MEAS;

			KFMeasEntry measEntry(&kfState);

			measEntry.addDsgnEntry(ionoKey,	1, ionoInit);

			measEntry.setInnov(5);
// 			measEntry.setNoise(1);

			measEntry.addNoiseEntry(obsKey, 1, 1);

			kfMeasEntryList.push_back(measEntry);
		}

		{
			KFKey obsKey;
			obsKey.num = 2;
			obsKey.type = KF::PHAS_MEAS;

			KFMeasEntry measEntry(&kfState);

			measEntry.addDsgnEntry(ionoKey,	1, ionoInit);
			measEntry.addDsgnEntry(ambKey,	1, ambInit);

			measEntry.setInnov(8);
// 			measEntry.setNoise(1);

			measEntry.addNoiseEntry(obsKey, 1, 1);

			kfMeasEntryList.push_back(measEntry);
		}

		kfState.output_residuals = true;

		kfState.stateTransition(std::cout, time);

		kfState.outputStates(std::cout);

		KFMeas kfMeas(kfState, kfMeasEntryList, time);


		alternatePostfits(std::cout, kfMeas, kfState);

		kfState.filterKalman(std::cout, kfMeas, "", true);

		kfState.outputStates(std::cout);


		time++;
	}
}

#include <iostream>
#include "ubxDecoder.hpp"

void getAccData()
{
	std::ifstream inputStream("../inputData/otherProducts/Grace/ACT1B_2019-02-14_C_04.txt");

	string line;
	while (std::getline(inputStream, line))
	{
		if (line[0] == '#')
			break;
	}

	while (std::getline(inputStream, line))
	{
		if (line.empty())
		{
			break;
		}

		long int intTime;
		char dummy;
		Vector3d accl;

		sscanf(line.c_str(), "%ld %c %lf %lf %lf", &intTime, &dummy, &accl[1], &accl[2], &accl[0]);

// 		accl[2] = 0;
// 		accl[1] = 0;

		GTime time;
		time.bigTime = 630763200;
		time += intTime;

// 		std::cout << "\n" << time << " " << accl.transpose();

		UbxDecoder::acclDataMaps["L64"][time] = accl;
	}
}

#include "orbitProp.hpp"

void perEpochPropTest(
	GTime time)
{
	SatSys Sat = SatSys("L51");

	SatPos satPos0;
	satPos0.Sat = Sat;

	bool pass = satPosPrecise(std::cout, time, satPos0, nav);

	if (!pass)
		return;

	ERPValues erpv0 = getErp(nav.erp, time);
	FrameSwapper frameSwapper0(time, erpv0);
	satPos0.rSatEci0 = frameSwapper0(satPos0.rSatCom, &satPos0.satVel, &satPos0.vSatEci0);

	KFState kfState;
	kfState.time = time;

	for (int i = 0; i < 3; i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::ORBIT;
		kfKey.Sat	= Sat;

		kfKey.num	= i;
		kfState.addKFState(kfKey, {.x = satPos0.rSatEci0[i]});

		kfKey.num	= i + 3;
		kfState.addKFState(kfKey, {.x = satPos0.vSatEci0[i]});
	}
	kfState.stateTransition(std::cout, kfState.time);

	KFState copy = kfState;

	double dt = 60;

	static double propSumSqr	= 0;
	static double ellipseSumSqr	= 0;
	static double j2SumSqr		= 0;

	static double num			= 0;

	GTime newTime = kfState.time + dt;

	predictOrbits(std::cout, copy, newTime);
	copy.stateTransition(std::cout, newTime);

	std::cout << "\n" << "\n" << kfState.time;

	SatPos satPosPrec;
	SatPos satPosProp;
	SatPos satPosEllipse;
	SatPos satPosJ2;

	satPosPrec		.Sat = SatSys("L51");
	satPosProp		.Sat = SatSys("L51");
	satPosEllipse	.Sat = SatSys("L51");
	satPosJ2		.Sat = SatSys("L51");

	pass = satPosPrecise(std::cout, newTime,	satPosPrec,		nav);						std::cout << " precise passed: "	<< pass;
	pass = satPosKalman	(std::cout, newTime,	satPosProp,		&copy);						std::cout << " prop Passed: "		<< pass;
	// pass = satPosKalman	(std::cout, newTime,	satPosEllipse,	&kfState);					std::cout << " ellp Passed: "		<< pass;
	pass = satPosKalman	(std::cout, newTime,	satPosJ2,		&kfState);			std::cout << " j2 Passed: "			<< pass;

	ERPValues erpv = getErp(nav.erp, newTime);
	FrameSwapper frameSwapper(newTime, erpv);
	satPosPrec.rSatEci0 = frameSwapper(satPosPrec.rSatCom, &satPosPrec.satVel, &satPosPrec.vSatEci0);

	Matrix3d E = ecef2rac(satPosPrec.rSatEci0, satPosPrec.vSatEci0);

	VectorEci differenceProp	= satPosProp	.rSatEciDt - satPosPrec.rSatEci0;		Vector3d rtnProp	= E * differenceProp;
	VectorEci differenceEllipse	= satPosEllipse	.rSatEciDt - satPosPrec.rSatEci0;		Vector3d rtnEllipse	= E * differenceEllipse;
	VectorEci differenceJ2		= satPosJ2		.rSatEciDt - satPosPrec.rSatEci0;		Vector3d rtnJ2		= E * differenceJ2;

	std::cout << "\r\nPrecise:\t"	<< satPosPrec		.rSatEci0	.transpose().format(heavyFmt);
	std::cout << "\r\nPropFull:\t"	<< satPosProp		.rSatEciDt	.transpose().format(heavyFmt) << "\t" << rtnProp	.transpose().format(heavyFmt) << " \t" << rtnProp	.norm();
	std::cout << "\r\nEllipse:\t"	<< satPosEllipse	.rSatEciDt	.transpose().format(heavyFmt) << "\t" << rtnEllipse	.transpose().format(heavyFmt) << " \t" << rtnEllipse.norm();
	std::cout << "\r\nEllipseJ2:\t"	<< satPosJ2			.rSatEciDt	.transpose().format(heavyFmt) << "\t" << rtnJ2		.transpose().format(heavyFmt) << " \t" << rtnJ2		.norm();

	propSumSqr		+= rtnProp		.squaredNorm();
	ellipseSumSqr	+= rtnEllipse	.squaredNorm();
	j2SumSqr		+= rtnJ2		.squaredNorm();

	num++;

	double propRms		= sqrt(propSumSqr		/ num);
	double ellipseRms	= sqrt(ellipseSumSqr	/ num);
	double j2Rms		= sqrt(j2SumSqr			/ num);

	std::cout << "\n";
	std::cout << "\n" << "PropFull  rms from prec with t=" << dt << " : " << propRms;
	std::cout << "\n" << "Ellipse   rms from prec with t=" << dt << " : " << ellipseRms;
	std::cout << "\n" << "EllipseJ2 rms from prec with t=" << dt << " : " << j2Rms;
}

void accel()
{
	KFState kfState;

	GTime time;
	time += 60;

	double actualX = 0;
	double actualV = 0;
	double actualA = 0;

	double actualScale	[2] = {1,1};//{1.2, 0.95};
	double actualBias	[2] = {0.02, -0.1};

	InitialState init;
	init.P = 100;

	InitialState sInit;
	sInit.P = 100;
	sInit.x = 1;

	InitialState vInit;
	vInit.P = 100;
	// vInit.Q = 100;

	InitialState aInit;
	aInit.P = 100;


	KFKey posKey	= {.type = KF::REC_POS};
	KFKey velKey	= {.type = KF::REC_VEL};
	KFKey accKey	= {.type = KF::REC_ACC};
	KFKey scaleKey	= {.type = KF::ACCL_SCALE};
	KFKey biasKey	= {.type = KF::ACCL_BIAS};


	//not really about indirectly estimating acceleration,
	//here, i indirectly estimated velocity instead as a first pass

	kfState.output_residuals = true;

	kfState.addKFState(posKey, init);

	for (int i = 0; i < 400; i++)
	{
		if (i > 100)
		{
			actualA = 1;
		}
		if (i > 200)
		{
			actualA = -1;
		}

		actualV += actualA;
		actualX += actualV;

		kfState.removeState(accKey);

		kfState.stateTransition(std::cout, time);

		kfState.outputStates(std::cout, "/Deleted");
		{
			KFMeasEntryList kfMeasEntryList;

			if (1)
			for (int i = 0; i < 2; i++)
			{
				scaleKey.num	= i;
				biasKey.num		= i;

				KFMeasEntry measEntry(&kfState);

				double stateBias			= 0;
				double stateScale			= 1;
				double stateAcceleration	= 0;

				// kfState.addKFState(scaleKey,	sInit);
				kfState.addKFState(biasKey,		init);

				// kfState.getKFValue(scaleKey,	stateScale);
				kfState.getKFValue(biasKey,		stateBias);
				// kfState.getKFValue(velKey,		stateVelocity);

				double measuredAcceleration		= actualA * actualScale[i] + actualBias[i];

				double estimatedAcceleration	= (measuredAcceleration - stateBias) / stateScale;

				measEntry.addDsgnEntry(accKey,	stateScale,		vInit);
				measEntry.addDsgnEntry(biasKey,	-stateScale,	init);
				// measEntry.addDsgnEntry(scaleKey,(measuredVelocity - stateBias),	sInit);

				double omc	= measuredAcceleration
							- stateAcceleration
							+ stateBias;

				std::cout << "\n" << "Acceleration  : " << actualA;
				std::cout << "\n" << "Measured      : " << measuredAcceleration;
				std::cout << "\n" << "Estimated     : " << estimatedAcceleration;
				std::cout << "\n" << "OMC           : " << omc;
				std::cout << "\n";


				measEntry.setInnov(omc);
				measEntry.setNoise(0.01);
				measEntry.obsKey.comment = "Acceleration";

				kfMeasEntryList.push_back(measEntry);
			}

			kfState.setKFTransRate(posKey, velKey,	+1,	vInit);
			kfState.setKFTransRate(velKey, accKey,	+1,	aInit);

			kfState.stateTransition(std::cout, time);

			KFMeas combinedMeas(kfState, kfMeasEntryList, time);

			kfState.filterKalman(std::cout, combinedMeas, "", true);

			kfState.outputStates(std::cout, "/Accelerations");
		}

		time++;


		kfState.stateTransition(std::cout, time);

		kfState.outputStates(std::cout, "/PREDICTED");

		//add measurement for position,
		{
			KFMeasEntryList kfMeasEntryList;

			KFMeasEntry measEntry(&kfState);

			double stateX = 0;

			kfState.getKFValue(posKey,	stateX);

			measEntry.addDsgnEntry(posKey,	1, init);

			double omc	= actualX
						- stateX;

			std::cout << "\n" << "actualX  : " << actualX;
			std::cout << "\n" << "stateX   : " << stateX;

			measEntry.setInnov(omc);
			measEntry.setNoise(1);
			measEntry.obsKey.comment = "Position";

			kfMeasEntryList.push_back(measEntry);

			KFMeas combinedMeas(kfState, kfMeasEntryList, time);

			kfState.filterKalman(std::cout, combinedMeas, "", true);
		}
	}
}


void doDebugs()
{

}
