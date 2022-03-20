
// #pragma GCC optimize ("O0")

#include <iostream>
#include <random>

#include "eigenIncluder.hpp"

#if 0
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
#include "enums.h"

void artificialSlip(
		Trace&	trace,		///<
		Obs&	obs,		///< lc_t&	lcBase,		///<
		char*	strprefix)	///<
{
	int lv = 3;

	int amb[3] = {0};
	switch (acsConfig.cscase)
	{
		/* artificial cycle slips */
		case 1:	amb[0] = 1;		amb[1] = 1;		amb[2] = 1;		break;
		case 2:	amb[0] = 0;		amb[1] = 1;		amb[2] = 0;		break;
		case 3:	amb[0] = 9;		amb[1] = 7;		amb[2] = 0;		break;
	}

	for (auto& [ft, sig] : obs.Sigs)
	{
		/* artifical cycle slip */
		if  ( acsConfig.cscase
			&&  lcBase.L_m[ft] != 0
			&&  lcBase.time.time % 300 == 0
			&&( lcBase.Sat == SatSys(E_Sys::GPS, 10)
			||lcBase.Sat == SatSys(E_Sys::GPS, 14)))
		{
			tracepde(lv, trace, "arti %s\n", strprefix);

			//artificially adding 'amb' cycles into the signal's measurements.
			double lambda = obs.satNav_ptr->lamMap[ft];
			lcBase.L_m[ft] += amb[ft] * lambda;
		}
	}
}

std::random_device				randoDev;
std::mt19937						randoGen(randoDev());
std::normal_distribution<double>	rando(0, 15);

void testClockParams()
{
	GTime gtime;
	gtime++;
	KFState kfState;

	for (int i = 0; i < 100; i++)
	{
		KFMeasEntryList kfMeasEntryList0;


		KFMeasEntry	codeMeas(&kfState);
		codeMeas.setValue(i + rando(randoDev));
		codeMeas.setNoise(10);

		KFKey recClockKey		= {KF::REC_SYS_BIAS};
		KFKey recClockRateKey	= {KF::REC_SYS_BIAS_RATE};

		InitialState recClkInit		= {0,	SQR(10),	SQR(1)};
		InitialState recClkRateInit	= {0,	SQR(20),	SQR(0.01)};

		codeMeas.addDsgnEntry(recClockKey,						+1,	recClkInit);
		kfState.setKFTransRate(recClockKey, recClockRateKey,	+1,	recClkRateInit);

		kfMeasEntryList0.push_back(codeMeas);

		kfState.stateTransition(std::cout, gtime++);

// 		kfState.consolidateKFState();

		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList0);
		kfState.filterKalman(std::cout, combinedMeas, false);

		kfState.outputStates(std::cout);
	}
	exit(0);
}



void minimumTest(
	Trace&					trace)
{
	GTime gtime;
	gtime++;
	trace << std::endl;
	map<string, Vector3d> pointMap;
	Vector3d p;

	pointMap["0"] = {+10,	+10,	0};
	pointMap["1"] = {-10, 	+10,	0};
	pointMap["2"] = {+10, 	-10,	0};
	pointMap["3"] = {-10, 	-10,	0};
	pointMap["4"] = {0,		0,		10};
	pointMap["5"] = {0,		10,		0};
	pointMap["6"] = {5,		5,		0};
	pointMap["7"] = {5,		5,		0};

	KFState kfStateStations;

	kfStateStations.initFilterEpoch();

	//generate fake station data
	{
		KFMeasEntryList stationEntries;
		for (auto& [id, a] : pointMap)
		{
			Station* station_ptr = new Station();


			a += Vector3d{0,1000,0};

			for (int i = 0; i < 3; i++)
			{
				station_ptr->snx.pos[i] = a[i];
			}

			double ang = 0.000015;
			Matrix3d rot;
			rot <<
			+cos(ang),	+sin(ang),	0,
			-sin(ang),	+cos(ang),	0,
			0,			0,			1;

			Vector3d p = a;
			p *= 1.123;
			p += Vector3d{1,2.6,3};
	// 		p += Vector3d{0,0,1};
			p = rot * p;
			trace << "\t" << p(0) << "\t" << p(1) << std::endl;

			KFMeasEntry measX(&kfStateStations);
			KFMeasEntry measY(&kfStateStations);
			KFMeasEntry measZ(&kfStateStations);

			measX.addDsgnEntry({KF::REC_POS, {}, id, 0, station_ptr},	1);
			measY.addDsgnEntry({KF::REC_POS, {}, id, 1, station_ptr},	1);
			measZ.addDsgnEntry({KF::REC_POS, {}, id, 2, station_ptr},	1);

			measX.setValue(p(0) - a(0));
			measY.setValue(p(1) - a(1));
			measZ.setValue(p(2) - a(2));

			measX.setNoise(100);
			measY.setNoise(100);
			measZ.setNoise(100);

			stationEntries.push_back(measX);
			stationEntries.push_back(measY);
			stationEntries.push_back(measZ);
		}

		KFMeasEntry dummyMeas(&kfStateStations);
		dummyMeas.setValue(1);
		dummyMeas.setNoise(100);
		dummyMeas.addDsgnEntry({KF::IONOSPHERIC, 	{}, "1", 3},	1);
		dummyMeas.addDsgnEntry({KF::REC_POS,		{}, "2", 2},	1);
		stationEntries.push_back(dummyMeas);


		//add process noise to existing states as per their initialisations.
		kfStateStations.stateTransition(trace, gtime);

		KFMeas combinedMeas = kfStateStations.combineKFMeasList(stationEntries);

		/* network parameter estimation */
		if (kfStateStations.lsqRequired)
		{
			kfStateStations.lsqRequired = false;
			trace << std::endl << " -------DOING LEAST SQUARES--------";
			kfStateStations.leastSquareInitStates(trace, combinedMeas);
		}
	}
	kfStateStations.outputStates(trace);

	minimum(trace, kfStateStations);
	kfStateStations.outputStates(trace);
}





#include <cmath>

MatrixXd recurse(int rows, int total, double* data)
{
	MatrixXd out(rows,rows);

// 	std::cout << "+++++++++++++++++++++++++" << std::endl;
	if (rows > 1)
	{
		int top = rows / 2;
		int bot = rows - top;

		auto tl = Eigen::Map<MatrixXd, 0, Eigen::OuterStride<Eigen::Dynamic>>(data,						top, top, Eigen::OuterStride<Eigen::Dynamic>(total));

		auto tr = Eigen::Map<MatrixXd, 0, Eigen::OuterStride<Eigen::Dynamic>>(data + top * total,		top, bot, Eigen::OuterStride<Eigen::Dynamic>(total));
		auto bl = Eigen::Map<MatrixXd, 0, Eigen::OuterStride<Eigen::Dynamic>>(data + top,				bot, top, Eigen::OuterStride<Eigen::Dynamic>(total));

		auto br = Eigen::Map<MatrixXd, 0, Eigen::OuterStride<Eigen::Dynamic>>(data + top * total + top,	bot, bot, Eigen::OuterStride<Eigen::Dynamic>(total));


// 		std::cout << std::endl << "BL " << std::endl << bl << std::endl;
// 		std::cout << std::endl << "TR " << std::endl << tr << std::endl;

// 		std::cout << std::endl << "inverting tl " << std::endl << tl << std::endl;
		MatrixXd tlinv = recurse(top, total, data);
// 		std::cout << std::endl << "inverted tl " << std::endl << tlinv << std::endl;

// 		std::cout << std::endl << "inverting br " << std::endl << br << std::endl;
		MatrixXd brinv = recurse(bot, total, data + top * total + top);
// 		std::cout << std::endl << "inverted br " << std::endl << brinv << std::endl;

// 		std::cout << std::endl << "tr " << std::endl << tr << std::endl;
// 		std::cout << std::endl << "bl " << std::endl << bl << std::endl;


		MatrixXd tlInter = tl - tr	* brinv * bl;
		MatrixXd brInter = br - bl	* tlinv * tr;

// 		std::cout << std::endl << "inverting TL " << std::endl << tlInter << std::endl;
		MatrixXd TL = recurse(top, tlInter.rows(), tlInter.data());
// 		std::cout << std::endl << "inverted TL " << std::endl << TL << std::endl;

// 		std::cout << std::endl << "inverting BR " << std::endl << brInter << std::endl;
		MatrixXd BR = recurse(bot, brInter.rows(), brInter.data());
// 		std::cout << std::endl << "inverted BR " << std::endl << BR << std::endl;

		out.topLeftCorner		(top, top) = TL;
		out.bottomLeftCorner	(bot, top) = - brinv * bl * TL;
		out.topRightCorner		(top, bot) = out.bottomLeftCorner(bot,top).transpose();
		out.bottomRightCorner	(bot, bot) = BR;
	}
	if (rows == 1)
	{
		out(0,0) = 1/(*data);
	}

// 	std::cout << "+++++++++++++++++++++++++" << std::endl;
	return out;
}

MatrixXd multiGauss(MatrixXd& Mat)
{
	int n = Mat.rows();

	MatrixXd aug = MatrixXd::Identity(n, n);

	std::cout << Mat << std::endl << std::endl;
	std::cout << std::endl << std::endl;

	for (int i = 0; i < n; i++)
	for (int j = 0; j < n; j++)
	if (i != j)
	{
		double r = Mat(j,i) / Mat(i,i);
		Mat.row(j) -= r * Mat.row(i);
		aug.row(j) -= r * aug.row(i);
	}

	std::cout << Mat << std::endl << std::endl;
	std::cout << aug << std::endl << std::endl;
	std::cout << std::endl << std::endl;

	for (int i = 0; i < n; i++)
	{
		double r = Mat(i,i);
		Mat.row(i) /= r;
		aug.row(i) /= r;
	}

	std::cout << Mat << std::endl << std::endl;
	std::cout << aug << std::endl << std::endl;
	std::cout << std::endl << std::endl;

	return aug;
}

// void inverter()
// {
// #define n 4
// 	MatrixXd Mat = MatrixXd::Random(n,n);
//
// 	Mat = (Mat * Mat.transpose()).eval();
// 	std::cout << Mat.inverse() << std::endl;
//
// 	multiGauss(Mat);
//
// 	std::cout << "begin" << std::endl;
// 	MatrixXd inv = recurse(Mat.rows(), Mat.rows(), Mat.data());
// 	std::cout << "middle" << std::endl;
// 	MatrixXd Inv = Mat.inverse();
// 	std::cout << "end" << std::endl;
// }


void ambiguities()
{
	GTime gtime;
	gtime++;
	KFState kfState;
	initFilterTrace(kfState, "rts.1");

	map<int, double> receiverBias;
	map<int, double> satelliteBias;
	map<int, map<int, int>> recSatAmb;

	int n = 5;

	for (int i = 0; i < n; i++)
	{
		receiverBias[i] = rando(randoDev);
	}
	for (int i = 0; i < n; i++)
	{
		satelliteBias[i] = rando(randoDev);
	}
	for (int i = 0; i < n; i++)
	for (int j = 0; j < n; j++)
	{
		recSatAmb[i][j] = rando(randoDev);
	}
	recSatAmb[0][5] = 0;
	receiverBias[0] = 0;

	std::cout << "*" << " : " << "RecBias" << "\t" << "SatBias" << std::endl;
	for (int i = 0; i < n; i++)
	{
		std::cout << i << " : " << receiverBias[i] << "\t" << satelliteBias[i] << std::endl;
	}

	std::cout << "*" << " : " << "Ambiguities" << std::endl;
	for (int i = 0; i < n; i++)
	{
		if (i == 0)
		{
			std::cout << "*" << "\t: ";
			for (int j = 0; j < n; j++)
			{
				std::cout << "\t" << j;
			}
			std::cout << std::endl;
			std::cout << "--------------------------------------------------------------------------------" << std::endl;
		}

		std::cout << i << " \t: ";
		for (int j = 0; j < n; j++)
		{
			std::cout << "\t" << recSatAmb[i][j];
		}
		std::cout << std::endl;
	}

	int N = 600;
	for (int i = 0; i < N; i++)
	{
		kfState.initFilterEpoch();

		KFMeasEntryList kfMeasEntryList;

		for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
		{
// 			if (i < n/2 && j < n/2) continue;
// 			if (i >=n/2 && j >=n/2) continue;

			KFMeasEntry	codeMeas(&kfState);
			codeMeas.setValue(receiverBias[i] + satelliteBias[j] + recSatAmb[i][j] + rando(randoDev)/50);
			codeMeas.setNoise(1/* + rando(randoDev) * i * j*/);

			KFKey recBiasKey		= {KF::REC_SYS_BIAS,	{},						"REC " + std::to_string(i)};
			KFKey satBiasKey		= {KF::SAT_CLOCK,		SatSys(E_Sys::GPS, j)};
			KFKey ambigtyKey		= {KF::AMBIGUITY,		SatSys(E_Sys::GPS, j),	"REC " + std::to_string(i)};

			InitialState boringInit		= {0,	SQR(100),	SQR(0)};

			codeMeas.addDsgnEntry(recBiasKey,	+1,	boringInit);
			codeMeas.addDsgnEntry(ambigtyKey,	+1,	boringInit);

// 			boringInit.P = 1;
			codeMeas.addDsgnEntry(satBiasKey,	+1,	boringInit);

			kfMeasEntryList.push_back(codeMeas);

		}

		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, gtime++);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(nullStream, combinedMeas, false);

		if (i == N-1)
		{
			kfState.outputStates(std::cout);

// 			std::cout << std::endl << kfState.P;
		}
	}

	std::cout << std::endl << "WEAKLY CONSTRAINING BIASES TO ZERO" << std::endl;

	{
		kfState.initFilterEpoch();

		KFMeasEntryList kfMeasEntryList;

		for (auto& [key, index] : kfState.kfIndexMap)
		{
			if (key.type != KF::REC_SYS_BIAS && key.type != KF::SAT_CLOCK)
				continue;
			KFMeasEntry	codeMeas(&kfState);
			codeMeas.setValue(0);
			codeMeas.setNoise(1);

			codeMeas.addDsgnEntry(key,	+1);
			codeMeas.obsKey.str = "hdh";
			kfMeasEntryList.push_back(codeMeas);
		}

		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, gtime++);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(nullStream, combinedMeas, false);

		kfState.outputStates(std::cout);
	}

	std::cout << std::endl << "CONSTRAINING REC 0 BIAS TO ZERO" << std::endl;
if (1)
	{
		kfState.initFilterEpoch();

		KFMeasEntryList kfMeasEntryList;

		{
			KFMeasEntry	codeMeas(&kfState);
			codeMeas.setValue(0);
			codeMeas.setNoise(0.0001);

			KFKey key		= {KF::REC_SYS_BIAS,		{},	"REC " + std::to_string(0)};

			codeMeas.addDsgnEntry(key,	+1);
			codeMeas.obsKey.str = "hdh";
			kfMeasEntryList.push_back(codeMeas);
		}

		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, gtime++);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(nullStream, combinedMeas, false);

		kfState.outputStates(std::cout);
	}

	std::cout << std::endl << "ADDING REDUNDANT FLOAT TERMS" << std::endl;

	{
		kfState.initFilterEpoch();

		KFMeasEntryList kfMeasEntryList;

		for (auto& [key, index] : kfState.kfIndexMap)
		{
			if (key.type != KF::AMBIGUITY)
				continue;
			KFMeasEntry	codeMeas(&kfState);
			codeMeas.setValue(0);
			codeMeas.setNoise(0.0001);

			KFKey key1		= {KF::AMBIGUITY,		key.Sat,	key.str};
			KFKey key2		= {KF::SAT_CLOCK,		key.Sat};
			KFKey key3		= {KF::PHASE_BIAS,		key.Sat,	key.str};

			double sat = 0;
			double amb = 0;
			kfState.getKFValue(key1, amb);
			kfState.getKFValue(key2, sat);

			InitialState boringInit		= {amb+sat,	SQR(100),	SQR(0)};
			codeMeas.addDsgnEntry(key1,	+1);
			codeMeas.addDsgnEntry(key2,	+1);
			codeMeas.addDsgnEntry(key3,	-1, boringInit);
			codeMeas.obsKey.str = "hdh";
			kfMeasEntryList.push_back(codeMeas);
		}

		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, gtime++);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(nullStream, combinedMeas, false);

		kfState.outputStates(std::cout);
	}



	for (int i = 0; i < n*2; i++)
	{

		std::cout << std::endl << "ROUNDING AN AMBIGUITY TO INTEGER: " << i + 1<< std::endl;

		kfState.initFilterEpoch();

		KFMeasEntryList kfMeasEntryList;

		int j = 0;
		for (auto& [key, index] : kfState.kfIndexMap)
		{
			if (key.type != KF::PHASE_BIAS)
				continue;

			if (j == i)
			{
				KFKey newKey = key;
				newKey.type = KF::AMBIGUITY;
				KFMeasEntry	codeMeas(&kfState);
				codeMeas.setValue(std::round(kfState.x(index)));
				codeMeas.setNoise(0.00001);

				codeMeas.addDsgnEntry(newKey,	+1);
				codeMeas.obsKey.str = "hdh";
				kfMeasEntryList.push_back(codeMeas);
			}
			j++;
		}

		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, gtime++);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(nullStream, combinedMeas, false);

		kfState.outputStates(std::cout);
	}

	std::cout << "Program finished!" << endl;
}

void minimumTest(Trace& trace);



void lamnda()
{
	int n = 4;
	MatrixXd q = MatrixXd::Random(n,n);	
	MatrixXd Q = q * q.transpose();
	
	std::cout << "Q"<< std::endl << Q << std::endl;
	
	MatrixXd Z = MatrixXd::Identity(n,n);
	MatrixXd P;
	
	P = MatrixXd::Identity(n,n);
	P(0,1) = 3;

	Z *= P;
	
	P = MatrixXd::Identity(n,n);
	P(1,3) = -2;

	Z *= P;
	
	P = MatrixXd::Identity(n,n);
	P(2,0) = 5;

	Z *= P;
	
	P = MatrixXd::Identity(n,n);
	P(3,1) = 7;

	Z *= P;
	
	std::cout << "Z"<< std::endl << Z << std::endl;
	
	MatrixXd Q1 = Z * Q * Z.transpose();
	
	std::cout << "Q1"<< std::endl << Q1 << std::endl;
	
	MatrixXd J2 = Q1;
	for (int i = 0; i < J2.cols(); i++)
	{
		J2.col(i) *= 1/J2(i,i);
	}
	
	std::cout << "J2"<< std::endl << J2 << std::endl;
	
	
	
	
	MatrixXd R = MatrixXd::Identity(n,n);
	
	R(0,3) = 6;
	R(1,3) = 2;
	R(2,3) = 1;
	
	MatrixXd Q3 = R * Q1 * R.transpose();
	
	std::cout << "Q3"<< std::endl << Q3 << std::endl;
	
	MatrixXd J4 = Q3;
	for (int i = 0; i < J2.cols(); i++)
	{
		J4.col(i) *= 1/J4(i,i);
	}
	
	std::cout << "J4"<< std::endl << J4 << std::endl;
	
	
	
	
	MatrixXd R2 = MatrixXd::Identity(n,n);
	
	R2(3,1) = -6;
	R2(2,1) = 2;
	
	MatrixXd Q5 = R2 * Q3 * R2.transpose();
	
	std::cout << "Q5"<< std::endl << Q5 << std::endl;
	
	MatrixXd J6 = Q5;
	for (int i = 0; i < J6.cols(); i++)
	{
		J6.col(i) *= 1/J6(i,i);
	}
	
	std::cout << "J6"<< std::endl << J6 << std::endl;
	
	
	
	
	MatrixXd R3 = MatrixXd::Identity(n,n);
	
	R3(0,1) = -1;
	R3(2,3) = -1;
	
	MatrixXd Q7 = R3 * Q5 * R3.transpose();
	
	std::cout << "Q7"<< std::endl << Q7 << std::endl;
	
	MatrixXd J8 = Q7;
	for (int i = 0; i < J8.cols(); i++)
	{
		J8.col(i) *= 1/J8(i,i);
	}
	
	std::cout << "J8"<< std::endl << J8 << std::endl;
	
	
	
	
	MatrixXd R4 = MatrixXd::Identity(n,n);
	
	R4(0,1) = -1;
	
	MatrixXd Q9 = R4 * Q7 * R4.transpose();
	
	std::cout << "Q9"<< std::endl << Q9 << std::endl;
	
	MatrixXd J10 = Q9;
	for (int i = 0; i < J10.cols(); i++)
	{
		J10.col(i) *= 1/J10(i,i);
	}
	
	std::cout << "J10"<< std::endl << J10 << std::endl;
	
	
	
	
	MatrixXd R5 = MatrixXd::Identity(n,n);
	
	R5(2,0) = -3;
	R5(0,1) = 1;
	
	MatrixXd Q11 = R5 * Q9 * R5.transpose();
	
	std::cout << "Q11"<< std::endl << Q11 << std::endl;
	
	MatrixXd J12 = Q11;
	for (int i = 0; i < J12.cols(); i++)
	{
		J12.col(i) *= 1/J12(i,i);
	}
	
	std::cout << "J12"<< std::endl << J12 << std::endl;
	

}

#include <fstream>


#include "navigation.hpp"
#include "constants.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "orbits.hpp"
#include "satSys.hpp"
#include "gTime.hpp"

bool peph2pos(
	Trace&		trace,
	GTime		time,
	SatSys&		Sat,
	Vector3d&	rSat,
	Vector3d&	satVel,
	double*		dtSat,
	double&		ephVar,
	E_Svh&		svh,
	nav_t& 		nav,
	PcoMapType*	pcoMap_ptr,
	bool		applyRelativity);


#define Rz(t,X) do {				\
	double sint = sin(t);			\
	(X)[8]=1;						\
	(X)[2]=(X)[5]=(X)[6]=(X)[7]=0;	\
	(X)[0]=(X)[4]=cos(t);			\
	(X)[3]=+sint;					\
	(X)[1]=-sint;					\
} while (0)


void eci2ecefA(
	const GTime		tutc,
	Matrix3d&		U)	
{
	const double ep2000[] = {2000, 1, 1, 12, 0, 0};

//	trace(4,"%s: tutc=%s\n", __FUNCTION__, tutc.to_string(0).c_str());

	/* terrestrial time */
	GTime	tutc_	= tutc;
	GTime	tgps	= utc2gpst(tutc_);
	double	t		= (tgps - epoch2time(ep2000) + 19 + 32.184);	
	
	double angle = 360 * t / 86400 * D2R;
	
	Rz(angle,	U.data());
}



void getPos(
	double		t, 
	Vector3d&	pos)
{
	double R = 25000000;
	
	double M = sqrt(MU_GPS / (R*R*R));
	
	double angle = M * t;
	
// 	std::cout << "\nPOSANGLE:" << angle;
	
	pos(0) = R * cos(angle) * 1;
	pos(1) = R * sin(angle) * cos(PI/8)*1.05;
	pos(2) = R * sin(angle)*sin(PI/8);	//todo aaron remove
}


void integrator2()
{
	GTime time(1563451230, 0);
	
	KFState kfState;
	kfState.output_residuals = true;
	kfState.inverter = E_Inverter::LDLT;

	std::cout << std::fixed;
	std::cout << std::setprecision(5);
	Vector3d pos0 = Vector3d::Zero();
	Vector3d pos1 = Vector3d::Zero();
		
	
	Vector3d vel = pos1 - pos0;
	
	double deltaT = 0.00001;
	int i = 0;
	int A = 1;
	double dM;
	
	int t = A + i;
	getPos(t + i,			pos0);
	getPos(t + i + deltaT,	pos1);
	
	vel = (pos1 - pos0) / deltaT;
	
	double ii = i;
	double tt = 0.001;
	
	Vector3d pos = pos0;
	
	for ( ; i < 100000; i+=100)
	{
		std::cout << std::endl << "======================= " << i << std::endl << std::endl;
		
		t = A + i;
		
		for ( ; ii < i; ii += tt)
		{
			Vector3d acc = Vector3d::Zero();
			
			acc = -pos.normalized() * MU_GPS / pos.squaredNorm();
			pos += vel * tt + acc * tt * tt / 2;
			vel += acc * tt;			
		}
		
		
		VectorXd keplers0;
		inertial2Keplers(std::cout, pos, vel, keplers0);
		
		kfState.stateTransition(std::cout, {t, 0});
		
		MatrixXd partials;
		getKeplerPartials(keplers0, partials);
		
		double noises[6]	= {10000,	10000,	10000,	1,			1,			1};
		double pnoises[6]	= {0.001,	0.001,	0.001,	0.0000001,	0.0000001,	0.00001};
		
		KFMeasEntryList measEntryList;
		if (1)
		{
			VectorXd stateKeplers = keplers0;
			
			for (int k = 0; k < 6; k++)
			{
				KFKey kfKey;
				
				kfKey.type	= KF::KEPLERS;
				kfKey.num	= k;
				
				
				kfState.getKFValue(kfKey, stateKeplers[k]);
			}
			
			Vector3d statePos;
			keplers2inertial(stateKeplers, statePos, dM);
			
			Vector3d deltaPos = pos - statePos;
			
			for (int j = 0; j < 3; j++)
			{
				KFMeasEntry measEntry(&kfState);
				
				for (int k = 0; k < KEPLER::NUM; k++)
				{
					KFKey kfKey;
					kfKey.type	= KF::KEPLERS;
					kfKey.num	= k;
					
					InitialState init;
					init.x = stateKeplers[k];
					init.P = SQR(noises	[k]);
					init.Q = SQR(pnoises[k]);
					
					if	(  k >= KEPLER::LX 
						&& k <= KEPLER::LZ)
					{
						init.P /= SQR(MOMENTUM_SCALE);
						init.Q /= SQR(MOMENTUM_SCALE);
					}
					
					measEntry.addDsgnEntry(kfKey, partials(j,k), init);
				}
				
				measEntry.setInnov(deltaPos[j]);
				measEntry.setNoise(0.1);
				
				measEntryList.push_back(measEntry);
			}
		}
		
		{
			KFKey kfKey;			
			kfKey.type	= KF::KEPLERS;
			kfKey.num	= KEPLER::M;
			kfState.setKFTransRate(kfKey, {.type = KF::ONE}, dM);
		}
			
		//use state transition to initialise
		kfState.stateTransition(std::cout, {t, 0});
		
		KFMeas combinedMeas = kfState.combineKFMeasList(measEntryList, {t, 0});
		
		kfState.filterKalman(std::cout, combinedMeas, true);
		
		kfState.outputStates(std::cout);
// 		kfState.outputCorrelations(std::cout);
	}
	
	std::cout << "DONE" << std::endl;
}



void getBodyPos(
	double t,
	double R,
	double T,
	Vector3d& body1)
{ 
	body1[0] = R * cos(t * 2*PI / T);
	body1[1] = R * sin(t * 2*PI / T);
	body1[2] = 0;
}

void getAcceleration(
	double		frac,
	Vector3d	pos,
	Vector3d	bodyPos,
	Vector3d&	acc)
{
	double mu = frac * MU_GPS;
	
	Vector3d R = bodyPos - pos;
	acc = mu * R.normalized() / R.squaredNorm();
// 	std::cout << std::setprecision(20);
// 	std::cout << "\na: " << acc.transpose();
}

void orbitTests()
{

	Vector3d pos0 = Vector3d::Zero();
	Vector3d vel0 = Vector3d::Zero();

	double t = 0;
	
	double dt	= 1/64.0;
	dt = 100;
	double R	= 0;
	R = 2000;
	bool	useCFM	= tru todo aaron change to just apply a constant force, then see if that works.e;
// 	useCFM = false;
	bool	outputIntegrator = true;
	outputIntegrator = false;
	double deltaT = 0.0001;
	
	//get initial position and velocity
	{
		Vector3d pos1 = Vector3d::Zero();
		
		getPos(t,			pos0);
		getPos(t + deltaT,	pos1);
		
		vel0 = pos1 - pos0;
		vel0 /= deltaT;
	}
	
	Vector3d pos = pos0;
	Vector3d vel = vel0;
	
	VectorXd keplers;
	inertial2Keplers(std::cout, pos, vel, keplers);
	
// 	MatrixXd partialsA;
// 	getKeplerPartials(keplers, partialsA);
	
// 	MatrixXd partialsB;
// 	getKeplerInversePartials(pos, vel, partialsB);
	
	MatrixXd dRVdA = MatrixXd::Zero(6,3);
	
	dRVdA(0,0) = 0.5	* dt * dt;
	dRVdA(1,1) = 0.5	* dt * dt;
	dRVdA(2,2) = 0.5	* dt * dt;
	dRVdA(3,0) = 1		* dt;
	dRVdA(4,1) = 1		* dt;
	dRVdA(5,2) = 1		* dt;
	
// 	MatrixXd dKdA = partialsB * dRVdA;
	
	
	
// 	std::cout << "\nPartialsA:\n" << partialsA;
// 	std::cout << "\nPartialsB:\n" << partialsB;
// 	std::cout << "\nPartialsb:\n" << partialsB.inverse();
	std::cout << "\ndRVdA:\n" << dRVdA;
// 	std::cout << "\ndKdA:\n" << dKdA;
	std::cout << "\n";

	
// 	std::cout << std::fixed;
// 	std::cout << std::setprecision(5);
	std::cout << "\nInitial Conds:\n";
	
	std::cout << "\npos: " << pos0.transpose() 
			  << "\nvel: " << vel0.transpose() << "\n";
	
	std::cout << "\ndt:  "	<< dt;
	std::cout << "\nR:   "	<< R;
	std::cout << "\nCFM: "	<< useCFM;
	std::cout << "\nInt: "	<< outputIntegrator;
	std::cout << "\n";

	std::cout << std::fixed;
	std::cout << std::setprecision(7);
	
	VectorXd previousKeplers = keplers;

	std::cout << "\nIntegration";
	Vector3d acc;
	while (t < 86401)
	{		
		int intT = t;
		
		if	(  intT % 120	== 0
			&& t - intT		== 0)
		{
// 			std::cout << std::setprecision(3);
			
			std::cout << "\n" << t << "\t" << pos.transpose() << "\t" << keplers.transpose();
			std::cout << " " << (keplers - previousKeplers).transpose();
			previousKeplers = keplers;
		}
		
			
		double dM;
		if (outputIntegrator == false)
		{
			keplers2inertial(keplers, pos, dM);
			
			Vector3d pos1;
			VectorXd keplers1 = keplers;
			
			keplers1(KEPLER::M) += dM * deltaT;
			
			keplers2inertial(keplers1, pos1, dM);
			
			vel = (pos1 - pos) / deltaT;
			std::cout << "\nkeplers: " << keplers.transpose() << "\n";
			std::cout << "\nPos: " << pos.transpose() << "\n";
// 			std::cout << "\nVel: " << vel.transpose() << "\n";
			
		}
		else
		{
			inertial2Keplers(std::cout, pos, vel, keplers);
			
		}
		
		
		double accTime = dt/2;
		
		Vector3d testPos	= pos
							+ vel / 1 * accTime
							+ acc / 2 * accTime * accTime;
		
		Vector3d testVel	= vel
							+ acc / 1 * accTime;
		
		MatrixXd dKdRV;
		getKeplerInversePartials(testPos, testVel, dKdRV);

		
		Vector3d accP;
		//propagate state into the future
		{
			//calcluate accelerations
			
			Vector3d body1Pos;
			getBodyPos(t, R, 86400, body1Pos);
			
			Vector3d body2Pos = -body1Pos;
			
			Vector3d acc0;
			Vector3d acc1;
			Vector3d acc2;
			
								
			getAcceleration(1,		testPos, Vector3d::Zero(),	acc0);
			getAcceleration(0.5,	testPos, body1Pos,			acc1);
			getAcceleration(0.5,	testPos, body2Pos,			acc2);
			
			if (useCFM)
			{
				acc = acc0;
			}
			else
			{
				acc	= acc1
					+ acc2;
			}
			
			accP = acc - acc0;
			
			double relativeMag = acc.norm() / acc0.norm();
			
// 			std::cout << " " << relativeMag;
		}
		
		if (outputIntegrator)
		{
			pos	= pos
				+ vel	/ 1 * dt
				+ acc	/ 2 * dt * dt;
			
			vel = vel
				+ acc	/ 1 * dt;
		}
		else
		{
			VectorXd dK = dKdRV * dRVdA * accP;
// 			std::cout << "\ndKdRV:\n" << dKdRV << "\n";
// 			std::cout << "\ndRVdA:\n" << dRVdA << "\n";
// 			std::cout << "\ndK:\n" << dK << "\n";
			
			keplers += dK;
			
			keplers(KEPLER::M) += dM * dt;
			if (keplers(KEPLER::M) > 2 * PI)
			{
				keplers(KEPLER::M) -= 2 * PI;
			}
		}
		
		t += dt;
	}
	
	std::cout << std::fixed;
	std::cout << std::setprecision(3);
	std::cout << "\nFinal Conds:\n";
	
	std::cout << "\npos: " << pos.transpose() 
			  << "\nvel: " << vel.transpose() << "\n";
	
	std::cout << "\ndt:  "	<< dt;
	std::cout << "\nR:   "	<< R;
	std::cout << "\nCFM: "	<< useCFM;
	std::cout << "\nInt: "	<< outputIntegrator;
	std::cout << "\n";

}
#endif



// #include <malloc.h>

// size_t bucket = 0;
// 
// static void* plumber_hook(size_t size, const void* caller);
// static void* plumber_hook(size_t size, const void* caller)
// {
// 	void*	result;
// 	
// 	/* Restore all old hooks */
// 	/* Call recursively */
// 	__malloc_hook		= 0;
// 	{
// 		result = malloc(size);
// 	}
// 	__malloc_hook		= plumber_hook;
// 
// 	bucket += size;
// 	
// 	return result;
// }
// 
// 
// template<typename T>
// size_t plumberTest(T& t)
// {
// 	//begin plumbing
// 	bucket = 0;
// 		
// 	__malloc_hook	= plumber_hook;
// 	{
// 		T newT = t;
// 	}
// 	__malloc_hook	= 0;
// 	
// 	return bucket;
// }

#include "sinex.hpp"
#include "acsStream.hpp"

void plumber()
{
// 	static map<string, size_t>	plumberMap;
// 	
// 	size_t New;
// 	string v;
// 	
// 	printf("Checking plumbing:\n");
// 	v = "nav";			New = plumberTest(nav			);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "ephMap";		New = plumberTest(nav.ephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "gephMap";		New = plumberTest(nav.gephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "sephMap";		New = plumberTest(nav.sephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pephMap";		New = plumberTest(nav.pephMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pclkMap";		New = plumberTest(nav.pclkMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "satNavMap";	New = plumberTest(nav.satNavMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "tecMap";		New = plumberTest(nav.tecMap	);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	v = "pcMap";		New = plumberTest(nav.pcMap		);	printf("%15s has %15ld drops added, %15ld in bucket\n", v.c_str(), (New - plumberMap[v]), New); 	plumberMap[v] = New;
// 	
// 	printf("\n");
}


void outputMeas(
			Trace&		trace,   	///< Trace to output to
			GTime 		time,
			KFMeas		meas)
{
	trace << std::endl << "+ Meas" << std::endl;
	
	tracepdeex(2, trace, "#\t%19s\t%15s\t%15s\t%15s\n", "Time", "Observed", "Meas Noise", "Design Mat");

	for (int i = 0; i < meas.A.rows(); i++)
	{
		tracepdeex(2, trace, "*\t%19s\t%15.4f\t%15.9f", time.to_string(0).c_str(), meas.Y(i), meas.R(i, i));

		for (int j = 0; j < meas.A.cols(); j++)		tracepdeex(2, trace, "\t%15.5f", meas.A(i, j));
		tracepdeex(2, trace, "\n");
	}
	trace << "- Meas" << std::endl;
}

// modified from testClockParams() to test chi^2 computation
std::random_device					randoDev;
std::mt19937						randoGen(randoDev());
std::normal_distribution<double>	rando(0, 8);

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
		outputMeas(std::cout, gtime, combinedMeas);

		kfState.filterKalman(std::cout, combinedMeas, false);

		std::cout << std::endl << "Post-fit:" << std::endl;
		kfState.outputStates(std::cout);
	}
//	exit(0);
}

void doDebugs()
{
// 	printf("\n\n\nsensible string test\n");
// 	string test = "heres a string";
// 	
// 	printf("%s\n", test);
// 	test+= "some more\n";
// 	printf("%s\n", test);
// 	exit(0);
	
// 	orbitTests();
// 	exit(0);
// 	integrator2();
// 	lamnda();
// 	exit(0);
// 	basicIntegrator();

//	testOutlierDetection();
//	exit(0);
}

