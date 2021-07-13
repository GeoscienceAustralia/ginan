
#pragma GCC optimize ("O0")

#include <iostream>
#include <random>


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
		Obs&	obs,		///<
		lc_t&	lcBase,		///<
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

		kfState.stateTransition(std::cout, 1);

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
		kfStateStations.stateTransition(trace, 0);

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



template<class M>
csvToMatrix<M>::csvToMatrix(){
	//cout<<"CsvToMatrix imported Correctly"<<endl;
}

template<class M>

M  csvToMatrix<M>::load_csv(const string path) {

	//cout<<"**********************************************"<<endl;
	//cout<<"Reading the data only from current active sheet "<<endl;
	//cout<<"Please Make sure the correct sheet is selected  "<<endl;
	//cout<<"**********************************************"<<endl;

	std::ifstream indata;
	indata.open(path);
	std::string line;
	std::vector<double> values;
	unsigned int rows = 0;
	while (std::getline(indata, line)) {
		std::stringstream lineStream(line);
		std::string cell;
		while (std::getline(lineStream, cell, ',')) {
			values.push_back(std::stod(cell));
		}
		++rows;
	}
	return Map<const Matrix<typename M::Scalar, M::RowsAtCompileTime, M::ColsAtCompileTime, RowMajor>>(values.data(), rows, values.size()/rows);
}

#include <iostream>
#include <vector>
#include <map>
#include <eigen3/Eigen/Dense>
#include <fstream>

using namespace Eigen;
using namespace std;


/*
	----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	| * State vectors  dim(x) = n =  1*3                                                                                                                                                                                                                                                                            |
	|* x1 = bias                                                                                                                                                                                                                                                                                                                           |
	| * x2 = d(bias)/dt                                                                                                                                                                                                                                                                                                              |
	| * x3 = dx2/dt                                                                                                                                                                                                                                                                                                                    |

	|Control variable u_{k} = 0 ;                                                                                                                                                                                                                                                                                         |
	|Measurement is scalar , by the definition of current problem, so measurement noise R is also scalar . Assigning some random value to noise R,                                         |
	|* Observerd variable z is just bias .So  z will be a scalar .                                                                                                                                                                                                                              |
	|* dim(z) = m = 1                                                                                                                                                                                                                                                                                                                |
	|*Time steps del(T)  = 0.1                                                                                                                                                                                                                                                                                              |
	|                                                                                                                                                                                                                                                                                                                                                 |
	| * Matrices :                                                                                                                                                                                                                                                                                                                        |
	|*  F : Make up for F .                                                                                                                                                                                                                                                                                                        |
	|* B : Take B = 0                                                                                                                                                                                                                                                                                                                  |
	|* P  : Defines confidence in the "prediction" of state variables , by virtue of covariance between different variables . Take its values  to be anything , KF f                    |
	|* will adjust its values.                                                                                                                                                                                                                                                                                                  |
	|* H is the helper matrix to get the dimensions right. In this case dim(H) = 1*3                                                                                                                                                                                |
	|* S_{K} will be calculated                                                                                                                                                                                                                                                                                             |
	|* Q_{K}  : Process Noise Matrix is assumed to be 0.                                                                                                                                                                                                                                        |
	---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
* */

int isgmain()
{

	cout << "Start program" << endl;

	// === Initialise filter ===

	int num_clocks = 31;
	int m = num_clocks;  // # observations
	int num_params = 2; // # params to model each clock bias
	int n = 3 * num_clocks;  // # states (when num_params < 3, higher order terms exist but are not used) //n = 93


	// x - State vector
	VectorXd x_pre		= VectorXd::Zero(n);
	VectorXd x_priori	= VectorXd::Zero(n);
	VectorXd x_post		= VectorXd::Zero(n);

	// P - Covariance matrix for the state vector
	MatrixXd P_pre		= MatrixXd::Zero(n, n);      // P(k-1|k-1)
	MatrixXd P_priori	= MatrixXd::Zero(n, n);   // P(k|k-1)
	MatrixXd P_post		= MatrixXd::Zero(n, n);     // P(k|k)

	for (int i = 0; i < n; ++i)
	{
		P_pre(i, i) = 1000;
	}



	// F - State transition matrix
	double delta_t = 30; // seconds

	MatrixXd F		= MatrixXd::Zero(n, n);
	MatrixXd F_base = MatrixXd::Zero(3, 3); // F for one clock
	F_base <<   1,  delta_t,    0.5 * delta_t* delta_t,
				0,  1,          delta_t,
				0,  0,          1;

	for (int i = num_params; i < 3; ++i)   // Limit # params by zeroing out cols of F. E.g. if 3rd column is zeroed-out, accel will have no effect on the other terms.
	{
		F_base.col(i) *= 0;                                 //num_params =3
	}

	for (int i = 0; i < num_clocks; ++i)   // Repeat F_base (3x3) along the diagonals of F
	{
		F.block(3 * i, 3 * i, 3, 3) = F_base;
	}

	// u - Control input, B - control matrix (both not used)
	int u_k = 0;
	MatrixXd B = MatrixXd::Zero(n, n);

	// Q - Process noise
	MatrixXd Q		= MatrixXd::Zero(n, n);
	MatrixXd Q_base	= MatrixXd::Zero(3, 3); // Q for one clock
	/*
	// This approach models process noise as a piecewise constant white sequence - i.e. the process noise is assumed to be constant over each sampling period and independent between periods.
	MatrixXd G = VectorXd::Zero(3); // https://en.wikipedia.org/wiki/Kalman_filter#Example_application,_technical, https://physics.stackexchange.com/questions/146491/what-are-the-equations-for-motion-with-constant-jerk
	if (num_params == 1)      G << delta_t, 0, 0;     // G - How higher-order parameters (e.g. drift for 1-param, accel for 2-param, jerk for 3-param) affect lower-order params)
	else if (num_params == 2) G << 1.0 / 2.0*pow(delta_t, 2), delta_t, 0;
	else if (num_params == 3) G << 1.0 / 6.0*pow(delta_t, 3), 1.0 / 2.0*pow(delta_t, 2), delta_t;
	else {
		cout << "Error: Unexpected value in num_params: " << num_params << endl;
		return 0;
	}
	Q_base = G * G.transpose();
	*/

	// This approach discretises continuous-time white noise (more relevant in our situation than the piecewise constant white noise sequence approach)
	// See https://github.com/rlabbe/filterpy/blob/master/filterpy/common/discretization.py, and
	// Bar-Shalom. "Estimation with Applications To Tracking and Navigation". John Wiley & Sons, 2001. Page 272.


	if (num_params == 1)
	{
		Q_base <<   delta_t,    0,  0,
					0,          0,  0,
					0,          0,  0;
	}
	else if (num_params == 2)
	{
		Q_base <<   pow(delta_t, 3) / 3.0, pow(delta_t, 2) / 2.0,   0,
					pow(delta_t, 2) / 2.0, delta_t,                 0,
					0,                     0,                       0;
	}
	else if (num_params == 3)
	{

		Q_base <<   pow(delta_t, 5) / 20.0, pow(delta_t, 4) / 8.0, pow(delta_t, 3) / 6.0,
					pow(delta_t, 4) / 8.0,  pow(delta_t, 3) / 3.0, pow(delta_t, 2) / 2.0,
					pow(delta_t, 3) / 6.0,  pow(delta_t, 2) / 2.0, delta_t;
	}
	else
	{
		cout << "Error: Unexpected value in num_params: " << num_params << endl;
		return 0;
	}

	MatrixXd Q_unit = MatrixXd::Zero(n, n);

	for (int i = 0; i < num_clocks; ++i)
	{
		Q_unit.block(3 * i, 3 * i, 3, 3) = Q_base;
	}

	// H - Observation matrix
	MatrixXd H = MatrixXd::Zero(m, n); // m=31

	for (int i = 0; i < num_clocks; ++i)   //num_clocks =31
	{
		H(i, 3 * i) = 1;
	}

	// R - Measurement noise
	MatrixXd R = MatrixXd::Zero(m, m);

	for (int i = 0; i < m; ++i)
	{
		R(i, i) = 1e-11;  // IGS clock bias var = ~1e-11
	}

	// z - Observation vector
	// Load data from csv file
	//string file_in = "./sample1.csv";

	string file_in = "../tests/all_sats.csv";

	cout << "Reading from " << file_in << endl;
	cout << "Start program" << endl;

	csvToMatrix<MatrixXd> obj1;

	MatrixXd z_stream = obj1.load_csv(file_in);
	cout << "Start program3" << endl;
	z_stream.transposeInPlace();

	assert(z_stream.rows() >= m);


	z_stream = z_stream.topRows(m).eval();

	// === Run filter ===
	cout << "Running filter" << endl;
	ofstream out;
	ofstream prefit_out;
	ofstream state_vector;

	out.			open("../tests/output.tra");
	prefit_out.		open("../tests/prefit.tra");
	state_vector.	open("../tests/state_vector.tra");

	for (int Q_pow = -1; Q_pow > -40 ; --Q_pow)
	{
		Q = Q_unit * pow(10, Q_pow);
		prefit_out << pow(10, Q_pow) << " ";

		for (int i = 0; i < n; ++i)          //n = 93
		{
			P_pre(i, i) = 1000;
		}

		cout << "Q_pow: " << Q_pow << endl;

		cout << "P_pre: " << P_pre << endl;
		VectorXd prefit_sum_of_squares = VectorXd::Zero(m);  //m = 31

		for (int k = 0; k < z_stream.cols(); k++)               //z_streams.cols() = 2880
		{
			out << endl << "Epoch: " << k << " ";

			// Predict
			x_priori = F * x_pre;
			P_priori = F * P_pre * F.transpose() + Q;

			out << endl << "x_priori: " << x_priori.transpose() << " ";

			// Update
			VectorXd z = z_stream.col(k);

			VectorXd y = z - H * x_priori;          //  dim(H)  = 31 * 93 , dim(x_priori) = 93*1 , y = 31 * 1

			prefit_out << y(0) << "\t";

			if (k > 20)   // Filter convergence
			{
				for (int i = 0; i < m; ++i)
				{
					prefit_sum_of_squares(i) += pow(y(i), 2);
				}
			}

			MatrixXd S		= (H * P_priori * H.transpose()) + R;
			MatrixXd S_inv	= S.inverse();
			MatrixXd K		= P_priori * H.transpose() * S_inv;
			x_post			= x_priori + K * y;

			state_vector << x_post.transpose() << endl;

			P_post = (MatrixXd::Identity(n, n) - K * H) * P_priori;

			out << endl << "K: " << K.transpose() << " ";

			// k = k-1:
			x_pre = x_post;
			P_pre = P_post;

			out << endl;
		}

		prefit_out << endl << "prefit_sum_of_squares" << prefit_sum_of_squares.transpose();
		prefit_out << endl << "y\t";
	}

	cout << "Program finished!" << endl;
	exit(0);
}

void newFilter()
{
	KFState kfState;
	initFilterTrace(kfState, "rts.1", "TEST", -1);

	KFMeasEntryList kfMeasEntryList = KFMeasEntryList();

	for (int i = 0; i < 10; i++)
	{
		std::cout << i << endl;
		std::cout << std::endl;

		kfState.initFilterEpoch();

		kfMeasEntryList = KFMeasEntryList();

		KFMeasEntry	codeMeas(&kfState);
		codeMeas.setValue(i + rando(randoDev));
		codeMeas.setNoise(15);

		KFKey recPosKey			= {KF::REC_POS};
		KFKey recPosKey2		= {KF::REC_POS, {}, "die"};
		KFKey recClockKey		= {KF::REC_SYS_BIAS};
		KFKey recClockRateKey	= {KF::REC_SYS_BIAS_RATE};

		InitialState recClkInit		= {123,	567,	SQR(0)};
		InitialState recClkRateInit	= {0,	1000,	SQR(0.01)};

		codeMeas.addDsgnEntry(recClockKey,						+1,	recClkInit);
		kfState.setKFTransRate(recClockKey, recClockRateKey,	+1,	recClkRateInit);

		if (i < 5)
		{
			codeMeas.addDsgnEntry(recPosKey,					+1,	recClkInit);
		}
		else
		{
			kfState.removeState(recPosKey);
		}

		kfMeasEntryList.push_back(codeMeas);


		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, 1);

		if (i == 4)
			continue;

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		if (kfState.lsqRequired)
		{
			kfState.lsqRequired = false;
			std::cout << std::endl << " -------INITIALISING NETWORK USING LEAST SQUARES--------" << std::endl;

			kfState.leastSquareInitStates(std::cout, combinedMeas);
		}

		kfState.filterKalman(std::cout, combinedMeas, false);

		kfState.outputStates(std::cout);

	}


	std::cout << "smoothing:!" << endl;

	RTS_Process(kfState, true);

	std::cout << "Program finished!" << endl;
	exit(0);
}


void doubleOffsets()
{
	KFState kfState;

	KFKey recPosKey				= {KF::REC_POS};
	InitialState recClkInit		= {0,	100,	SQR(0)};

	for (int i = 0; i < 100; i++)
	{
		std::cout << i << endl;
		std::cout << std::endl;

		kfState.initFilterEpoch();

		KFMeasEntryList kfMeasEntryList;

		KFMeasEntry	codeMeas(&kfState);
		codeMeas.setNoise(15);

		double pos;
		kfState.getKFValue(recPosKey, pos);

		double tide = sin(PI / 20 * i) * 10;

		pos += tide;

		double meas  = tide;
		codeMeas.addDsgnEntry(recPosKey, +1,	recClkInit);

		codeMeas.setValue(meas - pos);

		kfMeasEntryList.push_back(codeMeas);

		//add process noise to existing states as per their initialisations.
		kfState.stateTransition(std::cout, 1);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(std::cout, combinedMeas, false);

		kfState.outputStates(std::cout);
	}

	std::cout << "Program finished!" << endl;
	exit(0);
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

// void ballistics()
// {
//
// 	Vector3d tide = Vector3d::Zero();
// 	Station rec;
//
// 	list<Vector3d> sats;
//
// 	sats.push_back({+20000000, 0, 0});
// 	sats.push_back({-20000000, 0, 0});
// 	sats.push_back({0, +20000000, 0});
// 	sats.push_back({0, -20000000, 0});
// 	sats.push_back({0, 0, +20000000});
// 	sats.push_back({0, 0, -20000000});
//
// 	Vector3d leo		= {42164000, 10, 10000};
// 	Vector3d leoVel		= Vector3d::Zero();
//
// 	acsConfig.tropOpts.corr_mode = 0;
// 	acsConfig.ionoOpts.corr_mode = 0;
//
// 	rec.rtk.sol.sppRRec = leo;
// 	rec.rtk.tt = 30;
//
// 	SatStat satStat;
// 	satStat.el = 1.5;
// 	satStat.vs = 1;
//
// 	SatNav satNav;
//
// 	acsConfig.satpcv = 0;
// 	acsConfig.phase_windup = 0;
//
//
// 	for (double t = 0; t < 60 * 60 * 24; t += 30)
// 	{
// 		ObsList obsList;
//
// 		for (auto& sat : sats)
// 		{
// 			Obs obs;
// 			obs.satStat_ptr = &satStat;
// 			obs.satNav_ptr	= &satNav;
//
// 			if (t == 0)
// 			{
// 				obs.Sigs[L1].L_corr_m = (leo - sat).norm();
// 				obs.Sigs[L1].P_corr_m = (leo - sat).norm();
// 				obs.rSat = sat;
// 			}
// 			obs.time.time = t;
//
// 			obsList.push_back(obs);
// 		}
//
//
//
// 		ppp_filter(nullStream, obsList, tide, rec.rtk, rec.station);
//
// 		rec.rtk.pppState.outputStates(std::cout);
// 	}
//
// }

void ambiguities()
{
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
		kfState.stateTransition(std::cout, 1);

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
		kfState.stateTransition(std::cout, 1);

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
		kfState.stateTransition(std::cout, 1);

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
		kfState.stateTransition(std::cout, 1);

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
		kfState.stateTransition(std::cout, 1);

		//combine the measurement list into a single matrix
		KFMeas combinedMeas = kfState.combineKFMeasList(kfMeasEntryList);

		kfState.filterKalman(nullStream, combinedMeas, false);

		kfState.outputStates(std::cout);
	}

	std::cout << "Program finished!" << endl;
}

void minimumTest(Trace& trace);


void mjdtest()
{
	int doms[] = {31,28,31,30,31,30,31,31,30,31,30,31};



	double time[6] = {};
	int yds[6] = {};
	time[0] = 1995;
	time[1] = 4;
	time[2] = 30;
	time[3] = 23;
	time[4] = 59;
	time[5] = 59;
	epoch2yds(time, yds);

	std::cout << yds[0] << " < " << yds[1] << " " << yds[2] << std::endl;
	return;




	if (0)
	for (int m = 0; m < 12; m++)
	for (int i = 1; i <= doms[m]; i++)
	{
		time[0] = 2019;
		time[1] = m + 1;
		time[2] = i;
		int jd = ymdhms2jd(time);
		jd2ymdhms(jd + JD2MJD,	time);
		std::cout << std::endl << time[0] << " " << time[1] << " " << time[2] << " " << time[3] ;
	}

}


#include "biasSINEX.hpp"

int read_biasnx_fil(
	string&		filename,
	bias_io_opt	opt);
GTime sinex_time_text(const char* buff);
GTime sinex_time_text(
	string& line);
void biastest()
{
	Obs obs;
	obs.satNav_ptr					= &nav.satNavMap[obs.Sat];
// 	obs.satStat_ptr					= &rec.rtk.satStatMap[obs.Sat];
	obs.satOrb_ptr					= &nav.orbpod.satOrbitMap[obs.Sat];
	updatenav(obs);
	obs.Sat = SatSys(E_Sys::GPS, 1);
	string line = "2019:199:00030";
	obs.time = sinex_time_text(line);
	
	bias_io_opt biasopt;
	biasopt.OSB_biases = false;
	biasopt.DSB_biases = true;
	biasopt.SSR_biases = false;
	biasopt.SAT_biases = true;
	biasopt.REC_biases = false;
	biasopt.HYB_biases = false;
	biasopt.COD_biases = true;
	biasopt.PHS_biases = true;	
	string file = "/data/acs/pea/proc/exs/products/CAS0MGXRAP_20191990000_01D_01D_DCB.BSX";
	read_biasnx_fil(file, biasopt);
	double bias[2];
	double bvar[2];
	E_ObsCode obsCode;
	obsCode = E_ObsCode::L1C;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L1W;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L2C;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L2S;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L2L;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L2X;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L2W;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L5Q;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
	
	obsCode = E_ObsCode::L5X;
	inpt_hard_bias(std::cout, obs, obsCode, bias, bvar, biasopt);
	printf("\n%f\n", bias[0]);
}

void doDebugs()
{
// 	biastest();
// 	mjdtest();
// 	exit(0);
// 	minimumTest(std::cout);
// 	doubleOffsets();

// 	newFilter();
//	newFilter();
//  	testClockParams();
// 	isgmain();
}
