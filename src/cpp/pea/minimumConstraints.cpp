
// #pragma GCC optimize ("O0")

#include <iostream>
#include <vector>
#include <list>

using std::vector;
using std::list;

#include "algebraTrace.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "enums.h"

#include "eigenIncluder.hpp"

void minimum(
	Trace&					trace,
	KFState&				kfStateStations)
{
	// Reference: Estimating regional deformation from a combination of space and terrestrial geodetic data - Appendix E
	// Perform LSQ/Kalman filter to determine transformation state
	// Use trasformation state as pseudo observations on original kalman filter state
	// : using generalised inverse as design matrix, (incorporating weights) and zero variance on translation pseudo elements.

	//Determine transformation state
	KFState			kfStateTrans;
	KFMeasEntryList	measList;

	kfStateTrans.initFilterEpoch();

	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{
		KFMeasEntry meas(&kfStateTrans);

		if (key.type != KF::REC_POS)
		{
			//Add null measurement and continue, its needed for inverse later
			measList.push_back(meas);
			continue;
		}

		Station&	rec			= *key.rec_ptr;
		auto		stationOpts	= acsConfig.getMinConOpts(rec.id);

		if (stationOpts.noise <= 0)
		{
			//Add null measurement and continue, its needed for inverse later
			measList.push_back(meas);
			continue;
		}

		if (key.num != 0)
		{
			//deal with all position dimensions at same time when (num == 0)
			continue;
		}

		//get all of the position elements for this station
		Vector3d filterPos;
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i;
			kfStateStations.getKFValue(kfKey, filterPos(i));
		}

		Vector3d aprioriPos = rec.snx.pos;

		//filter states are corrections only, need to add to apriori to get vector arms for cross products etc.
		Vector3d statePos = aprioriPos + filterPos;

		Vector3d dRdThetaX = statePos.cross(Vector3d{1,0,0});
		Vector3d dRdThetaY = statePos.cross(Vector3d{0,1,0});
		Vector3d dRdThetaZ = statePos.cross(Vector3d{0,0,1});

		Vector3d deltaR = statePos - aprioriPos;

		for (short xyz = 0; xyz < 3; xyz++)
		{
			ObsKey obsKey = {{}, rec.id, { (char)('X' + xyz)}};

			KFMeasEntry meas(&kfStateTrans, obsKey);

			if (acsConfig.minCOpts.estimate_translation)
			{
				meas.addDsgnEntry({KF::XFORM_XLATE, {}, "", xyz	},	1);
			}

			if (acsConfig.minCOpts.estimate_rotation)
			{
				meas.addDsgnEntry({KF::XFORM_RTATE,	{}, "", 0	},	dRdThetaX(xyz) * 1e-6);	//convert to micro rads
				meas.addDsgnEntry({KF::XFORM_RTATE,	{}, "", 1	},	dRdThetaY(xyz) * 1e-6);
				meas.addDsgnEntry({KF::XFORM_RTATE,	{}, "", 2	},	dRdThetaZ(xyz) * 1e-6);
			}

			if (acsConfig.minCOpts.estimate_scale)
			{
				meas.addDsgnEntry({KF::XFORM_SCALE},				aprioriPos(xyz));
			}

			meas.setNoise(stationOpts.noise);
			meas.setValue(deltaR(xyz));

			measList.push_back(meas);
		}
	}

	//use a state transition to initialise elements
	kfStateTrans.stateTransition(trace, kfStateStations.time);

	KFMeas combinedMeas = kfStateTrans.combineKFMeasList(measList);

	trace << std::endl << " -------INITIALISING MINIMUM CONSTRAINTS TRANSFORMATION USING LEAST SQUARES--------"<< std::endl;
	kfStateTrans.leastSquareInitStates(trace, combinedMeas);

	kfStateTrans.outputStates(trace, " Transform");

	//Do kalman filter on original state using pseudomeasurements

	//generalised inverse (Ref:E.3)
	MatrixXd T = combinedMeas.A;
	VectorXd W = (1 / combinedMeas.R.diagonal().array()).matrix();
	for (int i = 0; i < W.rows(); i++)
	{
		if (std::isinf(W(i)))
		{
			W(i) = 0;
		}
	}
	
	MatrixXd TW		= T.transpose() * W.asDiagonal();
	MatrixXd TWT	= TW * T;
	
	auto QQ = TWT.bottomRightCorner(TWT.rows()-1, TWT.cols()-1).triangularView<Eigen::Upper>().adjoint();
	LDLT<MatrixXd> solver;
	solver.compute(QQ);
	if (solver.info() != Eigen::ComputationInfo::Success)
	{
		std::cout << "Mincon borked." << std::endl;
		return;
	}

	MatrixXd Tdash = solver.solve(TW.bottomRows(TW.rows()-1));
	if (solver.info() != Eigen::ComputationInfo::Success)
	{
		std::cout << "Mincon borked!" << std::endl;
		return;
	}
	
	//perform kalman filtering using pseudo elements
	{
		KFState&	kfState = kfStateStations;

		KFMeas pseudoMeas;
		int rows = kfStateTrans.x.rows() - 1;
		pseudoMeas.V = - kfStateTrans.x.bottomRows(rows);
		pseudoMeas.A = Tdash;
		pseudoMeas.R = MatrixXd::Zero	(rows, rows);
		pseudoMeas.obsKeys.resize		(rows);

		//use a state transition to ensure output logs are complete
		kfState.initFilterEpoch();
		kfState.stateTransition(std::cout, kfState.time + 0.000001);	//dont repeat the last epoch
		
		trace << std::endl << " -------DOING KALMAN FILTER WITH PSEUDO ELEMENTS FOR MINIMUM CONSTRAINTS --------" << std::endl;

		if (kfState.rts_filename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_MINUS, kfState.rts_forward_filename);
		}

		MatrixXd Pp = kfState.P;
		VectorXd xp = kfState.x;
		VectorXd dx = kfState.dx;

		bool pass = kfState.kFilter(std::cout, pseudoMeas, xp, Pp, dx, 0, kfState.x.rows(), 0, pseudoMeas.V.rows());

		if (pass == false)
		{
			trace << "FILTER FAILED" << std::endl;
		}

		kfState.x	= xp;
		kfState.P	= Pp;
		kfState.dx	= dx;

		if (kfState.rts_filename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_PLUS, kfState.rts_forward_filename);
		}
	}
}
