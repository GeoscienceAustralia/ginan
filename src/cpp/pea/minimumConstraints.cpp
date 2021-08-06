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

		Station&	rec			= *key.station_ptr;
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
	kfStateTrans.stateTransition(trace, GTime::noTime());

	KFMeas combinedMeas = kfStateTrans.combineKFMeasList(measList);

	trace << std::endl << " -------INITIALISING MINIMUM CONSTRAINTS TRANSFORMATION USING LEAST SQUARES--------"<< std::endl;
	kfStateTrans.leastSquareInitStates(trace, combinedMeas);

	kfStateTrans.outputStates(trace);

	//Do kalman filter on original state using pseudomeasurements

	//generalised inverse (Ref:E.3)
	MatrixXd T = combinedMeas.A;
	VectorXd W = (1 / combinedMeas.R.array()).matrix();
	for (int i = 0; i < W.rows(); i++)
	{
		if (std::isinf(W(i)))
		{
			W(i) = 0;
		}
	}
	MatrixXd TWT	= T.transpose() * W.asDiagonal() * T;
	MatrixXd Tdash	= TWT.inverse() * T.transpose() * W.asDiagonal();

	//clear the top row as it will have failed due to zero variances before the inverse
	Tdash.row(0).setZero();

	//perform kalman filtering using pseudo elements
	{
		KFState&	kfState = kfStateStations;

		KFMeas pseudoMeas;
		int rows = kfStateTrans.x.rows() - 1;
		pseudoMeas.V = - kfStateTrans.x.bottomRows(rows);
		pseudoMeas.A = Tdash.bottomRows	(rows);
		pseudoMeas.R = VectorXd::Zero	(rows);
		pseudoMeas.obsKeys.resize		(rows);

		//use a state transition to ensure output logs are complete
		kfState.initFilterEpoch();
		kfState.stateTransition(std::cout, GTime::noTime());

		trace << std::endl << " -------DOING KALMAN FILTER WITH PSEUDO ELEMENTS FOR MINIMUM CONSTRAINTS --------" << std::endl;

		if (kfState.rts_filename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_MINUS, kfState.rts_forward_filename);
		}

		MatrixXd Pp;
		VectorXd xp;
		VectorXd dx;

		bool pass = kfState.kFilter(std::cout, pseudoMeas, xp, Pp, dx);

		if (pass == false)
		{
			trace << "FILTER FAILED" << std::endl;
		}

		kfState.x = xp;
		kfState.P = Pp;

		kfState.clampStateValues();

		if (kfState.rts_filename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_PLUS, kfState.rts_forward_filename);
		}
	}
}
