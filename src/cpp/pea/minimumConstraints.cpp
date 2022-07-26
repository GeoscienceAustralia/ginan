
// #pragma GCC optimize ("O0")

#include <iostream>
#include <vector>
#include <list>

using std::vector;
using std::list;

#include "eigenIncluder.hpp"
#include "algebraTrace.hpp"
#include "streamTrace.hpp"
#include "acsConfig.hpp"
#include "station.hpp"
#include "algebra.hpp"
#include "enums.h"

#define MINCONONLY_FILENAME "minconOnly.bin"


void minSiteData(
	Trace&			trace,
	KFState&		kfStateStations,
	string			suffix		= "",
	map<int, bool>*	usedMap_ptr	= nullptr)
{
	trace << std::endl << "+ Site Data" + suffix;
	tracepdeex(0, trace, "\n#\t%4s\t%9s\t%9s\t%8s\t%8s\t%8s\t%8s\t%8s\t%8s\t%8s\t%s", 
			"Site", 
			"Lat", 
			"Lon", 
			"Height", 
			"resN(mm)", 
			"resE(mm)", 
			"resH(mm)", 
			"sig_N", 
			"sig_E", 
			"sig_H", 
			"Constraint");
	
// 	if (0)
	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{
		if (key.type	!= KF::REC_POS)		continue;
		if (key.rec_ptr	== nullptr)			continue;
		if (key.num		!= 0)				continue;
		
		Station&	rec			= *key.rec_ptr;
		auto		stationOpts	= acsConfig.getMinConOpts(rec.id);

		bool used = true;
	
		if (usedMap_ptr)
		{
			auto& usedMap	= *usedMap_ptr;
			
			used = usedMap[index];
		}

		//get all of the position elements for this station
		map<KFKey, int>	kfKeyMap;
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i;
			kfKeyMap[kfKey] = i;
		}
		
		MatrixXd filterVar;
		VectorXd filterPos = kfStateStations.getSubState(kfKeyMap, &filterVar);
		
		string constraint = "";
		if (used)
		{
			constraint = "!";
		}
		
		Vector3d aprioriPos = rec.snx.pos;

		//filter states are corrections only, need to add to apriori to get vector arms for cross products etc.
		Vector3d statePos = aprioriPos + filterPos;
		
		double pos[3];
		ecef2pos(statePos, pos);
		
		Vector3d enuResidual;
		Matrix3d enuCovariance;
		Matrix3d E;
		xyz2enu(pos, E.data());
		enuCovariance	= E * filterVar * E.transpose();
		enuResidual		= E * filterPos;
		
		tracepdeex(0, trace, "\n@\t%4s\t%9.4f\t%9.4f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%s", 
				rec.id.c_str(),
				pos[0] * R2D, 
				pos[1] * R2D, 
				pos[2],
				enuResidual(1) * 1000,
				enuResidual(0) * 1000,
				enuResidual(2) * 1000,
				sqrt(enuCovariance(1,1)) * 1e3,
				sqrt(enuCovariance(0,0)) * 1e3,
				sqrt(enuCovariance(2,2)) * 1e3,
				constraint.c_str());
	}
	trace << std::endl << "- Site Data" + suffix;
}

void mincon(
	Trace&					trace,
	KFState&				kfStateStations)
{
	// Reference: Estimating regional deformation from a combination of space and terrestrial geodetic data - Appendix E
	// Perform LSQ/Kalman filter to determine transformation state
	// Use trasformation state as pseudo observations on original kalman filter state
	// : using generalised inverse as design matrix, (incorporating weights) and zero variance on translation pseudo elements.

	if (acsConfig.mincon_only)
	{
		long int startPos = -1;
		E_SerialObject type = getFilterTypeFromFile(startPos, MINCONONLY_FILENAME);
		if (type == +E_SerialObject::NONE)
		{
			std::cout << std::endl << "Writing backup point for minimum constraints to " << MINCONONLY_FILENAME; 
			spitFilterToFile(kfStateStations, E_SerialObject::FILTER_PLUS, MINCONONLY_FILENAME);
		}
	}
	
	//Determine transformation state
	KFState			kfStateTrans;
	
	kfStateTrans.id					= "MINIMUM";
	kfStateTrans.max_filter_iter	= acsConfig.minCOpts.max_filter_iter;
	kfStateTrans.max_prefit_remv	= acsConfig.minCOpts.max_prefit_remv;
	kfStateTrans.inverter			= acsConfig.minCOpts.inverter;
	kfStateTrans.sigma_threshold	= acsConfig.minCOpts.sigma_threshold;
	kfStateTrans.sigma_check		= acsConfig.minCOpts.sigma_check;
	kfStateTrans.w_test				= acsConfig.minCOpts.w_test;
	kfStateTrans.chi_square_test	= acsConfig.minCOpts.chi_square_test;
	kfStateTrans.chi_square_mode	= acsConfig.minCOpts.chi_square_mode;
	kfStateTrans.output_residuals	= acsConfig.output_residuals;
	
	kfStateTrans.measRejectCallbacks.push_back(deweightStationMeas);
	
	KFMeasEntryList	measList;
	KFMeasEntryList	measListCulled;

	InitialState xlateInit = initialStateFromConfig(acsConfig.minCOpts.translation);
	InitialState rtateInit = initialStateFromConfig(acsConfig.minCOpts.rotation);
	InitialState scaleInit = initialStateFromConfig(acsConfig.minCOpts.scale);
	
	MatrixXd R = kfStateStations.P;
	
	vector	<int>		indices;
	map		<int, bool>	usedMap;
	
	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{
		if (key.type != KF::REC_POS)
		{
			//Add null measurement and continue, its needed for inverse later
			KFMeasEntry meas(&kfStateTrans);
			measList.push_back(meas);
			R.row(index).setZero();
			R.col(index).setZero();
			continue;
		}

		if (key.rec_ptr == nullptr)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: rec_ptr is null during mincon";
			
			continue;
		}
		
		Station&	rec			= *key.rec_ptr;
		auto		stationOpts	= acsConfig.getMinConOpts(rec.id);

		bool used = true;
		if (stationOpts.noise[0] <= 0)
		{
			R.row(index).setZero();
			R.col(index).setZero();
			
			used = false;
		}

		if (key.num != 0)
		{
			//deal with all position dimensions at same time when (num == 0)
			continue;
		}
		
		//get all of the position elements for this station
		Vector3d filterPos;
		Vector3d filterVar;
		
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i;
			kfStateStations.getKFValue(kfKey, filterPos(i), &filterVar(i));
		}

		Vector3d aprioriPos = rec.snx.pos;

		//filter states are corrections only, need to add to apriori to get vector arms for cross products etc.
		Vector3d statePos = aprioriPos + filterPos;

// 		std::cout << std::endl << aprioriPos.transpose() << "\t" << filterPos.transpose();
		
		Vector3d linearPos = aprioriPos;//+ filterPos;
		
		Vector3d dRdThetaX = linearPos.cross(Vector3d{1,0,0});
		Vector3d dRdThetaY = linearPos.cross(Vector3d{0,1,0});
		Vector3d dRdThetaZ = linearPos.cross(Vector3d{0,0,1});

		Vector3d deltaR = statePos - aprioriPos;
		
		Vector3d enuNoise;
		for (int i = 0; i < 3; i++)
		{
			int j = i;
			if (j >= stationOpts.noise.size())	
				j = stationOpts.noise.size() - 1;
			
			enuNoise(i) = stationOpts.noise[j];
		}
		
		MatrixXd oldVarianceXYZ;
		if (acsConfig.minCOpts.scale_by_vcv)	oldVarianceXYZ = kfStateStations.P.block(index, index, 3, 3);
		else									oldVarianceXYZ = Matrix3d::Identity();
		
		double pos[3];
		ecef2pos(linearPos, pos);
		
		Matrix3d E;
		xyz2enu(pos, E.data());
		
		Matrix3d S = enuNoise.asDiagonal();
		
		Matrix3d oldVarianceNED = E				* oldVarianceXYZ * E.transpose();
		Matrix3d newVarianceNED = S				* oldVarianceNED * S.transpose();
		Matrix3d newVarianceXYZ = E.transpose()	* newVarianceNED * E;
		
		if (acsConfig.minCOpts.full_vcv == false)
		{
			R.middleRows(index, 3).setZero();
			R.middleCols(index, 3).setZero();
		}
		
		if	(  acsConfig.minCOpts.full_vcv == false
			&& used)
		{
			R.block(index, index, 3, 3) = newVarianceXYZ;
		}
		
		for (short xyz = 0; xyz < 3; xyz++)
		{
			ObsKey obsKey = {{}, rec.id, { (char)('X' + xyz)}};

			KFMeasEntry meas(&kfStateTrans, obsKey);

			if (xlateInit.estimate)
			{
				meas.addDsgnEntry({KF::XFORM_XLATE, {}, "", xyz			},	1,						xlateInit);
			}

			if (rtateInit.estimate)
			{
				meas.addDsgnEntry({KF::XFORM_RTATE,	{}, "", 0,	"MAS"	},	dRdThetaX(xyz) * MAS2R,	rtateInit);
				meas.addDsgnEntry({KF::XFORM_RTATE,	{}, "", 1,	"MAS"	},	dRdThetaY(xyz) * MAS2R,	rtateInit);
				meas.addDsgnEntry({KF::XFORM_RTATE,	{}, "", 2,	"MAS"	},	dRdThetaZ(xyz) * MAS2R,	rtateInit);
			}

			if (scaleInit.estimate)
			{
				meas.addDsgnEntry({KF::XFORM_SCALE, {}, "", 0,	"PPB"	},	aprioriPos(xyz) * 1e-9,	scaleInit);
			}
			
			double innov = deltaR(xyz);
			
			meas.setValue(innov);


			//Add null measurement and continue, its needed for inverse later
			
			measList.push_back(meas);
			
			if (used)
			{
				int xIndex = index + xyz;
				indices.push_back(xIndex);
				usedMap[xIndex] = used;
				meas.metaDataMap["used_ptr"] = &usedMap[xIndex];
				
				measListCulled.push_back(meas);
			}
		}
	}

	//use a state transition to initialise elements
	kfStateTrans.stateTransition(trace, kfStateStations.time);

// 	std::cout << std::endl << "R" << std::endl << R << std::endl;
	
	MatrixXd RR = R(indices, indices);
	
	KFMeas combinedMeas			= kfStateTrans.combineKFMeasList(measList,			GTime::noTime(), &R);
	KFMeas combinedMeasCulled	= kfStateTrans.combineKFMeasList(measListCulled,	GTime::noTime(), &RR);
	
	if (kfStateTrans.lsqRequired)
	{
		std::cout	<< std::endl << "------- LEAST SQUARES FOR MINIMUM CONSTRAINTS TRANSFORMATION --------" << std::endl;
		kfStateTrans.leastSquareInitStates(trace, combinedMeasCulled, false, &kfStateTrans.dx);
		kfStateTrans.outputStates(trace, " LSQ");
	}
	
	std::cout	<< std::endl << "------- FILTERING FOR MINIMUM CONSTRAINTS TRANSFORMATION --------" << std::endl;
	trace		<< std::endl << "------- FILTERING FOR MINIMUM CONSTRAINTS TRANSFORMATION --------" << std::endl;
	kfStateTrans.filterKalman(trace, combinedMeasCulled);
	
	kfStateTrans.outputStates(trace, " Transform");

	//Do kalman filter on original state using pseudomeasurements

	//generalised inverse (Ref:E.3)
	MatrixXd T = combinedMeas.H;
	MatrixXd W	= MatrixXd::Zero(combinedMeas.R.rows(), combinedMeas.R.cols());

// 	std::cout << std::endl << "R" << std::endl << combinedMeasCulled.R<< std::endl;

	for (auto& [kfKey, index] : kfStateStations.kfIndexMap)
	{
		if (kfStateStations.P(index, index))
			W(index, index) = 1 / kfStateStations.P(index, index);	//todo aaron, should this be all of them, or just a subset?
	}
// 	MatrixXd W = combinedMeas.R.diagonal().asDiagonal();

	MatrixXd TW		= T.transpose() * W;
	MatrixXd TWT	= T.transpose() * W * T;
	
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
	
	
// 		std::cout << std::endl << "T" << std::endl << T.transpose() << std::endl;
// 		std::cout << std::endl << "W" << std::endl << W << std::endl;
// 		std::cout << std::endl << "TWT" << std::endl << TWT << std::endl;
// 		std::cout << std::endl << "TW" << std::endl << TW << std::endl;
// 		std::cout << std::endl << "TDash" << std::endl << Tdash << std::endl;
	
	minSiteData(trace, kfStateStations, " Pre Constraint", &usedMap);	
	
	
	VectorXd oldStateStationsX = kfStateStations.x;
	
	//perform kalman filtering using pseudo elements
	{
		KFState&	kfState = kfStateStations;

		KFMeas pseudoMeas;
		int rows = kfStateTrans.x.rows() - 1;
		pseudoMeas.V = - kfStateTrans.x.bottomRows(rows);
		pseudoMeas.H = Tdash;
		pseudoMeas.R = MatrixXd::Zero	(rows, rows);
		pseudoMeas.obsKeys.resize		(rows);

		//use a state transition to ensure output logs are complete
		kfState.stateTransition(std::cout, kfState.time + 0.000001);	//dont repeat the last epoch
		
		trace << std::endl << " -------DOING KALMAN FILTER WITH PSEUDO ELEMENTS FOR MINIMUM CONSTRAINTS --------" << std::endl;

		if (kfState.rts_basename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_MINUS, kfState.rts_basename + FORWARD_SUFFIX);
		}

		MatrixXd Pp = kfState.P;
		VectorXd xp = kfState.x;
		VectorXd dx = kfState.dx;

		bool pass = kfState.kFilter(std::cout, pseudoMeas, xp, Pp, dx, 0, kfState.x.rows(), 0, pseudoMeas.V.rows());

		if (pass == false)
		{
			trace << "FILTER FAILED" << std::endl;
		}

		if (isPositiveSemiDefinite(Pp) == false)
		{
			std::cout << std::endl << "WARNING, NOT PSD";
		}
		
		kfState.x	= xp;
		kfState.P	= Pp;
		kfState.dx	= dx;

		if (kfState.rts_basename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_PLUS, kfState.rts_basename + FORWARD_SUFFIX);
		}
	}
	
	minSiteData(trace, kfStateStations, " Post Constraint", &usedMap);
	
	
	for (auto usedOnly : {true, false})
	{
		int count = 0;
		
		trace << std::endl << std::endl << "Minimum constraints: Position RMS changed\n";
		
		double	normsSum	[2][4]	= {};
		int		normsCount	[2][4]	= {};
		
		for (auto before		: {true, false})
		for (auto& [key, index] : kfStateStations.kfIndexMap)
		{
			if (key.type != KF::REC_POS)	{	continue;	}
			if (key.num != 0)				{	continue;	}
			
			if	(  usedOnly
				&& usedMap[index] == false)
			{
				continue;
			}
	
			Vector3d filterPos;
			for (int i = 0; i < 3; i++)
			{
				if (before)		filterPos(i) = oldStateStationsX(index + i);
				else			filterPos(i) = kfStateStations.x(index + i);
			}
			
			Station&	rec			= *key.rec_ptr;
			Vector3d	aprioriPos	= rec.snx.pos;
			count++;

			//filter states are corrections only, need to add to apriori to get vector arms for cross products etc.
			Vector3d statePos = aprioriPos + filterPos;
	
			double pos[3];
			ecef2pos(statePos, pos);
			
			Vector3d enuResidual;
			Matrix3d E;
			xyz2enu(pos, E.data());
			enuResidual = E * filterPos;
	
			for (int i = 0; i < 4; i++)
			{
				if (i < 3)		normsSum[before][i]	+= SQR(enuResidual(i));
				else			normsSum[before][i]	+= enuResidual.squaredNorm();	
				
				normsCount[before][i]++;
			}
		}
		
		for (int i = 0; i < 4; i++)
		{
			for (auto before : {true, false})
			{
				if (before)		trace << "from: "	<< sqrt(normsSum[before][i] / normsCount[before][i]);
				else			trace << "\tto: "	<< sqrt(normsSum[before][i] / normsCount[before][i]);
				
			}
			
			switch (i)
			{
				case 0:		trace << "\tfor E  component\n";	break;
				case 1:		trace << "\tfor N  component\n";	break;
				case 2:		trace << "\tfor U  component\n";	break;
				case 3:		trace << "\tfor 3D component\n";	break;
			}
		}
		trace << "for " << (count / 2) << " stations ";
		
		if (usedOnly)		trace << "that were constrained";
		else				trace << "in the complete set of stations";
	}
	
	kfStateStations.outputStates(trace, " Constrained");
	
	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{
		if	(  key.num	== 0
			&& key.type	== KF::REC_POS)
		{
			sinex_add_comment((string)" Minimum Constraints Stations: " + key.str + (usedMap[index] ? "   used" : " unused"));
		}
	}
	
	for (auto& [key, index] : kfStateTrans.kfIndexMap)
	{
		if (index == 0)
			continue;
		char line[128] = "";
		snprintf(line, sizeof(line), " Minimum Constraints Transform: %12s:%c %+9f %6s +- %8f",
				KF::_from_integral(key.type)._to_string(),
				'X' + key.num,
				kfStateTrans.x(index),
				key.comment.c_str(),
				sqrt(kfStateTrans.P(index,index)));
				 
		sinex_add_comment(line);
	}
}

KFState minconOnly(
	Trace&		trace,
	StationMap&	stationMap)
{
	long int startPos = -1;
	E_SerialObject type = getFilterTypeFromFile(startPos, MINCONONLY_FILENAME);
	if (type != +E_SerialObject::FILTER_PLUS)
	{
		return KFState();
	}
	
	trace << std::endl << "Performing minimum constraints using dataset saved to " << MINCONONLY_FILENAME << std::endl;
	
	KFState kalmanPlus;
	bool pass = getFilterObjectFromFile(type, kalmanPlus, startPos, MINCONONLY_FILENAME);
	if (pass == false)
	{
		return KFState();
	}
	
	int n = kalmanPlus.x.rows();
	kalmanPlus.Z = MatrixXd::Zero(n,n);
	
	tryPrepareFilterPointers(kalmanPlus, &stationMap);
	
	for (auto& [kfKey, index] : kalmanPlus.kfIndexMap)
	{
		kalmanPlus.stateTransitionMap[kfKey][kfKey]	= {1,	0};
	}
	
	for (auto& [id, rec] : stationMap)
	{
		sinexPerEpochPerStation(kalmanPlus.time, rec);
	}
	
	kalmanPlus.outputStates(trace, " Unconstrained");
	{
		KFState& kfState = kalmanPlus;
		mincon(trace, kalmanPlus);
	}
	
	exit(0);
	return kalmanPlus;
}
