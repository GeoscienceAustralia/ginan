
// #pragma GCC optimize ("O0")

#include <iostream>
#include <vector>

using std::vector;

#include "minimumConstraints.hpp"
#include "eigenIncluder.hpp"
#include "algebraTrace.hpp"
#include "coordinates.hpp"
#include "navigation.hpp"
#include "mongoWrite.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "algebra.hpp"
#include "sinex.hpp"
#include "trace.hpp"
#include "enums.h"

void minSiteData(
	Trace&			trace,
	KFState&		kfStateStations,
	string			suffix,
	map<int, bool>&	usedMap)
{
	Block block(trace, (string)"SITE/DATA" + suffix);

	tracepdeex(0, trace, "#\t%4s\t%9s\t%9s\t%8s\t%8s\t%8s\t%8s\t%8s\t%8s\t%8s\t%s\n",
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

	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{
		if (key.type	!= KF::REC_POS)		continue;
		if (key.rec_ptr	== nullptr)			continue;
		if (key.num		!= 0)				continue;

		Receiver& rec = *key.rec_ptr;

		//get all of the position elements for this station
		map<KFKey, int>	kfKeyMap;
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i;
			kfKeyMap[kfKey] = i;
		}

		MatrixXd filterVar;
		Vector3d filterPos = kfStateStations.getSubState(kfKeyMap, &filterVar);

		string constraint;
		if (usedMap[index])
		{
			constraint = "!";
		}

		Vector3d aprioriPos = rec.minconApriori;

		VectorPos pos = ecef2pos(filterPos);

		Vector3d deltaR = filterPos - aprioriPos;

		Matrix3d E;
		pos2enu(pos, E.data());
		Matrix3d enuCovariance	= E * filterVar * E.transpose();
		Vector3d enuResidual	= E * deltaR;

		tracepdeex(0, trace, "@\t%4s\t%9.4f\t%9.4f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%8.3f\t%s\n",
				rec.id.c_str(),
				pos.latDeg(),
				pos.lonDeg(),
				pos.hgt(),
				enuResidual(1) * 1000,
				enuResidual(0) * 1000,
				enuResidual(2) * 1000,
				sqrt(enuCovariance(1,1)) * 1e3,
				sqrt(enuCovariance(0,0)) * 1e3,
				sqrt(enuCovariance(2,2)) * 1e3,
				constraint.c_str());
	}
}

void minOrbitData(
	Trace&			trace,
	KFState&		kfStateStations,
	string			suffix,
	map<int, bool>&	usedMap,
	FrameSwapper&	frameSwapper)
{
	Block block(trace, (string)"ORBIT/DATA" + suffix);

	tracepdeex(0, trace, "#\t%4s\t%9s\t%9s\t%12s\t%10s\t%10s\t%10s\t%10s\t%10s\t%10s\t%s\n",
			"Sat",
			"Lat",
			"Lon",
			"Height",
			"resR(mm)",
			"resT(mm)",
			"resN(mm)",
			"sig_R",
			"sig_Y",
			"sig_N",
			"Constraint");

	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{
		if (key.type	!= KF::ORBIT)		continue;
		if (key.num		!= 0)				continue;

		//get all of the position elements for this station
		map<KFKey, int>	kfKeyMap;
		for (int i = 0; i < 6; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i;
			kfKeyMap[kfKey] = i;
		}

		MatrixXd filterVar;
		VectorXd filterState = kfStateStations.getSubState(kfKeyMap, &filterVar);

		Vector3d filterPos = filterState.head(3);
		Vector3d filterVel = filterState.tail(3);

		string constraint;
		if (usedMap[index])
		{
			constraint = "!";
		}

		auto& satNav = nav.satNavMap[key.Sat];

		Vector3d aprioriPos	= satNav.aprioriPos;

		VectorPos pos = ecef2pos(frameSwapper((VectorEci)filterPos));

		Vector3d deltaR = filterPos - aprioriPos;

		Matrix3d E = ecef2rac(filterPos, filterVel);

		Matrix3d rtnCovariance	= E * filterVar.topLeftCorner(3,3) * E.transpose();
		Vector3d rtnResidual	= E * deltaR;

		tracepdeex(0, trace, "@\t%4s\t%9.4f\t%9.4f\t%12.3f\t%10.3f\t%10.3f\t%10.3f\t%10.3f\t%10.3f\t%10.3f\t%s\n",
				key.Sat.id().c_str(),
				pos.latDeg(),
				pos.lonDeg(),
				pos.hgt(),
				rtnResidual(0) * 1000,
				rtnResidual(1) * 1000,
				rtnResidual(2) * 1000,
				sqrt(rtnCovariance(0,0)) * 1e3,
				sqrt(rtnCovariance(1,1)) * 1e3,
				sqrt(rtnCovariance(2,2)) * 1e3,
				constraint.c_str());
	}
}

void zeroAndPush(int index, MatrixXd& R, KFState& kfStateTrans, KFMeasEntryList& measList)
{
	R.row(index).setZero();
	R.col(index).setZero();

	// Add null measurement and continue, it's needed for inverse later
	KFMeasEntry meas(&kfStateTrans);
	measList.push_back(meas);
}

void mincon(
	Trace&				trace,
	KFState&			kfStateStations,
	MinconStatistics*	minconStatistics_ptr0,
	MinconStatistics*	minconStatistics_ptr1,
	bool				commentSinex,
	KFState*			kfStateTransform_ptr)
{
	// Reference: Estimating regional deformation from a combination of space and terrestrial geodetic data - Appendix E
	// Perform LSQ/Kalman filter to determine transformation state

	if (acsConfig.output_mincon)
	{
		std::cout << "\n" << "Writing backup point for minimum constraints to " << acsConfig.mincon_filename;

		spitFilterToFile(kfStateStations, E_SerialObject::FILTER_PLUS, acsConfig.mincon_filename);
	}

	if (acsConfig.mincon_only)
	{
		long int startPos = -1;
		E_SerialObject type = getFilterTypeFromFile(startPos, acsConfig.mincon_filename);
	}

	//Determine transformation state
	KFState kfStateTrans;

	kfStateTrans.FilterOptions::operator=(acsConfig.minconOpts);
	kfStateTrans.id							= "MINIMUM";
	kfStateTrans.output_residuals			= acsConfig.output_residuals;
	kfStateTrans.outputMongoMeasurements	= acsConfig.mongoOpts.output_measurements;

	kfStateTrans.measRejectCallbacks.push_back(deweightStationMeas);

	KFMeasEntryList	measList;
	KFMeasEntryList	measListCulled;

	InitialState xlateInit = initialStateFromConfig(acsConfig.minconOpts.translation);
	InitialState rtateInit = initialStateFromConfig(acsConfig.minconOpts.rotation);
	InitialState scaleInit = initialStateFromConfig(acsConfig.minconOpts.scale);
	InitialState delayInit = initialStateFromConfig(acsConfig.minconOpts.delay);

	MatrixXd R = kfStateStations.P;

	vector	<int>		indices;
	map		<int, bool>	usedMap;

	bool	hasStations		= false;
	bool	hasSatellites	= false;


	ERPValues erpv = getErp(nav.erp, kfStateStations.time);

	FrameSwapper frameSwapper(kfStateStations.time, erpv);

	VectorEcef xUnitEcef = (Vector3d) Vector3d::UnitX();
	VectorEcef yUnitEcef = (Vector3d) Vector3d::UnitY();
	VectorEcef zUnitEcef = (Vector3d) Vector3d::UnitZ();

	VectorEci xUnitEci = frameSwapper(xUnitEcef);
	VectorEci yUnitEci = frameSwapper(yUnitEcef);
	VectorEci zUnitEci = frameSwapper(zUnitEcef);

	for (auto& [key, index] : kfStateStations.kfIndexMap)
	{

		if (key.type != KF::REC_POS && key.type != KF::ORBIT)
		{
			zeroAndPush(index, R, kfStateTrans, measList);
			continue;
		}

		if	(   key.type == KF::ORBIT
			&&( key.num >= 3
			  ||acsConfig.minconOpts.constrain_orbits == false))
		{
			zeroAndPush(index, R, kfStateTrans, measList);
			continue;
		}

		Vector3d	aprioriPos	= Vector3d::Zero();
		Matrix3d	aprioriVar	= Matrix3d::Zero();
		Matrix3d	filterVar	= Matrix3d::Zero();
		string		str;

		if (key.type == +KF::REC_POS)
		{
			if (key.rec_ptr	== nullptr)
			{
				BOOST_LOG_TRIVIAL(error)
				<< "Error: rec_ptr is null during mincon";

				continue;
			}

			auto& rec		= *key.rec_ptr;
			auto& recOpts	= acsConfig.getRecOpts(rec.id);

			aprioriPos		= rec.minconApriori;
			aprioriVar		= rec.aprioriVar								* SQR(recOpts.mincon_scale_apriori_sigma);
			filterVar		= kfStateStations.P.block(index, index, 3, 3)	* SQR(recOpts.mincon_scale_filter_sigma);
			str				= rec.id;

			hasStations = true;
		}
		else if (key.type == +KF::ORBIT)
		{
			auto& satNav = nav.satNavMap[key.Sat];

			if (satNav.aprioriPos.isZero())
			{
				zeroAndPush(index, R, kfStateTrans, measList);
				continue;
			}

			auto& satOpts = acsConfig.getSatOpts(key.Sat);

			aprioriPos		= satNav.aprioriPos;
			aprioriVar		= Matrix3d::Identity()							* SQR(satOpts.mincon_scale_apriori_sigma);
			filterVar		= kfStateStations.P.block(index, index, 3, 3)	* SQR(satOpts.mincon_scale_filter_sigma);
			str				= key.Sat.id();

			hasSatellites = true;
		}

		bool used = true;

		MatrixXd noise = aprioriVar + filterVar;

		if	( aprioriVar(0,0) <= 0
			&&acsConfig.minconOpts.transform_unweighted == false)
		{
			zeroAndPush(index, R, kfStateTrans, measList);
			continue;
		}

		if	( aprioriVar(0,0) <= 0
			||aprioriPos.isZero())
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

		BOOST_LOG_TRIVIAL(debug) << "\n" << str << "Noises" << "\n" << aprioriVar << "\n" << filterVar;

		//get all of the position elements for this thing
		Vector3d	filterPos = Vector3d::Zero();
		Vector3d	filterVel = Vector3d::Zero();

		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i;
			kfStateStations.getKFValue(kfKey, filterPos(i));
		}

		if (key.type == +KF::ORBIT)
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey = key;
			kfKey.num = i + 3;
			kfStateStations.getKFValue(kfKey, filterVel(i));
		}

		Vector3d dRdX;
		Vector3d dRdY;
		Vector3d dRdZ;
		Vector3d dRdT;
		Vector3d dRdThetaX;
		Vector3d dRdThetaY;
		Vector3d dRdThetaZ;

		if		(key.type == +KF::REC_POS)
		{
			dRdX		= xUnitEcef;
			dRdY		= yUnitEcef;
			dRdZ		= zUnitEcef;
			dRdT		= filterVel;
			dRdThetaX	= aprioriPos.cross(xUnitEcef);
			dRdThetaY	= aprioriPos.cross(yUnitEcef);
			dRdThetaZ	= aprioriPos.cross(zUnitEcef);
		}
		else if (key.type == +KF::ORBIT)
		{
			dRdX		= xUnitEci;
			dRdY		= yUnitEci;
			dRdZ		= zUnitEci;
			dRdT		= filterVel;
			dRdThetaX	= aprioriPos.cross(xUnitEci);
			dRdThetaY	= aprioriPos.cross(yUnitEci);
			dRdThetaZ	= aprioriPos.cross(zUnitEci);
		}

		Vector3d deltaR = filterPos - aprioriPos;

		if (acsConfig.minconOpts.full_vcv == false)
		{
			R.middleRows(index, 3).setZero();
			R.middleCols(index, 3).setZero();
		}

		if	(  acsConfig.minconOpts.full_vcv == false
			&& used)
		{
			R.block(index, index, 3, 3) = noise;
		}

		for (short xyz = 0; xyz < 3; xyz++)
		{
			KFKey obsKey;
			obsKey.str		= str;
			obsKey.num		= xyz;
			obsKey.comment	= "MINCON";

			KFMeasEntry meas(&kfStateTrans, obsKey);

			if (xlateInit.estimate)
			{
				meas.addDsgnEntry({KF::XFORM_XLATE, {}, "", 0,	"M"		},	dRdX(xyz),				xlateInit);
				meas.addDsgnEntry({KF::XFORM_XLATE, {}, "", 1,	"M"		},	dRdY(xyz),				xlateInit);
				meas.addDsgnEntry({KF::XFORM_XLATE, {}, "", 2,	"M"		},	dRdZ(xyz),				xlateInit);
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

			if (delayInit.estimate)
			{
				meas.addDsgnEntry({KF::XFORM_DELAY, {}, "", 0,	"S"		},	dRdT(xyz),				delayInit);
			}

			double innov = deltaR(xyz);

			meas.setValue(innov);


			//Add null measurement and continue, its needed for inverse later

			measList.push_back(meas);

			if (used)
			{
				int xIndex = index + xyz;
				indices.push_back(xIndex);
				usedMap[xIndex] = true;
				meas.metaDataMap["used_ptr"]	= &usedMap[xIndex];
				meas.metaDataMap["otherIndex"]	= (void*) (measList.size() - 1);	//need an index into the big measurement matrix for deweighting to get applied in 2 places

				measListCulled.push_back(meas);
			}
		}
	}

	//use a state transition to initialise elements
	kfStateTrans.stateTransition(trace, kfStateStations.time);

// 	std::cout << "\n" << "R" << "\n" << R << "\n";

	MatrixXd RR = R(indices, indices);

	KFMeas combinedMeas			(kfStateTrans, measList,		GTime::noTime(), &R);
	KFMeas combinedMeasCulled	(kfStateTrans, measListCulled,	GTime::noTime(), &RR);

	for (auto& metaDataMap : combinedMeasCulled.metaDataMaps)
	{
		metaDataMap["otherNoiseMatrix_ptr"] = &combinedMeas.R;
	}

	if (kfStateTrans.lsqRequired)
	{
		trace << "\n" << "------- LEAST SQUARES FOR MINIMUM CONSTRAINTS TRANSFORMATION --------" << "\n";
		kfStateTrans.leastSquareInitStates(trace, combinedMeasCulled, false, &kfStateTrans.dx);
		kfStateTrans.dx = VectorXd::Zero(kfStateTrans.x.rows());
		kfStateTrans.outputStates(trace, "/MINCON_TRANSFORM_LSQ");
	}

	trace << "\n" << "------- FILTERING FOR MINIMUM CONSTRAINTS TRANSFORMATION --------" << "\n";

	kfStateTrans.filterKalman(trace, combinedMeasCulled, "/MINCON_TRANSFORM");

	kfStateTrans.outputStates(trace, "/MINCON_TRANSFORM");

	if (kfStateTransform_ptr)
	{
		*kfStateTransform_ptr = kfStateTrans;
	}

	mongoStates(kfStateTrans,
				{
					.suffix		= "/MINCON_TRANSFORM",
					.instances	= acsConfig.mongoOpts.output_states,
					.queue		= acsConfig.mongoOpts.queue_outputs
				});

	KFState oldStateStations = kfStateStations;

	//Do kalman filter on original state using pseudomeasurements
	MatrixXd K;
	VectorXd v;
	MatrixXd H;

	auto& P = kfStateStations.P;

	switch (acsConfig.minconOpts.application_mode)
	{
		case E_Mincon::PSEUDO_OBS:
		{
			//rename for some semblance of resemblance to the 'bible'
			auto& T		= combinedMeas.H;
			auto& Theta	= kfStateTrans.x;

			//create a subset matrix for positions only
			vector<int> posIndices;
			for (int i = 0; i < T.rows(); i++)
			{
				if (T.row(i).any())
				{
					posIndices.push_back(i);
				}
			}

			//calculate position deltas and their variances according to transform
			VectorXd tTheta	= T * Theta;								//will correspond to (but not be) pseudoobservations
			MatrixXd Omega	= T * kfStateTrans.P * T.transpose();		//will be pseudoobservations variances

			//just get the subset that are positions
			tTheta	= tTheta(posIndices				).eval();
			Omega	= Omega (posIndices, posIndices	).eval();


			//standard kalman filter stuff using the ordinary states
			//filter using pseudoobs for each position state

			//design matrix is subset of identity matrix
			H	= MatrixXd::Identity(kfStateStations.x.rows(), kfStateStations.x.rows());
			H	= H(posIndices, all).eval();


			//calculate kalman gain
			K = P * H.transpose() * (H * P * H.transpose() + Omega).inverse();

			//do some algebra to compute the required v values such that a subset of kalman state adjustments are the desired dx calculated previously
			//ie, find v

			//			H. K. v =			tTheta
			//	Kt. Ht.	H. K. v	= Kt. Ht.	tTheta		//normal equations

			// solve A.x = b, where:
			// x = v
			auto A = K.transpose() * H.transpose() * H * K;
			auto b = K.transpose() * H.transpose() * tTheta;

			v = A.ldlt().solve(b);

			if (0)
			{
				VectorXd errors = tTheta - H * K * v;
				std::cout << "\n" << "tTheta:"	<< "\n" << tTheta	<< "\n";
				std::cout << "\n" << "H:"		<< "\n" << H		<< "\n";
				std::cout << "\n" << "errors:"	<< "\n" << errors	<< "\n";
			}
			break;
		}
		case E_Mincon::WEIGHT_MATRIX:
		case E_Mincon::VARIANCE_INVERSE:
		case E_Mincon::COVARIANCE_INVERSE:
		{
			int numXform	= kfStateTrans.x.rows() - 1;
			int numStates	= combinedMeas.R.rows();

			v =  kfStateTrans.x.bottomRows(numXform);

			//generalised inverse (Ref:E.3)
			MatrixXd T = combinedMeas.H.bottomRightCorner(numStates, numXform);
			MatrixXd W	= MatrixXd::Zero(numStates, numStates);
			// 	std::cout << "\n" << "R" << "\n" << combinedMeasCulled.R<< "\n";

			switch (acsConfig.minconOpts.application_mode)
			{
				case E_Mincon::WEIGHT_MATRIX:
				{
					for (int i = 0; i < numStates; i++)
					{
						double val = combinedMeas.R(i,i);
						if (val)
						{
							W(i,i) = 1 / val;
						}
					}
					break;
				}
				case E_Mincon::VARIANCE_INVERSE:
				{
					for (auto& [kfKey, index] : kfStateStations.kfIndexMap)
					{
						if (kfStateStations.P(index, index))
						{
							W(index, index) = 1 / kfStateStations.P(index, index);
						}
					}
					break;
				}
				case E_Mincon::COVARIANCE_INVERSE:
				{
					vector<int>	validP;
					for (auto& [kfKey, index] : kfStateStations.kfIndexMap)
					{
						if (kfStateStations.P(index, index))
						{
							validP.push_back(index);
						}
					}

					MatrixXd inverse = kfStateStations.P(validP, validP).inverse();

					int row = 0;
					for (auto& i : validP)
					{
						int col = 0;
						for (auto& j : validP)
						{
							W(i, j) = inverse(row, col);
							col++;
						}
						row++;
					}

					break;
				}
			}



			W = ((W + W.transpose()) / 2).eval();

			// 	std::cout << "\n" << "P" << "\n" << kfStateStations.P << "\n";
// 				std::cout << "\n" << "W_" << "\n" << W << "\n";
			//
// 				std::cout << "\n" << "T" << "\n" << T << "\n";

			MatrixXd TW		= T.transpose() * W;

			MatrixXd TWT	= TW * T;

// 			std::cout << "\n" << "TWT" << "\n" << TWT << "\n";

			auto QQ = TWT.triangularView<Eigen::Upper>().transpose();
			LDLT<MatrixXd> solver;
			solver.compute(QQ);
			if (solver.info() != Eigen::ComputationInfo::Success)
			{
				std::cout << "Mincon borked." << "\n";
				return;
			}

			H = MatrixXd::Zero(numXform, numStates);
			H.rightCols(numStates) = solver.solve(TW);
			if (solver.info() != Eigen::ComputationInfo::Success)
			{
				std::cout << "Mincon borked!" << "\n";
				return;
			}

// 			std::cout << "\n" << "TWT" << "\n" << TWT << "\n";
// 			std::cout << "\n" << "TW" << "\n" << TW << "\n";
// 			std::cout << "\n" << "TDash" << "\n" << H << "\n";

			//calculate kalman gain
			K = P * H.transpose() * (H * P * H.transpose()/* + Omega*/).inverse();

			break;
		}
	}

	//standard kalman filter things again, but do manually since already have K calculated
	{
		KFState& kfState = kfStateStations;

		//use a state transition to ensure output logs are complete
		kfState.stateTransition(std::cout, kfState.time);

		trace << "\n" << " -------DOING KALMAN FILTER WITH PSEUDO ELEMENTS FOR MINIMUM CONSTRAINTS --------" << "\n";

		if (kfState.rts_basename.empty() == false)
		{
			spitFilterToFile(kfState, E_SerialObject::FILTER_MINUS, kfState.rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
		}

		kfState.dx	= (K * v					).eval();
		kfState.x	= (kfState.x - kfState.dx	).eval();
		kfState.P	= (kfState.P - K * H * P	).eval();

		if (isPositiveSemiDefinite(P) == false)
		{
			std::cout << "\n" << "WARNING, NOT PSD";
		}

		if (kfState.rts_basename.empty() == false)
		{
			kfState.metaDataMap["SKIP_PREV_RTS"] = "TRUE";
			spitFilterToFile(kfState.metaDataMap,	E_SerialObject::METADATA,		kfState.rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);

			spitFilterToFile(kfState,				E_SerialObject::FILTER_PLUS,	kfState.rts_basename + FORWARD_SUFFIX, acsConfig.pppOpts.queue_rts_outputs);
		}
	}

	if (hasStations)
	{
		minSiteData	(trace, oldStateStations,	" Pre Constraint",	usedMap);
		minSiteData	(trace, kfStateStations,	" Post Constraint",	usedMap);
	}

	if (hasSatellites)
	{
		minOrbitData(trace, oldStateStations,	" Pre Constraint",	usedMap, frameSwapper);
		minOrbitData(trace, kfStateStations,	" Post Constraint",	usedMap, frameSwapper);
	}

	for (auto type : {KF::REC_POS,	KF::ORBIT})
	{
		if (type == KF::REC_POS		&& hasStations		== false)	continue;
		if (type == KF::ORBIT		&& hasSatellites	== false)	continue;

		for (auto before		: {true, false})
		for (auto& [key, index] : kfStateStations.kfIndexMap)
		{
			if (key.type	!= type)	{	continue;	}
			if (key.num		!= 0)		{	continue;	}

			Vector3d filterPos;
			for (int i = 0; i < 3; i++)
			{
				if (before)		filterPos(i) = oldStateStations	.x(index + i);
				else			filterPos(i) = kfStateStations	.x(index + i);
			}

			Vector3d filterVel;
			if (type == KF::ORBIT)
			for (int i = 0; i < 3; i++)
			{
				if (before)		filterVel(i) = oldStateStations	.x(index + i + 3);
				else			filterVel(i) = kfStateStations	.x(index + i + 3);
			}

			string		aggregatedUsed	= "Agg-Used";
			string		aggregatedAll	= "Agg-All";
			string		str;
			Vector3d	aprioriPos		= Vector3d::Zero();
			if		(type == KF::REC_POS)	{	auto& rec		= *key.rec_ptr;				aprioriPos = rec.minconApriori;		str = key.str;		}
			else if	(type == KF::ORBIT)		{	auto& satNav	= nav.satNavMap[key.Sat];	aprioriPos = satNav.aprioriPos;		str = key.Sat.id();	}

			Vector3d deltaR	= filterPos - aprioriPos;

			Matrix3d E		= Matrix3d::Zero();

			if		(type == KF::REC_POS)	{	VectorPos pos = ecef2pos(filterPos);		pos2enu(pos, E.data());	}
			else if	(type == KF::ORBIT)		{	E = ecef2rac(filterPos, filterVel);									}

			Vector3d frameResidual = E * deltaR;


			for (auto& rmsStatisticsMap_ptr : {minconStatistics_ptr0, minconStatistics_ptr1})
			for (auto& entry : {aggregatedUsed, aggregatedAll, str})
			{
				if (rmsStatisticsMap_ptr == nullptr)
				{
					continue;
				}

				if	( entry == aggregatedUsed
					&&usedMap[index] == false)
				{
					continue;
				}

				auto& rmsStatisticsMap = *rmsStatisticsMap_ptr;

				for (int i = 0; i < 4; i++)
				{
					string component = "3D";
					if		(type == KF::REC_POS)
					switch (i)
					{
						case 0:		component = "E";	break;
						case 1:		component = "N";	break;
						case 2:		component = "U";	break;
					}
					else if	(type == KF::ORBIT)
					switch (i)
					{
						case 0:		component = "R";	break;
						case 1:		component = "T";	break;
						case 2:		component = "N";	break;
					}

					if (i < 3)		rmsStatisticsMap[entry][component][before].sum	+= SQR(frameResidual(i));
					else			rmsStatisticsMap[entry][component][before].sum	+= frameResidual.squaredNorm();

									rmsStatisticsMap[entry][component][before].count++;
				}
			}
		}
	}

	if (commentSinex)
	{
		for (auto& [key, index] : kfStateStations.kfIndexMap)
		{
			if	(  key.num	== 0
				&& key.type	== KF::REC_POS)
			{
				sinexAddComment((string)" Minimum Constraints Stations: " + key.str + (usedMap[index] ? "   used" : " unused"));
			}
		}

		for (auto& [key, index] : kfStateTrans.kfIndexMap)
		{
			if (key.type == +KF::ONE)
				continue;

			char line[128];
			snprintf(line, sizeof(line), " Minimum Constraints Transform: %12s:%c %+9f %6s +- %8f",
					KF::_from_integral(key.type)._to_string(),
					'X' + key.num,
					kfStateTrans.x(index),
					key.comment.c_str(),
					sqrt(kfStateTrans.P(index,index)));

			sinexAddComment(line);
		}
	}
}

void outputMinconStatistics(
	Trace&				trace,
	MinconStatistics&	minconStatistics,
	const string&		suffix)
{
	Block block(trace, (string)"MINCON STATISTICS" + suffix);
	trace << "# Position RMS changed in meters:\n";

	for (auto& [str,		rmsStatistics]	: minconStatistics)
	for (auto& [component,	statistics]		: rmsStatistics)
	for (auto	before						: {true, false})
	{
		auto& entry = statistics[before];
		double val = sqrt(entry.sum / entry.count);
		if (isnan(val))
		{
			val = 0;
		}

		int lvl = 3;
		if (str.length() > 4)
		{
			lvl = 0;
		}

		if (before)		tracepdeex(lvl, trace, "& %-10s - from: %12.6f ",				str.c_str(), val);
		else			tracepdeex(lvl, trace, "to: %12.6f in %2s over %5d entries\n",	val, component.c_str(), entry.count);
	}
}

KFState minconOnly(
	Trace&			trace,
	ReceiverMap&	receiverMap)
{
	long int startPos = -1;
	E_SerialObject type = getFilterTypeFromFile(startPos, acsConfig.mincon_filename);
	if (type != +E_SerialObject::FILTER_PLUS)
	{
		return KFState();
	}

	trace << "\n" << "Performing minimum constraints using dataset saved to " << acsConfig.mincon_filename << "\n";

	KFState kfState;
	bool pass = getFilterObjectFromFile(type, kfState, startPos, acsConfig.mincon_filename);
	if (pass == false)
	{
		return KFState();
	}

	GTime time = kfState.time;

	ERPValues erpv = getErp(nav.erp, time);

	FrameSwapper frameSwapper(time, erpv);

	tryPrepareFilterPointers(kfState, receiverMap);

	for (auto& [key, index] : kfState.kfIndexMap)
	{
		if	( key.type != KF::ORBIT
			||key.num != 0)
		{
			continue;
		}

		auto&	satNav = nav.satNavMap[key.Sat];

		SatPos satPos;
		satPos.Sat			= key.Sat;
		satPos.satNav_ptr	= &satNav;

		bool pass =	satpos(nullStream, time, time, satPos, {E_Source::PRECISE, E_Source::BROADCAST}, E_OffsetType::COM, nav);
		if (pass == false)
		{
			BOOST_LOG_TRIVIAL(warning) << "Warning: No sat pos found for " << satPos.Sat.id() << ".";
			continue;
		}

		satNav.aprioriPos = frameSwapper(satPos.rSatCom);
	}

	for (auto& [id, rec] : receiverMap)
	{
		sinexPerEpochPerStation(nullStream, time, rec);

		bool dummy;
		selectAprioriSource(nullStream, rec, time, dummy, kfState);

		rec.minconApriori = rec.aprioriPos;
	}

	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		kfState.stateTransitionMap[kfKey][kfKey][0] = 1;
	}

	kfState.outputStates(trace, "/UNCONSTRAINED");

	{
		MinconStatistics minconStatistics;

		mincon(trace, kfState, &minconStatistics);

		outputMinconStatistics(trace, minconStatistics);
	}

	kfState.outputStates(trace, "/CONSTRAINED");

	exit(0);
}
