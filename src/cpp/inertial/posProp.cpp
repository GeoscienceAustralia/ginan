

// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>
#include <deque>
#include <map>

using std::deque;
using std::map;

#include "eigenIncluder.hpp"
#include "acceleration.hpp"
#include "coordinates.hpp"
#include "ubxDecoder.hpp"
#include "posProp.hpp"
#include "planets.hpp"

void propLinear(
	double 			dt,
	string&			id,
	GTime			time,
	Vector3d&		r,
	Vector3d&		v,
	Vector4d&		q,
	const Vector3d&	gyroScale,
	const Vector3d&	acclScale,
	const Vector3d&	gyroBias,
	const Vector3d&	acclBias)
{
	Quaterniond Q;
	Q.w() = q(0);
	Q.x() = q(1);
	Q.y() = q(2);
	Q.z() = q(3);

	Vector3d acclBody = Vector3d::Zero();
	{
		auto it = UbxDecoder::acclDataMaps[id].lower_bound(time);

		auto& [foundTime, accl] = *it;

		acclBody = ((accl - acclBias).array() * acclScale.array()).matrix();

// 		Vector3d other = quat * accl;

// 		double temp = other.y();
// 		other.y() = other.z();
// 		other.z() = temp;

// 			Vector3d acclEcef = (accl - inertialInit.acclBias)[2] * rRec.normalized();	//todo aaron, rotate to ecef

	}

	Vector3d accCF		= accelCentralForce(r, GM_values[E_ThirdBody::EARTH]);
	Vector3d accAccl	= Q * acclBody;

	Vector3d a = accAccl + accCF;

// 	std::cout << "\n" << "                                      " << accCF	.transpose();
// 	std::cout << "\n" << "                                      " << accAccl		.transpose();
// 	std::cout << "\n" << "Using " << a.transpose();

	Vector3d gyroBody;
	{
		auto it = UbxDecoder::gyroDataMaps[id].lower_bound(time);

		auto& [foundTime, gyro] = *it;

		gyroBody = ((gyro - gyroBias).array() * gyroScale.array()).matrix() * PI / 180;
// 	std::cout << "\n" << "                                      " << gyro	.transpose();
// 	std::cout << "\n" << "                                      " << gyroBias		.transpose();
	}

// 	std::cout << "\n" << "Guyo " << gyroBody.transpose();
	Quaterniond qBody(Eigen::AngleAxis(gyroBody.norm() * dt, gyroBody.normalized()));

	Vector3d 	rPlus = r + v * dt;
	Vector3d 	vPlus = v + a * dt;;

	Quaterniond qPlus = Q * qBody;	//todo aaron, check ordering of this

	r = rPlus;
	v = vPlus;

	q(0) = qPlus.w();
	q(1) = qPlus.x();
	q(2) = qPlus.y();
	q(3) = qPlus.z();
}


void InertialIntegrator::operator()(
	const	Inertials&	inertialInits,
			Inertials&	derivatives,
	const	double		timeOffset)
{
	GTime time = timeInit + timeOffset;

	for (int i = 0; i < inertialInits.size(); i++)
	{
		auto& inertialInit	= inertialInits	[i];
		auto& derivative	= derivatives	[i];

		if (inertialInit.exclude)
		{
			continue;
		}

		string id = inertialInit.str;

		int numParams = inertialInit.numParams;

		Vector3d acc		= Vector3d::Zero();
		Vector4d dQuat		= Vector4d::Zero();
		Matrix3d dAdPos		= Matrix3d::Zero();
		Matrix3d dAdVel		= Matrix3d::Zero();
 		MatrixXd dAdQuat	= MatrixXd::Zero(3, 4);
 		MatrixXd dAdParam	= MatrixXd::Zero(3, numParams);

		Vector3d	rRec = inertialInit.pos;
		Vector3d	vRec = inertialInit.vel;
		Vector4d	quat = inertialInit.quat.normalized();

		const double offset	= 1e-6;
		const double dt		= 0.1;

		Vector3d r0 = inertialInit.pos;
		Vector3d v0 = inertialInit.vel;
		Vector4d q0 = inertialInit.quat;
		Vector3d r_;
		Vector3d v_;
		Vector4d q_;
		MatrixXd A = MatrixXd::Zero(inertialInit.subState.x.rows(), inertialInit.subState.x.rows());

		//get propagated values with no offsets
		{
			Vector3d r = inertialInit.pos;
			Vector3d v = inertialInit.vel;
			Vector4d q = inertialInit.quat;

			Vector3d gs	= inertialInit.gyroScale;
			Vector3d as	= inertialInit.acclScale;
			Vector3d gb	= inertialInit.gyroBias;
			Vector3d ab	= inertialInit.acclBias;

			propLinear(dt, id, time, r, v, q, gs, as, gb, ab);

			r_ = r;
			v_ = v;
			q_ = q;

			derivative.pos	= (r - r0) / dt;
			derivative.vel	= (v - v0) / dt;
			derivative.quat	= (q - q0) / dt;
		}

		//get propagated values with offsets to determine sensitivtiy
		for (auto& [key, index] : inertialInit.subState.kfIndexMap)
		{
			Vector3d r	= inertialInit.pos;
			Vector3d v	= inertialInit.vel;
			Vector4d q	= inertialInit.quat;

			Vector3d gs	= inertialInit.gyroScale;
			Vector3d as	= inertialInit.acclScale;
			Vector3d gb	= inertialInit.gyroBias;
			Vector3d ab	= inertialInit.acclBias;

			if (key.type == KF::REC_POS)		{	r	(key.num) += offset;	}
			if (key.type == KF::REC_POS_RATE)	{	v	(key.num) += offset;	}
			if (key.type == KF::ORIENTATION)			{	q	(key.num) += offset;	}
			if (key.type == KF::GYRO_SCALE)		{	gs	(key.num) += offset;	}
			if (key.type == KF::ACCL_SCALE)		{	as	(key.num) += offset;	}
			if (key.type == KF::GYRO_BIAS)		{	gb	(key.num) += offset;	}
			if (key.type == KF::ACCL_BIAS)		{	ab	(key.num) += offset;	}

			Vector3d r__ = r;
			Vector3d v__ = v;
			Vector4d q__ = q;

			propLinear(dt, id, time, r, v, q, gs, as, gb, ab);

			Vector3d deltaR = (r - r__) - (r_ - r0); 		A.col(index).segment(0, 3) = deltaR / dt / offset;
			Vector3d deltaV = (v - v__) - (v_ - v0); 		A.col(index).segment(3, 3) = deltaV / dt / offset;
			Vector4d deltaQ = (q - q__) - (q_ - q0); 		A.col(index).segment(6, 4) = deltaQ / dt / offset;
		}

// 		std::cout << "\n" << "A" << "\n" << A << "\n";

		derivative.posVelQuatSTM	= A * inertialInit.posVelQuatSTM;

//       derivative.posVelQuatSTM.bottomRightCorner(3, numParams) += dAdParam;			//this is basically assuming params are unchanged and identity matrix?
	}
};


void integrateInertials(
	InertialIntegrator&	inertialPropagator,
	Inertials&			inertials,
	double				integrationPeriod,
	double 				dtRequested)
{
	if	( inertials.empty()
		||integrationPeriod == 0)
	{
		return;
	}

	double	dt			= dtRequested;
	int		steps		= round(integrationPeriod / dt);
	double	remainder	= fmod (integrationPeriod, dtRequested);

	if (steps == 0)
	{
		steps = 1;
	}

	if (remainder != 0)
	{
		double newDt = integrationPeriod / steps;

		BOOST_LOG_TRIVIAL(warning) << "Warning: Time step adjusted from " << dt << " to " << newDt;

		dt = newDt;
	}

	for (int i = 0; i < steps; i++)
	{
		double initTime	= i * dt;

		Inertials errors;
		inertialPropagator.odeIntegrator.do_step(boost::ref(inertialPropagator), inertials, initTime, dt, errors);

		for (auto error : errors)
		{
			double errorMag = error.pos.norm();
			if (errorMag > 0.001)
			{
				BOOST_LOG_TRIVIAL(warning) << " Integrator error " << errorMag << " greater than 1mm for " << error.Sat << " " << error.str;
			}
		}
	}
}

/** Get the estimated elements for a single satellite's orbit
 */
KFState getInertialFromState(
	Trace&			trace,
	string			str,
	const KFState&	kfState)
{
	map<KFKey, int> kfKeyMap;

	int index = 0;

	//find all satellite orbit related states related to this sat/str pair
	for (auto& [kfKey, unused] : kfState.kfIndexMap)
	{
		if	( kfKey.str != str)
		{
			continue;
		}

		if	(	kfKey.type == KF::REC_POS
			||	kfKey.type == KF::REC_POS_RATE
			||	kfKey.type == KF::ORIENTATION
			||( kfKey.type >= KF::BEGIN_INERTIAL_STATES
			  &&kfKey.type <= KF::END_INERTIAL_STATES))
		{
			kfKeyMap[kfKey] = index;
			index++;
		}
	}

	KFState subState;
	kfState.getSubState(kfKeyMap, subState);

	return subState;
}

Inertials prepareInertials(
	Trace&			trace,
	const KFState&	kfState)
{
	Inertials inertials;

	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{
		if	(  kfKey.type	!= KF::ORIENTATION
			|| kfKey.num	!= 0)
		{
			continue;
		}

		inertials.emplace_back(InertialState{.str = kfKey.str});
	}

	for (auto& inertial : inertials)
	{
		auto& subState	= inertial.subState;

		subState = getInertialFromState(trace, inertial.str, kfState);

		for (auto& [key, index] : subState.kfIndexMap)
		{
			if (key.type == KF::REC_POS)		{	inertial.pos		(key.num) = inertial.subState.x[index];	}
			if (key.type == KF::REC_POS_RATE)	{	inertial.vel		(key.num) = inertial.subState.x[index];	}
			if (key.type == KF::ORIENTATION)			{	inertial.quat		(key.num) = inertial.subState.x[index];	}
			if (key.type == KF::GYRO_SCALE)		{	inertial.gyroScale	(key.num) = inertial.subState.x[index];	}
			if (key.type == KF::ACCL_SCALE)		{	inertial.acclScale	(key.num) = inertial.subState.x[index];	}
			if (key.type == KF::GYRO_BIAS)		{	inertial.gyroBias	(key.num) = inertial.subState.x[index];	}
			if (key.type == KF::ACCL_BIAS)		{	inertial.acclBias	(key.num) = inertial.subState.x[index];	}
		}

		if (inertial.pos.isZero())
		{
			inertial.exclude = true;
			continue;
		}

		inertial.posVelQuatSTM	= MatrixXd::Identity(inertial.subState.kfIndexMap.size(), inertial.subState.kfIndexMap.size());
	}

	return inertials;
}

/** Apply the prediction using the filter's state transition
 */
void applyInertials(
	Trace&			trace,
	Inertials&		inertials,
	const KFState&	kfState,
	GTime			time,
	double			tgap)
{
	for (auto& inertial : inertials)
	{
		if (inertial.exclude)
		{
			continue;
		}

		auto& subState = inertial.subState;

		Vector10d inertialState;
		inertialState << inertial.pos, inertial.vel, inertial.quat;

		//Convert the absolute transition matrix to an identity matrix (already populated elsewhere) and stm-per-time matrix
		MatrixXd transition = inertial.posVelQuatSTM - MatrixXd::Identity(inertial.posVelQuatSTM.rows(), inertial.posVelQuatSTM.cols());
		transition /= tgap;

		KFKey keyI;
		keyI.str	= inertial.str;

		//use the calculated transition matrix for the main filter, and a also a test filter (substate)
		for (int i = 0; i < 10; i++)
		for (auto& [keyJ, indexJ] : subState.kfIndexMap)
		{
			if (i >= 0 && i < 3)	{	keyI.type	= KF::REC_POS;			keyI.num = i - 0;	}
			if (i >= 3 && i < 6)	{	keyI.type	= KF::REC_POS_RATE;		keyI.num = i - 3;	}
			if (i >= 6 && i < 10)	{	keyI.type	= KF::ORIENTATION;				keyI.num = i - 6;	}

			double transferIJ = transition(i, indexJ);

			kfState	.setKFTransRate(keyI, keyJ, transferIJ);
			subState.setKFTransRate(keyI, keyJ, transferIJ);
		}

		//run a test run of the transition on the substate to see how much we miss by
		subState.stateTransition(trace, time);

		//calculate the state error between the linearly transitioned filter state, and the orbit propagated states.
		Vector10d deltaState			= inertialState
										- subState.x.head(10);

		//We should ensure that the state transition is smooth, without bulk adjustments or 'setting' the state
		//add per-time adjusting state transitions to implement an addition of the shortfall delta calculated above
		Vector10d deltaStatePerSec	= deltaState / tgap;

		KFKey kfKey;
		kfKey.str	= inertial.str;

		for (int i = 0; i < 6; i++)
		{
			if (i >= 0 && i < 3)	{	kfKey.type	= KF::REC_POS;			kfKey.num	= i - 0;	}
			if (i >= 3 && i < 6)	{	kfKey.type	= KF::REC_POS_RATE;		kfKey.num	= i - 3;	}
			if (i >= 6 && i < 10)	{	kfKey.type	= KF::ORIENTATION;				kfKey.num	= i - 6;	}

			kfState.setKFTransRate(kfKey, KFState::oneKey, deltaStatePerSec(i));
		}
	}
}

/** Use models to predict orbital motion and prepare state transition equations to implement those predictions in the filter
 */
void predictInertials(
	Trace&			trace,
	const KFState&	kfState,
	GTime			time)
{
	double tgap = (time - kfState.time).to_double();

	if (tgap == 0)
	{
		return;
	}

	Inertials inertials = prepareInertials(trace, kfState);

	if (inertials.empty())
	{
		return;
	}

	BOOST_LOG_TRIVIAL(info) << " ------- PROPAGATING INERTIALS        --------" << "\n";

	InertialIntegrator integrator;
	integrator.timeInit				= kfState.time;
// 	integrator.propagationOptions	= acsConfig.inertialPropagation;

	double timestep = 0.1;
	integrateInertials(integrator, inertials, tgap, timestep);

	applyInertials(trace, inertials, kfState, time, tgap);

	for (auto& [id, timeMap] : UbxDecoder::acclDataMaps)
	for (auto it = timeMap.begin(); it != timeMap.end(); )
	{
		auto& [entryTime, thing] = *it;

		if (entryTime < time - 1.0)
		{
			it = timeMap.erase(it);
		}
		else
		{
			++it;
		}
	}
};
