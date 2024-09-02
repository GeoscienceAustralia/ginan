
#pragma once

#include <boost/numeric/odeint.hpp>

#include <fstream>
#include <vector>
#include <map>

using std::vector;
using std::map;

#include "eigenIncluder.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"
#include "erp.hpp"

using namespace boost::numeric::odeint;

struct InertialState
{
	SatSys	Sat;
	string	str;

	bool	exclude		= false;

	KFState	subState;

// 	vector<EMP> empInput;

	map<E_Component, double>	componentsMap;

	int numParams	= 0;
	Vector3d	pos			= Vector3d::Zero();
	Vector3d	vel			= Vector3d::Zero();
	Vector4d	quat		= Vector4d::Zero();
	MatrixXd	posVelQuatSTM;

	Vector3d	gyroBias	= Vector3d::Zero();
	Vector3d	acclBias	= Vector3d::Zero();
	Vector3d	gyroScale	= Vector3d::Ones();
	Vector3d	acclScale	= Vector3d::Ones();

	double		posVar = 0;

	InertialState& operator+=(double rhs)
	{
		pos				= (pos			.array() + rhs).matrix();
		vel				= (vel			.array() + rhs).matrix();
		quat			= (quat			.array() + rhs).matrix();
		posVelQuatSTM	= (posVelQuatSTM.array() + rhs).matrix();
		return *this;
	}

	InertialState& operator*=(double rhs)
	{
		pos				*= rhs;
		vel				*= rhs;
		quat			*= rhs;
		posVelQuatSTM	*= rhs;
		return *this;
	}

	InertialState operator+(double rhs) const
	{
		InertialState newState = *this;
		newState += rhs;
		return newState;
	}

	InertialState operator+(const InertialState& rhs) const
	{
		InertialState newState = *this;
		newState.pos			+= rhs.pos;
		newState.vel			+= rhs.vel;
		newState.quat			+= rhs.quat;
		newState.posVelQuatSTM	+= rhs.posVelQuatSTM;
		return newState;
	}

	InertialState operator*(double rhs) const
	{
		InertialState newState = *this;
		newState *= rhs;
		return newState;
	}
};

typedef vector<InertialState> Inertials;

inline InertialState operator*(
	const	double		lhs,
	const	InertialState&	rhs)
{
	return rhs * lhs;
};

inline Inertials operator+(
	const Inertials& lhs,
	const Inertials& rhs)
{
	Inertials newState = lhs;
	for (int i = 0; i < lhs.size(); i++)
	{
		newState[i] = lhs[i] + rhs[i];
	}
	return newState;
}

inline Inertials operator*(
	const Inertials&	lhs,
	const double	rhs)
{
	Inertials newState = lhs;
	for (int i = 0; i < lhs.size(); i++)
	{
		newState[i] *= rhs;
	}
	return newState;
}

inline Inertials operator*(
	const double		rhs,
	const Inertials&	lhs)
{
	Inertials newState = lhs;
	for (int i = 0; i < lhs.size(); i++)
	{
		newState[i] *= rhs;
	}
	return newState;
}


struct InertialIntegrator
{
	GTime						timeInit;
// 	InertialPropagation  		propagationOptions;

	runge_kutta_fehlberg78<Inertials, double, Inertials, double, vector_space_algebra> odeIntegrator;

	void operator()(
		const	Inertials&	inertialInit,
				Inertials&	inertialUpdate,
		const	double	mjdSec);

	void computeDeltaInertial(
		const	InertialState&	inertialInit,
				Vector3d&		acc,
				Vector4d&		dQuat,
				Matrix3d&		dAdPos,
				Matrix3d&		dAdVel,
				Matrix4d&		dAdQuat,
				MatrixXd&		dAdParam);
};

void predictInertials(
	Trace&			trace,
	const KFState&	kfState,
	GTime           time);
