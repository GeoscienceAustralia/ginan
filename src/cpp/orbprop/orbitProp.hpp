
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

using namespace boost::numeric::odeint;

struct EMP
{
	bool		scaleEclipse	= false;
	int			deg				= 0;
	E_EmpAxis	axisId			= E_EmpAxis::NONE;
	E_TrigType	type			= E_TrigType::COS;
	double		value			= 0;
};

struct OrbitState : OrbitOptions
{
	SatSys	Sat;
	string	str;

	bool	exclude		= false;

	shared_ptr<KFState>	subState_ptr;

	vector<EMP> empInput;

	mutable map<E_Component, double>	componentsMap;

	int numEmp		= 0;
	int numParam	= 0;
	Vector3d	pos;
	Vector3d	vel;
	MatrixXd	posVelSTM;

	AttStatus	attStatus;
	Vector3d	gyroBias	= Vector3d::Zero();
	Vector3d	acclBias	= Vector3d::Zero();
	Vector3d	gyroScale	= Vector3d::Ones();
	Vector3d	acclScale	= Vector3d::Ones();
	double		posVar = 0;

	OrbitState& operator+=(double rhs)
	{
		pos			= (pos		.array() + rhs).matrix();
		vel			= (vel		.array() + rhs).matrix();
		posVelSTM	= (posVelSTM.array() + rhs).matrix();
		return *this;
	}

	OrbitState& operator*=(double rhs)
	{
		pos			*= rhs;
		vel			*= rhs;
		posVelSTM	*= rhs;
		return *this;
	}

	OrbitState operator+(double rhs) const
	{
		OrbitState newState = *this;
		newState += rhs;
		return newState;
	}

	OrbitState operator+(const OrbitState& rhs) const
	{
		OrbitState newState = *this;
		newState.pos		+= rhs.pos;
		newState.vel		+= rhs.vel;
		newState.posVelSTM	+= rhs.posVelSTM;
		return newState;
	}

	OrbitState operator*(double rhs) const
	{
		OrbitState newState = *this;
		newState *= rhs;
		return newState;
	}
};

typedef vector<OrbitState> Orbits;

inline OrbitState operator*(
	const	double		lhs,
	const	OrbitState&	rhs)
{
	return rhs * lhs;
};

inline Orbits operator+(
	const Orbits& lhs,
	const Orbits& rhs)
{
	Orbits newState = lhs;
	for (int i = 0; i < lhs.size(); i++)
	{
		newState[i] = lhs[i] + rhs[i];
	}
	return newState;
}

inline Orbits operator*(
	const Orbits&	lhs,
	const double	rhs)
{
	Orbits newState = lhs;
	for (int i = 0; i < lhs.size(); i++)
	{
		newState[i] *= rhs;
	}
	return newState;
}

inline Orbits operator*(
	const double rhs,
	const Orbits& lhs)
{
	Orbits newState = lhs;
	for (int i = 0; i < lhs.size(); i++)
	{
		newState[i] *= rhs;
	}
	return newState;
}


struct OrbitIntegrator
{
	GTime						timeInit;

	Matrix3d		eci2ecf;
	Matrix3d		deci2ecf;

	map<E_ThirdBody, Vector3dInit> planetsPosMap;
	map<E_ThirdBody, Vector3dInit> planetsVelMap;

	MatrixXd Cnm;
	MatrixXd Snm;
	runge_kutta_fehlberg78<Orbits, double, Orbits, double, vector_space_algebra> odeIntegrator;

	void operator()(
		const	Orbits&	orbInit,
				Orbits&	orbUpdate,
		const	double	mjdSec);

	void computeCommon(
		const	GTime	time);

	void computeAcceleration(
		const	OrbitState&	orbInit,
				Vector3d&	acc,
				Matrix3d&	dAdPos,
				Matrix3d&	dAdVel,
				MatrixXd&	dAdParam,
		const	GTime		time);
};

KFState getOrbitFromState(
	Trace&			trace,
	string			id,
	const KFState&	kfState);

void predictOrbits(
	Trace&			trace,
	const KFState&	kfState,
	GTime           time);

Orbits prepareOrbits(
	Trace&			trace,
	const KFState&	kfState);

void integrateOrbits(
	OrbitIntegrator&	orbitPropagator,
	Orbits&				orbits,
	double				integrationPeriod,
	double 				dt);


void addEmpStates(
	const	EmpKalmans&		satOpts,
	const	KFState&		kfState,
	const	string&			id);

void addNilDesignStates(
	const	KalmanModel&	model,
	const	KFState&		kfState,
	const	KF&				kfType,
			int				num,
	const	string&			id);

void outputOrbitConfig(
		KFState&	kfState,
		string		suffix = "");

