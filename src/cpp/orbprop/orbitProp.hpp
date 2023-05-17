
#pragma once

#include <boost/numeric/odeint.hpp>

#include <fstream>
#include <vector>
#include <map>

using std::vector;
using std::map;

#include "eigenIncluder.hpp"
#include "staticField.hpp"
#include "oceanTide.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"
#include "erp.hpp"

using namespace boost::numeric::odeint;

struct EMP
{
	int			deg		= 0;
	int			axisId	= 0;
	E_TrigType	type	= E_TrigType::CONSTANT;
	double		value	= 0;
};

struct OrbitState
{
	SatSys	Sat;
	string	str;
	double	satMass		= 0;
	double	satPower	= 0;
	
	KFState	subState;
	
	vector<EMP> empInput;
	
	int empnum	= 0;
	Vector3d	pos;
	Vector3d	vel;
	MatrixXd	posVelSTM;

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
	OrbitPropagation  			propagationOptions;

	//Common parameter for all integrators.
	Matrix3d eci2ecf;
	Matrix3d deci2ecf;
	
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
		const	double	mjdinsec);
	
	void computeAcceleration(
		const	OrbitState&	orbInit,
				Vector3d&	acc,
				Matrix3d&	dAdPos,
				Matrix3d&	dAdVel,
				MatrixXd&	dAdParam);
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

void addKFSatEMPStates( 
			KalmanModel&	model, 
	const	KFState&		kfState,
			KF				kfType,
			string			id);

void outputOrbitConfig(
		KFState&	kfState);

