
#pragma once


#include "eigenIncluder.hpp"
#include "station.hpp"
#include "satSys.hpp"


///< initial orbit state info
struct InitialOrbit
{
	SatSys		Sat;
	double		t0[2];				///< initial epoch [MJD][sod]
	VectorXd	initialConds;		///< initial icrf pos(m), vel(m/s), srp parameters
};

struct OrbitInfo
{
	// orbit info for one sat on the ith epoch
	GTime		time;		///< Time of the orbit epoch
	double		ti[2];		///< the ith epoch [MJD][sod]

	VectorEci	posEci;
	VectorEci	velEci;
	VectorEcef	posEcef;
	VectorEcef	velEcef;

	MatrixXd	partials;	///< partial wrt initial state
};

struct SatOrbit
{
	InitialOrbit								initialOrbit;
	map<GTime, OrbitInfo, std::greater<GTime>>	orbitInfoMap;
	int											numUnknowns;
	vector<string>								parameterNames;
	string										srpModel[2];
	double										mass;
};



int orbPartials(
	Trace&		trace,
	GTime		time,
	SatSys		Sat,
	MatrixXd&	interpPartials);

int readorbit(
	string		file);

struct KFState;

void outputOrbit(
			KFState&	kfState);

VectorEci keplers2Inertial(
			Trace&		trace,
	const	Vector6d&	keplers0);

bool inertial2Keplers(
			Trace&		trace,
	const	VectorEci&	r,
	const	VectorEci&	v,
			Vector6d&	keplers);

VectorEci propagateEllipse(
			Trace&		trace,
			GTime		time,
			double		dt, 
			VectorEci&	rSat,
			VectorEci&	vSat, 
			VectorEcef&	ecef);
