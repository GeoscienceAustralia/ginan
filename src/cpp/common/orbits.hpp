
#ifndef __ORBIT_HPP__
#define __ORBIT_HPP__


#include <unordered_map>
#include <list>

using std::unordered_map;
using std::list;


#include "eigenIncluder.hpp"
#include "satSys.hpp"
#include "station.hpp"


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
	GTime	time;					///< Time of the orbit epoch
	double	ti[2];					///< the ith epoch [MJD][sod]

	Vector<double, 6>	xcrf;		///< icrf pos(m) and vel(m/s)
	Vector<double, 6>	xtrf;		///< itrf pos(m) and vel(m/s)

	MatrixXd			partials;	///< partial wrt initial state
};

struct SatOrbit
{
	InitialOrbit								initialOrbit;
	map<GTime, OrbitInfo, std::greater<GTime>>	orbitInfoMap;
	int											numUnknowns;			///< number of unknowns
	vector<string>								parameterNames;
	string										srpModel[2];
	double										mass;
};

void readegm(
	string      file,
	EGMCoef&	egm);


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


void keplers2inertial(
	VectorXd&	keplers0, 
	Vector3d&	pos,
	double&		dM);


void inertial2Keplers(
			Trace&		trace,
	const	Vector3d&	r,
	const	Vector3d&	v,
			VectorXd&	keplers);

void getKeplerPartials(
	VectorXd&	keplers0,
	MatrixXd&	partials);
	
void getKeplerInversePartials(
	Vector3d&	pos,
	Vector3d&	vel,
	MatrixXd&	partials);
 
#endif

