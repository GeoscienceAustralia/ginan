
#ifndef READORBIT_H
#define READORBIT_H


#include <unordered_map>
#include <list>

using std::unordered_map;
using std::list;


#include "eigenIncluder.hpp"


#include "navigation.hpp"
#include "satSys.hpp"


struct InitialOrbit
{
	///< initial orbit state info
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
	InitialOrbit			initialOrbit;
	map<GTime, OrbitInfo>	orbitInfoList;
	int						numUnknowns;			///< number of unknowns
	vector<string>			parameterNames;
	string					srpModel[2];
	double					mass;
};

struct orbpod_t
{
	/* orbit info from POD */
	double							startEpoch[2];			///< start epoch [mjd][sod]
	double							endEpoch[2];			///< end epoch [mjd][sod]
	int								numSats;				///< number of satellite
	int								numEpochs;				///< number of epochs
	int								nint;					///< interval (s)
	map<SatSys, SatOrbit>			satOrbitMap;			//key is satSys uid
	list<string>					infoList;
};

int readorbit(
	string		file,
	orbpod_t&	orbpod);

struct KFState;

void outputOrbit(
	KFState&	kfState);
#endif

