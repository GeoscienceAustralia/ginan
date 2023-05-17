
#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "mongoRead.hpp"
#include "algebra.hpp"
#include "trace.hpp"
#include "gTime.hpp"

bool satClkRemote(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos)
{
	return false;
// 	bool found = true;
// 	
// 	GTime t0;
//  	t0.bigTime = (long int) (time.bigTime + 0.5);	// time tags in mongo will be rounded up to whole sec
// 	
// 	double 
// 	Vector6d inertialState = mongoReadOrbit(t0, satPos.Sat);
// 
// 	if (inertialState.isZero())
// 	{
// 		return false;
// 	}
// 	
// 	
// 	auto& rSat0 = satPos.rSatEci0;
// 	auto& vSat0 = satPos.vSatEci0;
// 	
// 	rSat0 = inertialState.head(3);
// 	vSat0 = inertialState.tail(3);
// 	
// 	double dt = (time - t0).to_double();
// 	
// 	satPos.rSatEciDt = propagateEllipse(trace, t0, dt, rSat0, vSat0, satPos.rSat);
	
	
	return true;
}

bool satPosRemote(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos)
{
	bool found = true;
	
	GTime t0;
 	t0.bigTime = (long int) (time.bigTime + 0.5);	// time tags in mongo will be rounded up to whole sec
	
	auto inertialStatesMap = mongoReadOrbits(t0, satPos.Sat);

	if (inertialStatesMap.empty())
	{
		return false;
	}
	
	auto& inertialState = inertialStatesMap[satPos.Sat][t0];
	
	auto& rSat0 = satPos.rSatEci0;
	auto& vSat0 = satPos.vSatEci0;
	
	rSat0 = inertialState.head(3);
	vSat0 = inertialState.tail(3);
	
	double dt = (time - t0).to_double();
	
	satPos.rSatEciDt = propagateEllipse(trace, t0, dt, rSat0, vSat0, satPos.rSat);
	
	return true;
}
