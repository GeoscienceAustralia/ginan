
#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "navigation.hpp"
#include "mongoRead.hpp"
#include "algebra.hpp"
#include "orbits.hpp"
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
	
	auto& rSat0 = satPos.rSatEci0;
	auto& vSat0 = satPos.vSatEci0;
	
	GTime t0;
 	t0.bigTime = (long int) (time.bigTime + 0.5);	// time tags in mongo will be rounded up to whole sec
	
	auto& satPos0 = satPos.satNav_ptr->satPos0;
	
	if	(  satPos0.posTime		== t0
		&& satPos0.posSource	== +E_Source::REMOTE)
	{
		//use the cached version
		rSat0 = satPos0.rSatEci0;
		vSat0 = satPos0.vSatEci0;
	}
	else
	{
		auto inertialStatesMap = mongoReadOrbits(t0, satPos.Sat, true, &satPos.posVar);

		if (inertialStatesMap.empty())
		{
			return false;
		}
		
		auto& inertialState = inertialStatesMap[satPos.Sat][t0];
		
		rSat0 = inertialState.head(3);
		vSat0 = inertialState.tail(3);
	}
	
	double dt = (time - t0).to_double();
	
	satPos.rSatEciDt = propagateEllipse(trace, t0, dt, rSat0, vSat0, satPos.rSat);
	
	return true;
}
