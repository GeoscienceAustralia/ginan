


#include "eigenIncluder.hpp"
#include "observations.hpp"
#include "navigation.hpp"
#include "algebra.hpp"
#include "trace.hpp"
#include "gTime.hpp"


bool satClkKalman(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos,
	const KFState*	kfState_ptr)
{
	if (kfState_ptr == nullptr)
	{
		return false;
	}
	
	auto& kfState = *kfState_ptr;
	
	double clk = 0;
	double vel = 0;
	
	//get orbit things from the state
	KFKey kfKey;
	kfKey.type	= KF::SAT_CLOCK;
	kfKey.Sat	= satPos.Sat;
	
	bool found = true;
	found &= kfState.getKFValue(kfKey, clk);
	
	if (found == false)
	{
		return false;
	}
	
	kfKey.type	= KF::SAT_CLOCK_RATE;
	
	found &= kfState.getKFValue(kfKey, vel);
	
	double dt = (time - kfState.time).to_double();
	
	clk /= CLIGHT;
	vel /= CLIGHT;
	
	satPos.satClk		= clk
						+ vel * dt;
					
	satPos.satClkVel	= vel;
				
	return true;
}

bool satPosKalman(
	Trace&			trace,
	GTime			time,
	SatPos&			satPos,
	const KFState*	kfState_ptr)
{
	if (kfState_ptr == nullptr)
	{
		return false;
	}
	
	auto& kfState = *kfState_ptr;
	
	bool found = true;
	
	auto& rSat0 = satPos.rSatEci0;
	auto& vSat0 = satPos.vSatEci0;
	
	//get orbit things from the state
	for (int i = 0; i < 3; i++)
	{
		KFKey kfKey;
		kfKey.type	= KF::ORBIT;
		kfKey.Sat	= satPos.Sat;
		
		kfKey.num	= i;
		found &= kfState.getKFValue(kfKey, rSat0(i));
		
		kfKey.num	= i + 3;
		found &= kfState.getKFValue(kfKey, vSat0(i));
		
		if (found == false)
		{
			return false;
		}
	}
	
	double dt = (time - kfState.time).to_double();
	
	if	( rSat0.isZero() == false
		&&vSat0.isZero() == false)
	{
		satPos.rSatEciDt = propagateEllipse(trace, kfState.time, dt, rSat0, vSat0, satPos.rSat);
	}
	
	return true;
}
