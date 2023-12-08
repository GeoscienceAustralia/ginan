


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
	VectorEci	rSat0;
	VectorEci	vSat0;
	GTime		t0;
	
	auto& satNav = nav.satNavMap[satPos.Sat];
	
	if	( satNav.satPos0.rSatEci0.isZero() == false
		&&satNav.satPos0.vSatEci0.isZero() == false
		&&satNav.satPos0.posTime != GTime::noTime())
	{
		rSat0	= satNav.satPos0.rSatEci0;
		vSat0	= satNav.satPos0.vSatEci0;
		t0		= satNav.satPos0.posTime;
	}
// 	else
	{
		if (kfState_ptr == nullptr)
		{
			return false;
		}
		
		auto& kfState = *kfState_ptr;
		
		bool found = true;
	
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
	
		t0 = kfState.time;
	}
		
		
	double dt = (time - t0).to_double();
	
	if	( rSat0.isZero() == false
		&&vSat0.isZero() == false)
	{
		satPos.rSatEciDt = propagateEllipse(trace, t0, dt, rSat0, vSat0, satPos.rSatCom, &satPos.satVel);
	}
	
	return true;
}
