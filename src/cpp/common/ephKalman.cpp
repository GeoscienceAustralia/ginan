

// #pragma GCC optimize ("O0")

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

	//get clocks from the state
	KFKey kfKey;
	kfKey.Sat	= satPos.Sat;

	bool anyFound = false;
	while (1)
	{
		double thisClk = 0;
		double thisVel = 0;

		kfKey.type	= KF::SAT_CLOCK;

		bool found = true;
		found &= kfState.getKFValue(kfKey, thisClk);

		if (found == false)
		{
			break;
		}

		anyFound = true;

		kfKey.type	= KF::SAT_CLOCK_RATE;

		found &= kfState.getKFValue(kfKey, thisVel);

		kfKey.num++;

		clk += thisClk;
		vel += thisVel;
	}

	if (anyFound == false)
	{
		return false;
	}

	double dt = (time - kfState.time).to_double();

	clk /= CLIGHT;
	vel /= CLIGHT;

	satPos.satClk		= clk
						+ vel * dt;

	satPos.satClkVel	= vel;

	return anyFound;
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
	else
	{
		if (kfState_ptr == nullptr)
		{
			return false;
		}

		auto& kfState = *kfState_ptr;

		//get orbit things from the state
		for (int i = 0; i < 3; i++)
		{
			KFKey kfKey;
			kfKey.type	= KF::ORBIT;
			kfKey.Sat	= satPos.Sat;

			kfKey.num	= i;
			double dummy;

			bool found	= (kfKey.num	= i,		kfState.getKFValue(kfKey, rSat0(i), &dummy, &dummy, false))
						&&(kfKey.num	= i + 3,	kfState.getKFValue(kfKey, vSat0(i), &dummy, &dummy, false));

			if (found == false)
			{
				return false;
			}
		}

		t0 = kfState.time;
	}

	double dt = (time - t0).to_double();

	// trace << "\n" << time << " " << satPos.Sat.id() << " dt: " << dt;

	if	( rSat0.isZero() == false
		&&vSat0.isZero() == false)
	{
		auto& satOpts = acsConfig.getSatOpts(satPos.Sat);

		if (dt <= satOpts.ellipse_propagation_time_tolerance)
		{
			satPos.rSatEciDt = propagateEllipse	(trace, t0, dt, rSat0, vSat0, satPos, true);
		}
		else
		{
			satPos.rSatEciDt = propagateFull	(trace, t0, dt, rSat0, vSat0, satPos);
		}
	}

	return true;
}
