
// #pragma GCC optimize ("O0")


#include "eigenIncluder.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "satSys.hpp"

void dummyOrbitPropagator(
	KFState& kfState)
{
	SatSys Sat = {};
	auto& satOpts = acsConfig.getSatOpts(Sat);
	
	if (satOpts.orbit.estimate)
	{
		for (int i = 0; i < 6; i++)
		{
			auto init = initialStateFromConfig(satOpts.orbit, i);
			
			KFKey key;
			key.type	= KF::ORBIT;
			key.num		= i;
			
			kfState.addKFState(key, init);
		}
	}
}
