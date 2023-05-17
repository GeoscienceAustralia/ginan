
// #pragma GCC optimize ("O0")

#include "ephPrecise.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "slr.hpp"


/** Sat position for SLR satellites (i.e. no clocks)
 */
void satPossSlr(
	Trace&				trace,				///< Trace to output to
	GTime				teph,				///< time to select ephemeris (gpst)
	ObsList&			slrObsList,			///< List of observations to complete with satellite positions
	Navigation&			nav,				///< Navigation data
	vector<E_Source>	ephTypes,			///< Source of ephemeris data
	E_OffsetType		offsetType,			///< Point of satellite to output position of
	E_Relativity		applyRelativity,	///< Option to apply relativistic correction to clock
	const KFState*		kfState_ptr)		///< Optional pointer to a kalman filter to take values from
{
	//tracepde(3, trace, "%s: teph=%s n=%d ephType=%d\n", __FUNCTION__, teph.to_string(3).c_str(), slrObsList.size(), ephType);

	for (auto& obs : only<LObs>(slrObsList))
	{
		if (obs.exclude)
		{
			continue;
		}

		satpos(trace, obs.time, obs.time, obs, ephTypes, offsetType, nav, kfState_ptr);
		
		if (obs.ephPosValid == false)
		{
			std::cout << "warning: obs.ephPosValid == false" << std::endl;
		}
		
		//tracepde(4, trace, "%s sat=%s rs=%13.3f %13.3f %13.3f var=%7.3f svh=%02X\n",
		//		obs.timeBn.to_string(6).c_str(),
		//		obs.Sat.id().c_str(),
		//		obs.rSat[0],
		//		obs.rSat[1],
		//		obs.rSat[2],
		//		obs.ephVar,
		//		obs.svh);
	}
}

