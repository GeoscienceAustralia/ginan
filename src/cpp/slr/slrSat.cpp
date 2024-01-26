
#include "observations.hpp"
#include "ephPrecise.hpp"
#include "ephemeris.hpp"
#include "testUtils.hpp"
#include "acsConfig.hpp"
#include "common.hpp"
#include "slr.hpp"


/** Sat position for SLR satellites (i.e. no clocks)
 */
void satPossSlr(
	Trace&				trace,				///< Trace to output to
	ObsList&			slrObsList,			///< List of observations to complete with satellite positions
	Navigation&			nav,				///< Navigation data
	vector<E_Source>	ephTypes,			///< Source of ephemeris data
	E_OffsetType		offsetType,			///< Point of satellite to output position of
	E_Relativity		applyRelativity,	///< Option to apply relativistic correction to clock
	const KFState*		kfState_ptr)		///< Optional pointer to a kalman filter to take values from
{
	for (auto& obs : only<LObs>(slrObsList))
	{
		if (obs.exclude)
			continue;

		satpos(trace, obs.time, obs.time, obs, ephTypes, offsetType, nav, kfState_ptr);
		
		if (obs.ephPosValid == false)
			BOOST_LOG_TRIVIAL(warning) << "Warning: Invalid position for " << obs.Sat.id() << " in " << __FUNCTION__;
	}
}

