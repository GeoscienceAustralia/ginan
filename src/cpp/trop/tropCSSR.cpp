#include "tropModels.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"

#define ERR_SAAS	0.3			///< saastamoinen model error std (m)

/** SSR Troposphere model
 * compute tropospheric delay using compact SSR contents
 * Neill Mapping functions ia used for mapping
* return : tropospheric delay (m)
*/
double tropCSSR(
	Trace&		trace,
	GTime		time,
	VectorPos&	pos,
	double		el,
	double&		dryZTD,
	double&		dryMap,
	double&		wetZTD,
	double&		wetMap,
	double&		var)
{
	tropSAAS(trace, time, pos, el, dryZTD, dryMap, wetZTD, wetMap, var);

	if (var < 0)
		return 0;

	var = -1;
	
	int regInd = checkSSRRegion(pos);
	
	if (regInd <= 0)
		return 0;

	auto& navAtm = nav.ssrAtm.atmosRegionsMap[regInd];

	if (navAtm.tropData.empty())
		return 0;

	auto& [tAtm, trpData] = *navAtm.tropData.begin();

	if (fabs((tAtm - time).to_double()) > acsConfig.ssrInOpts.local_trop_valid_time)
		return 0;

	if (var < 0)
		return 0;

	var = SQR(trpData.sigma);

	if (trpData.polyDry.size() > 0)
	{
		dryZTD = 0;
		double dLat = pos.latDeg() - navAtm.gridLatDeg[0];
		double dLon = pos.lonDeg() - navAtm.gridLonDeg[0];

		tracepdeex(2, trace,"Trop poly:  %.4f %.4f\n", dLat, dLon);

		for (auto& [ind, val] : trpData.polyDry)
		{
			switch (ind)
			{
				case 0: dryZTD += val; 					break;
				case 1: dryZTD += val * dLat; 			break;
				case 2: dryZTD += val * dLon; 			break;
				case 3: dryZTD += val * dLat * dLon;	break;		//todo aaron magic numbers
			}
			tracepdeex(2, trace,", %.4f\n", val);
		}
	}

	map<int, double> numerators;
	int		gridIndex	= 0;
	double	denominator	= 0;

	for (auto& [regID, regData] : nav.ssrAtm.atmosRegionsMap)
	{
		if (regData.intLatDeg <= 0)
			continue;

		if (regData.intLonDeg <= 0)
			continue;

		if (regData.tropData.empty())
			continue;

		auto& [tTrop, tropData] = *regData.tropData.begin();

		if (fabs((tTrop - time).to_double()) > acsConfig.ssrInOpts.local_trop_valid_time)
			continue;

		double midLonDeg = (regData.minLonDeg + regData.maxLonDeg) / 2;
		double recLonDeg = pos.lonDeg();

		if		((recLonDeg - midLonDeg) > +180)		recLonDeg -= 360;
		else if	((recLonDeg - midLonDeg) < -180)		recLonDeg += 360;

		for (auto& [rind, grdval] : tropData.gridWet)
		{
			if (regData.gridLatDeg.find(rind) == regData.gridLatDeg.end())
				continue;

			double dLat = fabs(pos.latDeg() - regData.gridLatDeg[rind]) / regData.intLatDeg;
			if (dLat > 1)
				continue;

			double dLon = fabs(recLonDeg - regData.gridLonDeg[rind]) / regData.intLonDeg;
			if (dLon > 1)
				continue;

			double coef = (1 - dLat) * (1 - dLon);
			denominator				+= coef;
			numerators[gridIndex]	+= coef * grdval;
			gridIndex++;
		}
	}

	if (gridIndex == 0)
	{
		var = -1;
		return 0;
	}

	wetZTD = 0;

	for (auto& [ind, val] : numerators)
		wetZTD += val;

	wetZTD /= denominator;

	double delay	= dryMap * dryZTD
					+ wetMap * wetZTD;

	return delay;
}
