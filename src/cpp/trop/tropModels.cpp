
// #pragma GCC optimize ("O0")

#include <math.h>

#include "observations.hpp"
#include "coordinates.hpp"
#include "tropModels.hpp"
#include "acsConfig.hpp"
#include "satStat.hpp"
#include "algebra.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "enums.h"

struct TropMapBasis
{
	int				regionID;					/* Atmospheric region ID */
	E_BasisType		type;						/* parameter type: 0 polnomial, 1 gridpoint */
	int				index;						/* parameter index */
};

vector<TropMapBasis>  tropBasisVec;

void defineLocalTropBasis()
{
	tropBasisVec.clear();

	for (auto& [iatm, atmReg] : nav.ssrAtm.atmosRegionsMap)
	{
		for (int i = 0; i < atmReg.tropPolySize; i++)
		{
			TropMapBasis basis;
			basis.regionID	= iatm;
			basis.type		= E_BasisType::POLYNOMIAL;
			basis.index		= i;

			tropBasisVec.push_back(basis);
		}

		if	(  atmReg.gridType >= 0
			&& atmReg.tropGrid)
		for (auto& [igrid, latgrid] : atmReg.gridLatDeg)
		{
			TropMapBasis basis;
			basis.regionID	= iatm;
			basis.type		= E_BasisType::GRIDPOINT;
			basis.index		= igrid;

			tropBasisVec.push_back(basis);
		}
	}

	acsConfig.ssrOpts.nbasis = tropBasisVec.size();
}

double tropModelCoef(
	int			ind,
	VectorPos&	pos)
{
	if (ind >= tropBasisVec.size())							return 0;

	auto& basis = tropBasisVec[ind];

	auto& atmReg = nav.ssrAtm.atmosRegionsMap[basis.regionID];

	double recLatDeg = pos.latDeg();
	double recLonDeg = pos.lonDeg();

	if (recLatDeg > atmReg.maxLatDeg)						return 0;
	if (recLatDeg < atmReg.minLatDeg)						return 0;

	double midLonDeg = (atmReg.minLonDeg + atmReg.maxLonDeg) / 2;
	if		((recLonDeg - midLonDeg) >  180)	recLonDeg -= 360;
	else if	((recLonDeg - midLonDeg) < -180)	recLonDeg += 360;

	if (recLonDeg > atmReg.maxLonDeg)						return 0;
	if (recLonDeg < atmReg.minLonDeg)						return 0;

	double latdiff = recLatDeg - atmReg.gridLatDeg[0];
	double londiff = recLonDeg - atmReg.gridLonDeg[0];

	switch (basis.type)
	{
		case E_BasisType::POLYNOMIAL:
		{
			switch (basis.index)
			{
				case E_PolyType::CONSTANT: 	return 1;
				case E_PolyType::LAT: 		return latdiff;
				case E_PolyType::LON: 		return londiff;
				case E_PolyType::LAT_LON: 	return latdiff * londiff;
				case E_PolyType::LAT_SQRD: 	return latdiff * latdiff;
				case E_PolyType::LON_SQRD: 	return londiff * londiff;
				default:	return 0;
			}
		}
		case E_BasisType::GRIDPOINT:
		{
			double dlatDeg = fabs(recLatDeg - atmReg.gridLatDeg[basis.index]);
			double dlonDeg = fabs(recLonDeg - atmReg.gridLonDeg[basis.index]);

			if (dlatDeg > atmReg.intLatDeg || atmReg.intLatDeg == 0)		return 0;
			if (dlonDeg > atmReg.intLonDeg || atmReg.intLonDeg == 0)		return 0;

			return (1 - dlatDeg / atmReg.intLatDeg) * (1 - dlonDeg / atmReg.intLonDeg);		//todo aaron use bilinear interpolation function?
		}
		default:
		{
			return 0;
		}
	}
}


double mapHerring(double el, double a, double b, double c)
{
	double sinel = sin(el);
	return	(1 + a /
				(1 + b /
					(1 + c))) / (sinel + (a /
										(sinel + b /
												(sinel + c))));
}

/** Returns gradient mapping function m_az(el)
* Valid for 0 < el < 0.9999 * PI/2; returns 0 otherwise
* Ref: https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/97JB01739
*/
double gradMapFn(
	double	el)		///< Elevation (rad)
{
	if	( el < 0
		||el > 0.9999 * PI/2)
	{
		return 0;
	}

	double c = 0.0031;
	return 1 / (sin(el) * tan(el) + c);
}


double tropModel(
	Trace&					trace,
	vector<E_TropModel> 	models,
	GTime					time,
	VectorPos&				pos,
	AzEl&					azel,
	TropStates&				tropStates,
	TropMapping&			dTropDx,
	double&					var)
{
	double dryZTD;
	double wetZTD;
	var = -1;

	for (auto& model : models)
	{
		switch (model)
		{
			case E_TropModel::STANDARD:	tropSAAS(trace, time, pos, azel.el, dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
			case E_TropModel::SBAS:		tropSBAS(trace, time, pos, azel.el, dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
			case E_TropModel::GPT2:		tropGPT2(trace, time, pos, azel.el, dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
			case E_TropModel::VMF3:		tropVMF3(trace, time, pos, azel.el, dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
			case E_TropModel::CSSR:		tropCSSR(trace, time, pos, azel.el, dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
			default: return 0;
		}

		if (var < 0)
			continue;

		tracepdeex(2, trace,"\nTroposphere Model %s %s  %d %d %d", time.to_string().c_str(), model._to_string(), pos.latDeg(), pos.lonDeg(), pos.hgt());

		break;
	}

	//todo aaron var might still be < 0 if everything in models failed

	if (tropStates.zenith == 0)	//initialization
	{
		tropStates.zenith = dryZTD + wetZTD;
	}
	else
	{
		wetZTD = tropStates.zenith - dryZTD;
		var = 0;
	}

	tracepdeex(4,trace,"  dry: %.4f x %.4f   wet: %.4f x %.4f", dTropDx.dryMap, dryZTD, dTropDx.wetMap, wetZTD);

	double gradMap = gradMapFn(azel.el);
	dTropDx.northMap	= gradMap * cos(azel.az);
	dTropDx.eastMap		= gradMap * sin(azel.az);

	return 		dTropDx.dryMap		* dryZTD
			+	dTropDx.wetMap		* wetZTD
			+	dTropDx.northMap	* tropStates.grads[0]
			+	dTropDx.eastMap		* tropStates.grads[1];
}

double tropDryZTD(
	Trace&					trace,
	vector<E_TropModel> 	models,
	GTime					time,
	VectorPos&				pos)
{
	double dryZTD;
	double dryMap;
	double wetZTD;
	double wetMap;
	double var = -1;
	for (auto& model : models)
	{
		switch (model)
		{
			case E_TropModel::STANDARD:	tropSAAS(trace, time, pos, PI/2, dryZTD, dryMap, wetZTD, wetMap, var);	break;
			case E_TropModel::SBAS:		tropSBAS(trace, time, pos, PI/2, dryZTD, dryMap, wetZTD, wetMap, var);	break;
			case E_TropModel::GPT2:		tropGPT2(trace, time, pos, PI/2, dryZTD, dryMap, wetZTD, wetMap, var);	break;
			case E_TropModel::VMF3:		tropVMF3(trace, time, pos, PI/2, dryZTD, dryMap, wetZTD, wetMap, var);	break;
			default: return 0;
		}

		if (var >= 0)
			break;
	}

	//todo aaron var might still be < 0 if everything in models failed
	return dryZTD;
}

double heightAdjustWet(
	double hgt)
{
	double hgtKm	= hgt / 1E3;
	double temp		= 288.15 - 6.5 * hgtKm;
	double eScale	=  exp(0.9636 * hgtKm / (38.4154 - hgtKm));

	return  eScale * (1255 / temp + 0.05) / 4.40537;
}

double heightAdjustDry(
	double hgt,
	double lat)
{
	double hgtKm	= hgt / 1E3;
	double latScale	= 1 - 0.00266 * cos(2 * lat);

	return pow((1 - 0.0226 * hgtKm), 5.225) * latScale / (latScale - 0.00028 * hgtKm);
}
