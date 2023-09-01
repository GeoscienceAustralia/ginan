
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
		for (auto& [igrid, latgrid] : atmReg.gridLat)
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
	double recLat = pos[0];
	double recLon = pos[1];
	
	if (recLat > atmReg.maxLat)								return 0;
	if (recLat < atmReg.minLat)								return 0;
	
	double midLon = (atmReg.minLon + atmReg.maxLon)/2;
	if		((recLon - midLon) >  PI)	recLon -= 2*PI;
	else if	((recLon - midLon) < -PI)	recLon += 2*PI;
		
	if (recLon > atmReg.maxLon)								return 0;
	if (recLon < atmReg.minLon)								return 0;
	
	double latdiff = recLat - atmReg.gridLat[0];
	double londiff = recLon - atmReg.gridLon[0];

	switch (basis.type)
	{
		case E_BasisType::POLYNOMIAL:
		{
			switch (basis.index)	//todo aaron magic numbers
			{
				case 0: 	return 1;
				case 1: 	return latdiff;
				case 2: 	return londiff;
				case 3: 	return latdiff * londiff;
				case 4: 	return latdiff * latdiff;
				case 5: 	return londiff * londiff;
				default:	return 0;
			}
		}
		case E_BasisType::GRIDPOINT:
		{
			double dlat = fabs(recLat - atmReg.gridLat[basis.index]);
			double dlon = fabs(recLon - atmReg.gridLon[basis.index]);
			
			if (dlat > atmReg.intLat || atmReg.intLat == 0)		return 0;
			if (dlon > atmReg.intLon || atmReg.intLon == 0)		return 0;
			
			return (1 - dlat / atmReg.intLat) * (1 - dlon / atmReg.intLon);		//todo aaorn use bilinear interpolation function?
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
	Trace&			trace,
	E_TropModel 	model,
	GTime			time,
	VectorPos&		pos,
	double*			azel,
	double*			tropStates,
	TropMapping&	dTropDx,
	double&			var)
{
	double dryZTD;
	double wetZTD;
	var = SQR(0.1);
	
	tracepdeex(4, trace,"\n Troposphere Model %s %s  %d %d %d", time.to_string(0), model._to_string(), pos[0]*R2D, pos[1]*R2D, pos[2]);
	switch (model)
	{
		case E_TropModel::STANDARD:	tropSAAS(time, pos, azel[1], dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
		case E_TropModel::SBAS:		tropSBAS(time, pos, azel[1], dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap, var);	break;
		case E_TropModel::GPT2:		tropGPT2(time, pos, azel[1], dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap);		break;
		case E_TropModel::VMF3:		tropVMF3(time, pos, azel[1], dryZTD, dTropDx.dryMap, wetZTD, dTropDx.wetMap);		break;
		default: var=SQR(ERR_TROP);	return 0;
	}
	
	if (tropStates[0] == 0)	//initialization
	{
		tropStates[0] = dryZTD + wetZTD;
	}
	else
	{
		wetZTD = tropStates[0] - dryZTD;
		var = 0;
	}
	
	tracepdeex(4,trace,"  dry: %.4f x %.4f   wet: %.4f x %.4f", dTropDx.dryMap, dryZTD, dTropDx.wetMap, wetZTD);
	
	double gradMap = gradMapFn(azel[1]);
	dTropDx.northMap	= gradMap * cos(azel[0]);
	dTropDx.eastMap		= gradMap * sin(azel[0]);
	
	return 		dTropDx.dryMap		* dryZTD 
			+	dTropDx.wetMap		* wetZTD 
			+	dTropDx.northMap	* tropStates[1] 
			+	dTropDx.eastMap		* tropStates[2];
}

double tropDryZTD(
	E_TropModel 	model,
	GTime			time,
	VectorPos&		pos)
{
	double dryZTD;
	double dryMap;
	double wetZTD;
	double wetMap;
	double var;
	switch (model)
	{
		case E_TropModel::STANDARD:	tropSAAS( time,pos,PI/2, dryZTD, dryMap, wetZTD, wetMap, var);	break;
		case E_TropModel::SBAS:		tropSBAS( time,pos,PI/2, dryZTD, dryMap, wetZTD, wetMap, var);	break;
		case E_TropModel::GPT2:		tropGPT2( time,pos,PI/2, dryZTD, dryMap, wetZTD, wetMap);		break;
		case E_TropModel::VMF3:		tropVMF3( time,pos,PI/2, dryZTD, dryMap, wetZTD, wetMap);		break;
		default: return 0;
	}
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
