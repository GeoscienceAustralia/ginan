#include "observations.hpp"
#include "coordinates.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "common.hpp"

#define IONO_OUT_THRESHOLD		120
#define DEFAULT_STEC_POLY_ACC	0.5

struct LocalBasis
{
	int			regionID;					///< Atmospheric region ID 
	SatSys		Sat;						///< Satellite System
	E_BasisType	type;						///< parameter type
	int			index;						///< parameter index 
};

vector<LocalBasis>  localBasisVec;

int defineLocalIonoBasis()
{
	localBasisVec.clear();
	
	for (auto& [iatm, atmReg]	: nav.ssrAtm.atmosRegionsMap)
	for (auto& [Sat, satNav]	: nav.satNavMap)
	{
		for (int i = 0; i < atmReg.ionoPolySize; i++)
		{
			LocalBasis basis;
			basis.regionID	= iatm;
			basis.Sat		= Sat;
			basis.type		= E_BasisType::POLYNOMIAL;
			basis.index		= i;
			
			localBasisVec.push_back(basis);
		}
		
		if	(  atmReg.gridType >= 0 
			&& atmReg.ionoGrid)
		for (auto& [igrid, latgrid] : atmReg.gridLat)	//todo aaron, can these both be configured at same time? is index correct for gps since they may be after polys?
		{
			LocalBasis basis;
			basis.regionID	= iatm;
			basis.Sat		= Sat;
			basis.type		= E_BasisType::GRIDPOINT;
			basis.index		= igrid;
			
			localBasisVec.push_back(basis);
		}
	}
	
	acsConfig.ionModelOpts.numBasis			= localBasisVec.size();	
	acsConfig.ionModelOpts.layer_heights.clear();
	acsConfig.ionModelOpts.layer_heights.push_back(0);							/* local ionosphere are mapped with respect to ground */
	acsConfig.ionModelOpts.estimate_sat_dcb	= false;
	
	return acsConfig.ionModelOpts.numBasis;
}

/** Checks if the Ionosphere Piercing Point falls in area of coverage. 
Return the region ID containing the IPP, 0 if out of coverage
*/
bool ippCheckLocal(
	GTime		time, 			///< time of observations (not used)
	VectorPos&	ionPP)			///< Ionospheric piercing point to be updated
{
	auto& RegMaps = nav.ssrAtm.atmosRegionsMap;
	
	for (auto& [iatm,atmReg] : RegMaps)
	{
		if (ionPP[0] < atmReg.minLat)			continue;
		if (ionPP[0] > atmReg.maxLat)			continue;
		
		double recLon = ionPP[1];
		double midLon = (atmReg.minLon + atmReg.maxLon) / 2;
		
		if      ((recLon - midLon) > PI)	recLon -= 2*PI;
		else if ((recLon - midLon) <-PI)	recLon += 2*PI;
	
		if (recLon > atmReg.maxLon)				continue;
		if (recLon < atmReg.minLon)				continue;

		return true;
	}
	
	return false;
}


/** calcuates the partials of observations with respect to basis functions
 */
double ionCoefLocal(
	Trace&		trace, 
	int 		ind, 	///< Basis function number
	IonoObs&	obs)	///< Metadata containing piercing points
{
	if (ind >= localBasisVec.size())						return 0;
	
	auto& basis = localBasisVec[ind];
	
	if (obs.ionoSat != basis.Sat)							return 0;
	
	auto& atmReg = nav.ssrAtm.atmosRegionsMap[basis.regionID];
	
	if (atmReg.maxLat <= atmReg.minLat)						return 0;
	if (atmReg.maxLon <= atmReg.minLon)						return 0;
	
	double recLat = obs.ippMap[0].lat;
	double recLon = obs.ippMap[0].lon;
	
	trace << "    localIono check #" << ind << ";  Sat: "<< basis.Sat.id() << ";  Sta_coor: " << recLat*R2D << "," << recLon*R2D;
	
	if (recLat > atmReg.maxLat)								return 0;
	if (recLat < atmReg.minLat)								return 0;
	
	double midLon = (atmReg.minLon + atmReg.maxLon) / 2;
	if		((recLon - midLon) >  PI)	recLon -= 2*PI;
	else if	((recLon - midLon) < -PI)	recLon += 2*PI;
	
	if (recLon > atmReg.maxLon)								return 0;
	if (recLon < atmReg.minLon)								return 0;
	
	double dLat = atmReg.maxLat - atmReg.minLat;
	double dLon = atmReg.maxLon - atmReg.minLon;
	
	double latdiff = (recLat - atmReg.gridLat[0]) / dLat;
	double londiff = (recLon - atmReg.gridLon[0]) / dLon;

	trace << "  -> " << latdiff << "," << londiff << std::endl;

	switch (basis.type)
	{
		case +E_BasisType::POLYNOMIAL:		//todo aaron magic numbers, x2
		{
			switch (basis.index)
			{
				case 0: 	return 1;
				case 1: 	return 2* latdiff;
				case 2: 	return 2* londiff;
				case 3: 	return 4* latdiff * londiff;
				case 4: 	return 3* latdiff * latdiff;
				case 5: 	return 3* londiff * londiff;
				default:	return 0;
			}
		}
		case +E_BasisType::GRIDPOINT:
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

void ionOutputLocal(
	Trace&		trace, 
	KFState&	kfState)
{
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONOSPHERIC)
			continue;
		
		auto& basis			= localBasisVec[key.num];
		auto& atmReg    	= nav.ssrAtm.atmosRegionsMap[basis.regionID];
		auto& stec_record	= atmReg.stecData[basis.Sat][kfState.time];
		
		switch (basis.type)
		{
			case E_BasisType::POLYNOMIAL:
			{
				double val;
				double var;
				kfState.getKFValue(key, val, &var);
				
				stec_record.poly[basis.index]	= val;
				stec_record.accr				= DEFAULT_STEC_POLY_ACC; 
			
				break;
			}
			case E_BasisType::GRIDPOINT:
			{
				double val;
				double var;
				kfState.getKFValue(key, val,&var);
				
				stec_record.grid[basis.index] = val;
				
				if (sqrt(var) > stec_record.accr)
					stec_record.accr = sqrt(var); 
				
				break;
			}
		}
	}
}

bool getCmpSSRIono(
	GTime		time,		///< GPS time
	SSRAtm&		ssrAtm,		///< SSR Atmospheric corrections 
	Vector3d&	rRec,		///< receiver position 
	double& 	iono,		///< ionoapheric delay (in TECu)
	double&		var,		///< ionoapheric delay (in TECu^2)
	SatSys	 	Sat)		///< Satellite
{
	GObs obs;
	obs.Sat = Sat;
	
	VectorPos pos = ecef2pos(rRec);
	obs.ippMap[0].lat = pos.lat();
	obs.ippMap[0].lon = pos.lon();
	obs.ionoSat		  = Sat;
	
	iono	= 0;
	var		= 0;
	
	for (int i = 0; i < localBasisVec.size(); i++)
	{
		auto& basis = localBasisVec[i];
		
		double coef = ionCoefLocal(std::cout, i, obs);
		if (coef == 0) 
			continue;
		
		auto it = ssrAtm.atmosRegionsMap[basis.regionID].stecData[Sat].lower_bound(time);
		if (it == ssrAtm.atmosRegionsMap[basis.regionID].stecData[Sat].end())
			continue;
		
		auto& [t0, ssrSTEC] = *it;
		
		var = ssrSTEC.accr;
		
		if (basis.type == +E_BasisType::POLYNOMIAL)		{	iono += coef * ssrSTEC.poly[basis.index];		}
		if (basis.type == +E_BasisType::GRIDPOINT)		{	iono += coef * ssrSTEC.grid[basis.index];		}
	}
	
	return var > 0;
}
