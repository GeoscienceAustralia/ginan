#include "observations.hpp"
#include "coordinates.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "common.hpp"

#define IONO_OUT_THRESHOLD		120
#define DEFAULT_STEC_POLY_ACC	0.5

struct LocalBasis
{
	int		regionID;					/* Atmospheric region ID */
	SatSys	Sat;						/* Satellite System */
	int		type;						/* parameter type: 0 polnomial, 1 gridpoint */
	int		index;						/* parameter index */
	GTime	lastUpdt;					/* last updated */
};

map<int, LocalBasis>  localBasisMap;

int defineLocalIonoBasis()
{
	int nbasis = 0;
	localBasisMap.clear();
	
	for (auto& [iatm, atmReg] : nav.ssrAtm.atmosRegionsMap)
	for (auto& [Sat, satNav] : nav.satNavMap)
	{
		for (int i = 0; i < atmReg.ionoPolySize; i++)
		{
			localBasisMap[nbasis].regionID	= iatm;
			localBasisMap[nbasis].Sat		= Sat;
			localBasisMap[nbasis].type		= 0;
			localBasisMap[nbasis].index		= i;
			nbasis++;
		}
		for (auto& [igrid,latgrid] : atmReg.gridLat)
		{
			localBasisMap[nbasis].regionID	= iatm;
			localBasisMap[nbasis].Sat		= Sat;
			localBasisMap[nbasis].type		= 1;
			localBasisMap[nbasis].index		= igrid;
			nbasis++;
		}
	}
	
	acsConfig.ionModelOpts.numBasis			= nbasis;
	acsConfig.ionModelOpts.layer_heights.clear();
	acsConfig.ionModelOpts.layer_heights[0]	= 0;							/* local ionosphere are mapped with respect to ground */
	acsConfig.ionModelOpts.estimate_sat_dcb	= false;
	
	return nbasis;
}

/** Initializes local ionosphere maps
	Parameters are set from files indicated by nav.SSRAtmMap.AtmosRegions
	*/
bool configLocalIonoFromFile()
{
	auto& regMaps = nav.ssrAtm.atmosRegionsMap;
		
	for (auto& region_file : acsConfig.atm_reg_definitions)
	{
		std::ifstream inputStream(region_file);
		if (!inputStream)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: Ionosphere region definition file error";

			return false;
		}
		
		char	tmp[20];
		int 	regID	 = -1;
		int 	gridType = -1;
		double	latitud0 = 0;
		int 	latNgrid = 0;
		double	latInter = 0;
		double	longitu0 = 0;
		int 	lonNgrid = 0;
		double	lonInter = 0;
		int 	nind	 = 0;
				
		string line;
		while (std::getline(inputStream, line))
		{
			char* buff = &line[0];
			char* comment = buff + 60;
			
			if (strlen(buff) < 60 )								{	continue;	}
			if (strstr(comment, "COMMENT"))						{	continue;	}
			
			if (strstr(comment, "REGION NUMBER"))
			{
				strncpy(tmp,buff   ,3);		tmp[3] = '\0';
				regID = atoi(tmp);
				regMaps[regID].gridLat.clear();
				regMaps[regID].gridLon.clear();
				nind=0;
				
				continue;
			}
			
			if (strstr(comment, "GRID TYPE"))
			{
				strncpy(tmp,buff   , 5); tmp[ 5] = '\0';		gridType = atoi(tmp);
			}
			
			if (strstr(comment, "POLYNOMIAL SIZE"))
			{
				int polySize1;
				int polySize2;
				
				strncpy(tmp,buff   , 5); tmp[ 5] = '\0';		polySize1 = atoi(tmp);
				strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';		polySize2 = atoi(tmp);
				
				regMaps[regID].tropPolySize = polySize1;
				regMaps[regID].ionoPolySize = polySize2;
				
				continue;
			}
			
			if (strstr(comment, "REGION LATITUDE"))
			{
				strncpy(tmp,buff   ,10); tmp[10] = '\0';		latitud0 = atof(tmp)*D2R;
				regMaps[regID].gridLat[0] = latitud0;
				if (gridType==0)
				{
					latNgrid = 0;
					nind = 1;
				}
				else if (gridType>0)
				{
					strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';	latNgrid = atoi(tmp);
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	latInter = atof(tmp)*D2R;
					
					if (gridType == 1)
					{
						regMaps[regID].minLat = latitud0 - latNgrid * latInter;
						regMaps[regID].maxLat = latitud0;
					}
					
					if (gridType == 2)
					{
						regMaps[regID].minLat = latitud0;
						regMaps[regID].maxLat = latitud0 + latNgrid * latInter;
					}
					regMaps[regID].intLat = latInter;
				}
				
				continue;
			}
			
			if (strstr(comment, "REGION LONGITUDE"))
			{
				strncpy(tmp,buff   ,10); tmp[10] = '\0';		longitu0 = atof(tmp)*D2R;
				regMaps[regID].gridLon[0] = longitu0;
				if (gridType == 0)
				{
					lonNgrid = 0;
					nind = 1;
				}
				else if (gridType > 0)
				{
					strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';	lonNgrid = atoi(tmp);
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	lonInter = atof(tmp)*D2R;
					
					regMaps[regID].minLon = longitu0;
					regMaps[regID].maxLon = longitu0 + lonNgrid*lonInter;
					regMaps[regID].intLon = lonInter;
				}
				
				continue;
			}
			
			if	(  strstr(comment, "GRID DELTA")
				&& gridType == 0)
			{
				int deltaLatitude;
				int deltaLongitud;
				
				strncpy(tmp,buff   ,10); tmp[10] = '\0';	deltaLatitude = atof(tmp)*D2R;
				strncpy(tmp,buff+20,10); tmp[10] = '\0';	deltaLongitud = atof(tmp)*D2R;
				
				regMaps[regID].gridLat[nind] = regMaps[regID].gridLat[nind-1] + deltaLatitude;
				regMaps[regID].gridLon[nind] = regMaps[regID].gridLon[nind-1] + deltaLongitud;
				
				if (regMaps[regID].gridLat[nind] > regMaps[regID].maxLat)				regMaps[regID].maxLat = regMaps[regID].gridLat[nind];
				if (regMaps[regID].gridLon[nind] > regMaps[regID].maxLon)				regMaps[regID].maxLon = regMaps[regID].gridLon[nind];
				
				nind++;
			}
			
			
			if (strstr(comment, "REGION END"))
			if	(  latNgrid > 1 
				|| lonNgrid > 1)
			{
				nind = 0;
				if (gridType==1)
				for (int i = 0; i <= latNgrid; i++)
				for (int j = 0; j <= lonNgrid; j++)
				{
					regMaps[regID].gridLat[nind] = latitud0 - latInter*i;
					regMaps[regID].gridLon[nind] = longitu0 + lonInter*j;
					nind++;
				}
				
				if (gridType==2)
				for (int j = 0; j <= lonNgrid; j++)
				for (int i = 0; i <= latNgrid; i++)
				{
					regMaps[regID].gridLat[nind] = latitud0 + latInter*i;
					regMaps[regID].gridLon[nind] = longitu0 + lonInter*j;
					nind++;
				}
				
				continue;
			}
		}
	}
	
	return defineLocalIonoBasis() > 0;
}

/** Checks if the Ionosphere Piercing Point falls in area of coverage. 
Return the region ID containing the IPP, 0 if out of coverage
*/
int ippCheckLocal(
	GTime		time, 			///< time of observations (not used)
	VectorPos&	ionPP)			///< Ionospheric piercing point to be updated
{
	auto& RegMaps = nav.ssrAtm.atmosRegionsMap;
	for (auto& [iatm,atmReg] : RegMaps)
	{
		
		if (ionPP[0] < atmReg.minLat)			continue;
		if (ionPP[0] > atmReg.maxLat)			continue;
		if (ionPP[1] > atmReg.maxLon)			continue;
		
		if	(ionPP[1] < atmReg.minLon
			&&( atmReg.maxLon < PI
			  || ionPP[1] > atmReg.maxLon-(2*PI)))
		{
			continue;	
		}

		return iatm + 1;
	}
	
	return 0;
}


/** calcuates the partials of observations with respect to basis functions
	int ind			I		Basis function number
	meas			I		Ionosphere measurement struct
		latIPP				- Latitude of Ionosphere Piercing Point
		lonIPP				- Longitude of Ionosphere Piercing Point

	This function should be called after ippCheckLocal
----------------------------------------------------------------------------*/
double ionCoefLocal(int ind, IonoObs& obs)
{
	if (localBasisMap.find(ind) == localBasisMap.end())		return 0;
	
	LocalBasis& basis = localBasisMap[ind];
	
	if (obs.ionoSat != basis.Sat)							return 0;
	
	auto& atmReg = nav.ssrAtm.atmosRegionsMap[basis.regionID];
	double recLat = obs.ippMap[0].lat;
	double recLon = obs.ippMap[0].lon;
	
	if (recLat > atmReg.maxLat)							return 0;
	if (recLat < atmReg.minLat)							return 0;
	if (recLon > atmReg.maxLon)							return 0;
	if (recLon < atmReg.minLon)
	{
		if (atmReg.maxLon < PI)							return 0;
		if (recLon > (atmReg.maxLon-2*PI))				return 0;
		recLon += 2*PI;
	}

	double latdiff = recLat - atmReg.gridLat[0];
	double londiff = recLon - atmReg.gridLon[0];

	if (basis.type == 0)
	{
		switch (basis.index)
		{
			case 0: 	return 1;
			case 1: 	return latdiff;
			case 2: 	return londiff;
			case 3: 	return latdiff*londiff;
			case 4: 	return latdiff*latdiff;
			case 5: 	return londiff*londiff;
			default:	return 0;
		}
	}
	
	if (basis.type == 1)
	{
		double dlat = fabs(recLat - atmReg.gridLat[basis.index]);
		double dlon = fabs(recLon - atmReg.gridLon[basis.index]);
		
		if (dlat > atmReg.intLat || atmReg.intLat == 0)		return 0;
		if (dlon > atmReg.intLon || atmReg.intLon == 0)		return 0;
		
		return (1-dlat/atmReg.intLat)*(1-dlon/atmReg.intLon);		//todo aaorn use bilinear interpolation function?
	}

	return 0;
}

void ionOutputLocal(
	Trace&		trace, 
	KFState&	kfState)
{
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONOSPHERIC)
			continue;
		
		LocalBasis basis	= localBasisMap[key.num];
		auto& atmReg    	= nav.ssrAtm.atmosRegionsMap[basis.regionID];
		auto& stec_record	= atmReg.stecData[basis.Sat][kfState.time];
		
		if		(basis.type == 0)
		{
			double val;
			double var;
			kfState.getKFValue(key, val, &var);
			
			stec_record.poly[basis.index]	= val;
			stec_record.accr				= DEFAULT_STEC_POLY_ACC; 
		}
		else if	(basis.type == 1)
		{
			double val;
			double var;
			kfState.getKFValue(key, val,&var);
			
			stec_record.grid[basis.index] = val;
			
			if (sqrt(var) > stec_record.accr)
				stec_record.accr = sqrt(var); 
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
	
	iono	= 0;
	var		= 0;
	for (auto [ind,basis] : localBasisMap)
	{
		double coef = ionCoefLocal(ind, obs);
		if (coef == 0) 
			continue;
		
		auto it = ssrAtm.atmosRegionsMap[basis.regionID].stecData[Sat].lower_bound(time);
		if (it == ssrAtm.atmosRegionsMap[basis.regionID].stecData[Sat].end())
			continue;
		
		auto& [t0, ssrSTEC] = *it;
		
		var = ssrSTEC.accr;
		
		if (basis.type == 0)		{	iono += coef * ssrSTEC.poly[basis.index];		}
		if (basis.type == 1)		{	iono += coef * ssrSTEC.grid[basis.index];		}
	}
	
	return var > 0;
}
