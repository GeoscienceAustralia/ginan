
#include "coordinates.hpp"
#include "tropModels.hpp"
#include "navigation.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"


#define DEFAULT_LAT_INTER 2.5
#define DEFAULT_LON_INTER 5.0

/** Initializes local ionosphere maps
	Parameters are set from files indicated by nav.SSRAtmMap.AtmosRegions
	*/
bool configAtmosRegion_File()
{
	auto& regMaps = nav.ssrAtm.atmosRegionsMap;
		
	for (auto& regionFile : acsConfig.atm_reg_definitions)
	{
		std::ifstream inputStream(regionFile);
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
				strncpy(tmp, buff   ,3);		tmp[3] = '\0';
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
				strncpy(tmp,buff   , 5); tmp[ 5] = '\0';		int polySize1 = atoi(tmp);
				strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';		int polySize2 = atoi(tmp);
				
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
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	latInter = atof(tmp)*D2R;
					if (latInter == 0)
						latInter = DEFAULT_LAT_INTER;
					nind = 1;
				}
				else if (gridType>0)
				{
					strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';	latNgrid = atoi(tmp);
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	latInter = atof(tmp)*D2R;
					
					if (gridType == 1)	//todo aaron magic numbers
					{
						regMaps[regID].minLat = latitud0 - latNgrid * latInter;
						regMaps[regID].maxLat = latitud0;
					}
					
					if (gridType == 2)
					{
						regMaps[regID].minLat = latitud0;
						regMaps[regID].maxLat = latitud0 + latNgrid * latInter;
					}
				}
				regMaps[regID].intLat = latInter;
				continue;
			}
			
			if (strstr(comment, "REGION LONGITUDE"))
			{
				strncpy(tmp,buff   ,10); tmp[10] = '\0';		longitu0 = atof(tmp)*D2R;
				regMaps[regID].gridLon[0] = longitu0;
				if (gridType == 0)
				{
					lonNgrid = 0;
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	lonInter = atof(tmp)*D2R;
					if (lonInter == 0)
						lonInter = DEFAULT_LON_INTER;
					nind = 1;
				}
				else if (gridType > 0)
				{
					strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';	lonNgrid = atoi(tmp);
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	lonInter = atof(tmp)*D2R;
					
					regMaps[regID].minLon = longitu0;
					regMaps[regID].maxLon = longitu0 + lonNgrid*lonInter;
				}
				regMaps[regID].intLon = lonInter;
				continue;
			}
			
			if	(  strstr(comment, "GRID DELTA")
				&& gridType == 0)
			{
				strncpy(tmp,buff   ,10); tmp[10] = '\0';	int deltaLatitude = atof(tmp)*D2R;
				strncpy(tmp,buff+20,10); tmp[10] = '\0';	int deltaLongitud = atof(tmp)*D2R;
				
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
	
	defineLocalTropBasis();
	
	return defineLocalIonoBasis() > 0;
}


bool configAtmosRegions(
	map<string, Station>&		stationMap)
{
	if (acsConfig.atm_reg_definitions.empty() == false)
		return configAtmosRegion_File();
		
	if (acsConfig.ssrOpts.region_id < 0)
		return false;
	
	std::cout << std::endl << "SSR Atmosphere Region #" << acsConfig.ssrOpts.region_id << ": ";
			
	if	(  acsConfig.ssrOpts.grid_type	< 0
		&& acsConfig.ssrOpts.npoly_trop	< 0
		&& acsConfig.ssrOpts.npoly_iono	< 0)
	{
		return false;
	}
	
	std::cout << "gridtype: " << acsConfig.ssrOpts.grid_type << "; TropPoly: " << acsConfig.ssrOpts.npoly_trop << "; IonoPoly: " << acsConfig.ssrOpts.npoly_iono << std::endl;

	bool coordFromRec = false; 
	if	(  acsConfig.ssrOpts.grid_type == 0
		|| acsConfig.ssrOpts.max_lat <= acsConfig.ssrOpts.min_lat
		|| acsConfig.ssrOpts.max_lon <= acsConfig.ssrOpts.min_lon)
	{                                                                                                	
		coordFromRec = true; 
	}
	
	SSRAtmRegion& atmRegion = nav.ssrAtm.atmosRegionsMap[acsConfig.ssrOpts.region_id];
	
	switch(acsConfig.ssrOpts.npoly_trop)
	{
		case 1:		atmRegion.tropPolySize = 1; break;
		case 2:	
		case 3:		atmRegion.tropPolySize = 3; break;
		case 4:		atmRegion.tropPolySize = 4; break;
		default:	atmRegion.tropPolySize = -1; 
	}
	
	switch(acsConfig.ssrOpts.npoly_iono)
	{
		case 1:		atmRegion.ionoPolySize = 1; break;
		case 2:	
		case 3:		atmRegion.ionoPolySize = 3; break;
		case 4:		atmRegion.ionoPolySize = 4; break;
		case 5:	
		case 6:		atmRegion.ionoPolySize = 6; break;
		default:	atmRegion.ionoPolySize = -1; 
	}
	
	atmRegion.gridType	= acsConfig.ssrOpts.grid_type;
	atmRegion.ionoGrid	= acsConfig.ssrOpts.use_grid_iono;
	atmRegion.tropGrid	= acsConfig.ssrOpts.use_grid_trop;
	
	int ngrid = 0;
	if (coordFromRec)
	{
		for (auto& [id,rec] : stationMap)
		{
			VectorEcef&	snxPos		= rec.snx.pos;
			
			auto& recOpts = acsConfig.getRecOpts(rec.id);
			
			if (recOpts.apriori_pos.isZero() == false)
				snxPos	= recOpts.apriori_pos;
			
			auto& pos = rec.pos;
			pos = ecef2pos(snxPos);
		
			if (atmRegion.gridLat.empty())
			{
				atmRegion.minLat = pos[0];
				atmRegion.maxLat = pos[0];
				atmRegion.minLon = pos[1];
				atmRegion.maxLon = pos[1];
			}
		
			if (atmRegion.minLat > pos[0])	atmRegion.minLat = pos[0];
			if (atmRegion.maxLat < pos[0])	atmRegion.maxLat = pos[0];
		
			double midLon = (atmRegion.minLon + atmRegion.maxLon)/2;
			double staLon = pos[1];
			if      ((staLon - midLon)> PI)	staLon -= 2*PI;
			else if ((staLon - midLon)<-PI)	staLon += 2*PI;
		
			if (atmRegion.minLon > staLon)	atmRegion.minLon = staLon;
			if (atmRegion.maxLon < staLon)	atmRegion.maxLon = staLon;
		
			atmRegion.gridLat[ngrid] = pos[0];
			atmRegion.gridLon[ngrid] = pos[1];
			ngrid++;
		}
		atmRegion.minLon -= 0.001 * D2R;
		atmRegion.minLat -= 0.001 * D2R;
		atmRegion.maxLat += 0.001 * D2R;
		atmRegion.maxLon += 0.001 * D2R;
	}
	
	if (acsConfig.ssrOpts.max_lat > acsConfig.ssrOpts.min_lat)
	{
		atmRegion.minLat = acsConfig.ssrOpts.min_lat*D2R;
		atmRegion.maxLat = acsConfig.ssrOpts.max_lat*D2R;
	}
	
	if (acsConfig.ssrOpts.max_lon > acsConfig.ssrOpts.min_lon)
	{
		atmRegion.minLon = acsConfig.ssrOpts.min_lon*D2R;
		atmRegion.maxLon = acsConfig.ssrOpts.max_lon*D2R;
	}
	
	if (acsConfig.ssrOpts.grid_type < 0)
	{
		ngrid = 0;
		atmRegion.gridLat.clear();
		atmRegion.gridLon.clear();
		atmRegion.gridLat[0] = atmRegion.maxLat;
		atmRegion.gridLon[0] = atmRegion.minLon;
	}
	
	if (acsConfig.ssrOpts.grid_type > 0)
	{    
		atmRegion.gridLat.clear();
		atmRegion.gridLon.clear();
		
		int    nIntLat;
		int    nIntLon;
		if (acsConfig.ssrOpts.int_lat > 0)	{	atmRegion.intLat = acsConfig.ssrOpts.int_lat;				nIntLat  = floor ((atmRegion.maxLat - atmRegion.minLat) / atmRegion.intLat) + 1;		}
		else								{	atmRegion.intLat = atmRegion.maxLat - atmRegion.minLat;		nIntLat  = 1;																		}				
		if (acsConfig.ssrOpts.int_lon > 0)	{	atmRegion.intLon = acsConfig.ssrOpts.int_lon;				nIntLon  = floor ((atmRegion.maxLon - atmRegion.minLon) / atmRegion.intLon) + 1;		}	
		else								{	atmRegion.intLon = atmRegion.maxLon - atmRegion.minLon;		nIntLon  = 1;																		}
		
		ngrid = 0;
		if (acsConfig.ssrOpts.grid_type == 1)
		for (int i = 0; i <= nIntLat; i++)
		for (int j = 0; j <= nIntLon; j++)
		{
			atmRegion.gridLat[ngrid] = atmRegion.maxLat - atmRegion.intLat * i;
			atmRegion.gridLon[ngrid] = atmRegion.minLon + atmRegion.intLon * j;
			ngrid++;
		}
		
		if (acsConfig.ssrOpts.grid_type == 2)
		for (int i = 0; i <= nIntLon; i++)
		for (int j = 0; j <= nIntLat; j++)
		{
			atmRegion.gridLat[ngrid] = atmRegion.minLat + atmRegion.intLat * i;
			atmRegion.gridLon[ngrid] = atmRegion.minLon + atmRegion.intLon * j;
			ngrid++;
		}
	}
	
	acsConfig.ssrOpts.ngrid = ngrid;
	
	if (atmRegion.intLat == 0)	atmRegion.intLat = atmRegion.maxLat - atmRegion.minLat;
	if (atmRegion.intLon == 0)	atmRegion.intLon = atmRegion.maxLon - atmRegion.minLon;

	std::cout << ";  Lats: " << atmRegion.minLat*R2D << ", " << atmRegion.maxLat*R2D;
	std::cout << ";  Lons: " << atmRegion.minLon*R2D << ", " << atmRegion.maxLon*R2D;    
	std::cout << ";  Lat0: " << atmRegion.gridLat[0]*R2D;
	std::cout << ";  Lon0: " << atmRegion.gridLon[0]*R2D;
	
	defineLocalTropBasis();
	return defineLocalIonoBasis() > 0;
}
