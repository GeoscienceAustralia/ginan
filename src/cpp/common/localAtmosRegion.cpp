
#include "coordinates.hpp"
#include "tropModels.hpp"
#include "navigation.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"


#define DEFAULT_LAT_INTERVAL 2.5
#define DEFAULT_LON_INTERVAL 5.0

int checkSSRRegion(
	VectorPos&	pos)
{
	int ssrAtmRegion = -1;

	if (pos[2] < -1000)
		return -1;
	if (nav.ssrAtm.atmosRegionsMap.empty())
		return -1;

	// tracepdeex(4,std::cout,"\n    Checking SSR regions %.4f %.4f... ", pos[0]*R2D, pos[1]*R2D);

	for (auto& [regId, regData] : nav.ssrAtm.atmosRegionsMap)
	{
		if (pos[0] > regData.maxLatDeg)	continue;
		if (pos[0] < regData.minLatDeg)	continue;

		double midLon = (regData.minLonDeg + regData.maxLonDeg)/2;
		double staLon = pos[1];
		if      ((staLon - midLon)> 180)	staLon -= 360;
		else if ((staLon - midLon)<-180)	staLon += 360;

		if (staLon > regData.maxLonDeg)	continue;
		if (staLon < regData.minLonDeg)	continue;

		// tracepdeex(4,std::cout," region = %d", regId);
		return regId;
	}
	// tracepdeex(4,std::cout," not found");
	return -1;
}

/** Initializes local ionosphere maps
	Parameters are set from files indicated by nav.SSRAtmMap.AtmosRegions
	*/
bool configAtmosRegion_File()
{
	auto& regMaps = nav.ssrAtm.atmosRegionsMap;

	regMaps.clear();

	for (auto& regionFile : acsConfig.atm_reg_definitions)
	{
		std::ifstream inputStream(regionFile);
		if (!inputStream)
		{
			BOOST_LOG_TRIVIAL(error)
			<< "Error: Ionosphere region definition file error " << regionFile;

			return false;
		}

		char	tmp[20];
		int 	regID		= -1;
		int 	gridType	= -1;
		double	lat0		= 0;
		int 	latNgrid	= 0;
		double	latInt		= 0;
		double	lon0		= 0;
		int 	lonNgrid	= 0;
		double	lonInt		= 0;
		int 	nind		= 0;

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
				regMaps[regID].gridLatDeg.clear();
				regMaps[regID].gridLonDeg.clear();
				nind=0;
				// std::cout << "    Configuring SSRATM:  Region " << regID << "\n";
				continue;
			}

			if (strstr(comment, "DEFINITION IOD"))
			{
				strncpy(tmp,buff   , 5); tmp[ 5] = '\0';		int regIOD = atoi(tmp);
				regMaps[regID].regionDefIOD = regIOD;
			}

			if (strstr(comment, "GRID TYPE"))
			{
				strncpy(tmp,buff   , 5); tmp[ 5] = '\0';		gridType = atoi(tmp);
				strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';		int tropGrid = atoi(tmp);
				strncpy(tmp,buff+40, 5); tmp[ 5] = '\0';		int ionoGrid = atoi(tmp);
				// std::cout << "    Configuring SSRATM:  Gridtype " << gridType << "\n";

				regMaps[regID].gridType = gridType;
				regMaps[regID].ionoGrid = (ionoGrid==1)?true:false;
				regMaps[regID].tropGrid = (tropGrid==1)?true:false;
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
				strncpy(tmp,buff   ,10); tmp[10] = '\0';		lat0		= atof(tmp);

				regMaps[regID].gridLatDeg[0] = lat0;

				if (gridType == 0)
				{
					latNgrid = 0;
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	latInt		= atof(tmp);
					if (latInt == 0)
						latInt = DEFAULT_LAT_INTERVAL;

					regMaps[regID].maxLatDeg = regMaps[regID].gridLatDeg[0] + 0.1;
					regMaps[regID].minLatDeg = regMaps[regID].gridLatDeg[0] - 0.1;
					nind = 1;
				}
				else if (gridType > 0)
				{
					strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';	latNgrid	= atoi(tmp);
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	latInt		= atof(tmp);

					if (gridType == 1)	//todo aaron magic numbers
					{
						regMaps[regID].minLatDeg = lat0 - latNgrid * latInt;
						regMaps[regID].maxLatDeg = lat0;
					}

					if (gridType == 2)
					{
						regMaps[regID].minLatDeg = lat0;
						regMaps[regID].maxLatDeg = lat0 + latNgrid * latInt;
					}
				}

				regMaps[regID].intLatDeg = latInt;

				continue;
			}

			if (strstr(comment, "REGION LONGITUDE"))
			{
				strncpy(tmp,buff   ,10); tmp[10] = '\0';		lon0		= atof(tmp);

				regMaps[regID].gridLonDeg[0] = lon0;

				if (gridType == 0)
				{
					lonNgrid = 0;
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	lonInt		= atof(tmp);
					if (lonInt == 0)
						lonInt = DEFAULT_LON_INTERVAL;

					regMaps[regID].maxLonDeg = regMaps[regID].gridLonDeg[0] + 0.1;
					regMaps[regID].minLonDeg = regMaps[regID].gridLonDeg[0] - 0.1;
					nind = 1;
				}
				else if (gridType > 0)
				{
					strncpy(tmp,buff+20, 5); tmp[ 5] = '\0';	lonNgrid	= atoi(tmp);
					strncpy(tmp,buff+40,10); tmp[10] = '\0';	lonInt		= atof(tmp);

					regMaps[regID].minLonDeg = lon0;
					regMaps[regID].maxLonDeg = lon0 + lonNgrid * lonInt;
				}
				regMaps[regID].intLonDeg = lonInt;
				continue;
			}

			if	(  strstr(comment, "GRID POINT")
				&& gridType == 0)
			{
				strncpy(tmp,buff   ,10); tmp[10] = '\0';	regMaps[regID].gridLatDeg[nind] = atof(tmp);
				strncpy(tmp,buff+20,10); tmp[10] = '\0';	regMaps[regID].gridLonDeg[nind] = atof(tmp);

				if ( (regMaps[regID].gridLatDeg[nind] + 0.1) > regMaps[regID].maxLatDeg)				regMaps[regID].maxLatDeg = regMaps[regID].gridLatDeg[nind] + 0.1;
				if ( (regMaps[regID].gridLonDeg[nind] + 0.1) > regMaps[regID].maxLonDeg)				regMaps[regID].maxLonDeg = regMaps[regID].gridLonDeg[nind] + 0.1;
				if ( (regMaps[regID].gridLatDeg[nind] - 0.1) < regMaps[regID].minLatDeg)				regMaps[regID].minLatDeg = regMaps[regID].gridLatDeg[nind] - 0.1;
				if ( (regMaps[regID].gridLonDeg[nind] - 0.1) < regMaps[regID].minLonDeg)				regMaps[regID].minLonDeg = regMaps[regID].gridLonDeg[nind] - 0.1;

				nind++;
			}


			if (strstr(comment, "REGION END"))
			if	(  latNgrid > 1
				|| lonNgrid > 1)
			{
				nind = 0;
				if (gridType == 1)
				for (int i = 0; i <= latNgrid; i++)
				for (int j = 0; j <= lonNgrid; j++)
				{
					regMaps[regID].gridLatDeg[nind] = lat0 - latInt * i;
					regMaps[regID].gridLonDeg[nind] = lon0 + lonInt * j;
					nind++;
				}

				if (gridType == 2)
				for (int j = 0; j <= lonNgrid; j++)
				for (int i = 0; i <= latNgrid; i++)
				{
					regMaps[regID].gridLatDeg[nind] = lat0 + latInt * i;
					regMaps[regID].gridLonDeg[nind] = lon0 + lonInt * j;
					nind++;
				}
				continue;
			}
		}
	}

	defineLocalTropBasis();

	bool pass = (regMaps.empty() == false);
	return pass;
}


bool configAtmosRegions(
	Trace&			trace,
	ReceiverMap&	receiverMap)
{
	if (acsConfig.atm_reg_definitions.empty() == false)
		return configAtmosRegion_File();

	int regionId = acsConfig.ssrOpts.region_id;
	if (regionId < 0)
		return false;

	trace << "\n" << "SSR Atmosphere Region #" << regionId << ": ";

	if	(  acsConfig.ssrOpts.grid_type	< 0
		&& acsConfig.ssrOpts.npoly_trop	< 0
		&& acsConfig.ssrOpts.npoly_iono	< 0)
	{
		return false;
	}

	trace << "gridtype: " << acsConfig.ssrOpts.grid_type << "; TropPoly: " << acsConfig.ssrOpts.npoly_trop << "; IonoPoly: " << acsConfig.ssrOpts.npoly_iono << "\n";

	bool coordFromRec = false;
	if	(  acsConfig.ssrOpts.grid_type == 0
		|| acsConfig.ssrOpts.lat_max <= acsConfig.ssrOpts.lat_min
		|| acsConfig.ssrOpts.lon_max <= acsConfig.ssrOpts.lon_min)
	{
		coordFromRec = true;
	}

	SSRAtmRegion& atmRegion = nav.ssrAtm.atmosRegionsMap[regionId];

	atmRegion.regionDefIOD = acsConfig.ssrOpts.region_iod;

	switch (acsConfig.ssrOpts.npoly_trop)
	{
		case 1:		atmRegion.tropPolySize = 1; break;
		case 2:
		case 3:		atmRegion.tropPolySize = 3; break;
		case 4:		atmRegion.tropPolySize = 4; break;
		default:	atmRegion.tropPolySize = -1;
	}

	switch (acsConfig.ssrOpts.npoly_iono)
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
		for (auto& [id, rec] : receiverMap)
		{
			VectorEcef&	snxPos		= rec.snx.pos;

			auto& recOpts = acsConfig.getRecOpts(id);

			if (recOpts.apriori_pos.isZero() == false)
				snxPos	= recOpts.apriori_pos;

			auto& pos = rec.pos;
			pos = ecef2pos(snxPos);

			if (atmRegion.gridLatDeg.empty())
			{
				atmRegion.minLatDeg = pos.latDeg();
				atmRegion.maxLatDeg = pos.latDeg();
				atmRegion.minLonDeg = pos.lonDeg();
				atmRegion.maxLonDeg = pos.lonDeg();
			}

			if (atmRegion.minLatDeg > pos.lat())	atmRegion.minLatDeg = pos.latDeg();
			if (atmRegion.maxLatDeg < pos.lat())	atmRegion.maxLatDeg = pos.latDeg();

			double midLonDeg = (atmRegion.minLonDeg + atmRegion.maxLonDeg) / 2;
			double recLonDeg = pos.lonDeg();
			if		((recLonDeg - midLonDeg) > 180)		recLonDeg -= 360;
			else if	((recLonDeg - midLonDeg) <-180)		recLonDeg += 360;

			if (atmRegion.minLonDeg > recLonDeg)		atmRegion.minLonDeg = recLonDeg;
			if (atmRegion.maxLonDeg < recLonDeg)		atmRegion.maxLonDeg = recLonDeg;

			atmRegion.gridLatDeg[ngrid] = pos.latDeg();
			atmRegion.gridLonDeg[ngrid] = pos.lonDeg();
			ngrid++;
		}
		atmRegion.minLonDeg -= 0.001;
		atmRegion.minLatDeg -= 0.001;
		atmRegion.maxLatDeg += 0.001;
		atmRegion.maxLonDeg += 0.001;
	}

	if (acsConfig.ssrOpts.lat_max > acsConfig.ssrOpts.lat_min)
	{
		atmRegion.minLatDeg = acsConfig.ssrOpts.lat_min;
		atmRegion.maxLatDeg = acsConfig.ssrOpts.lat_max;
	}

	if (acsConfig.ssrOpts.lon_max > acsConfig.ssrOpts.lon_min)
	{
		atmRegion.minLonDeg = acsConfig.ssrOpts.lon_min;
		atmRegion.maxLonDeg = acsConfig.ssrOpts.lon_max;
	}

	if (acsConfig.ssrOpts.grid_type < 0)
	{
		ngrid = 0;
		atmRegion.gridLatDeg.clear();
		atmRegion.gridLonDeg.clear();
		atmRegion.gridLatDeg[0] = atmRegion.maxLatDeg;
		atmRegion.gridLonDeg[0] = atmRegion.minLonDeg;
	}

	if (acsConfig.ssrOpts.grid_type > 0)
	{
		atmRegion.gridLatDeg.clear();
		atmRegion.gridLonDeg.clear();

		int nIntLat;
		int nIntLon;
		if (acsConfig.ssrOpts.lat_int > 0)	{	atmRegion.intLatDeg = acsConfig.ssrOpts.lat_int;						nIntLat  = floor ((atmRegion.maxLatDeg - atmRegion.minLatDeg) / atmRegion.intLatDeg) + 1;	}
		else								{	atmRegion.intLatDeg = atmRegion.maxLatDeg - atmRegion.minLatDeg;		nIntLat  = 1;																				}
		if (acsConfig.ssrOpts.lon_int > 0)	{	atmRegion.intLonDeg = acsConfig.ssrOpts.lon_int;						nIntLon  = floor ((atmRegion.maxLonDeg - atmRegion.minLonDeg) / atmRegion.intLonDeg) + 1;	}
		else								{	atmRegion.intLonDeg = atmRegion.maxLonDeg - atmRegion.minLonDeg;		nIntLon  = 1;																				}


		ngrid = 0;
		if (acsConfig.ssrOpts.grid_type == 1)
		for (int i = 0; i <= nIntLat; i++)
		for (int j = 0; j <= nIntLon; j++)
		{
			atmRegion.gridLatDeg[ngrid] = atmRegion.maxLatDeg - atmRegion.intLatDeg * i;
			atmRegion.gridLonDeg[ngrid] = atmRegion.minLonDeg + atmRegion.intLonDeg * j;
			ngrid++;
		}

		if (acsConfig.ssrOpts.grid_type == 2)
		for (int i = 0; i <= nIntLon; i++)
		for (int j = 0; j <= nIntLat; j++)
		{
			atmRegion.gridLatDeg[ngrid] = atmRegion.minLatDeg + atmRegion.intLatDeg * i;
			atmRegion.gridLonDeg[ngrid] = atmRegion.minLonDeg + atmRegion.intLonDeg * j;
			ngrid++;
		}
	}

	acsConfig.ssrOpts.ngrid = ngrid;

	if (atmRegion.intLatDeg == 0)	atmRegion.intLatDeg = atmRegion.maxLatDeg - atmRegion.minLatDeg;
	if (atmRegion.intLonDeg == 0)	atmRegion.intLonDeg = atmRegion.maxLonDeg - atmRegion.minLonDeg;

	trace << ";  Lats: " << atmRegion.minLatDeg << ", " << atmRegion.maxLatDeg;
	trace << ";  Lons: " << atmRegion.minLonDeg << ", " << atmRegion.maxLonDeg;
	trace << ";  Lat0: " << atmRegion.gridLatDeg[0];
	trace << ";  Lon0: " << atmRegion.gridLonDeg[0];

	defineLocalTropBasis();

	return true;
}
