#include "observations.hpp"
#include "coordinates.hpp"
#include "ionoModel.hpp"
#include "acsConfig.hpp"
#include "common.hpp"

#define IONO_OUT_THRESHOLD		120
#define DEFAULT_STEC_POLY_ACC	0.5

double currRecLatDeg = 0;
double currRecLonDeg = 0;

map<int,map<int,double>> irrGrdCoef;

struct LocalBasis
{
	int			regionID;					///< Atmospheric region ID
	SatSys		Sat;						///< Satellite System
	E_BasisType	type;						///< parameter type
	int			index;						///< parameter index
};

map<int, LocalBasis>  localBasisVec;

double IrregGridCoef(
	int 			Index,
	double			recLatDeg,
	double			recLonDeg)
{
	if	(  recLatDeg != currRecLatDeg
		|| recLonDeg != currRecLonDeg)
	{
		double acum = 0;
		SatSys sat0 = localBasisVec[0].Sat;

		map<int,map<int,double>> coefMap;

		for (auto& [ind, basis] : localBasisVec)
		{
			if (basis.Sat != sat0)
				break;

			if (basis.type != +E_BasisType::GRIDPOINT)
				continue;

			auto& atmReg = nav.ssrAtm.atmosRegionsMap[basis.regionID];

			double dlat = fabs(recLatDeg - atmReg.gridLatDeg[basis.index]);
			double dlon = fabs(recLonDeg - atmReg.gridLonDeg[basis.index]);

			if (dlat > atmReg.intLatDeg || atmReg.intLatDeg == 0)		continue;
			if (dlon > atmReg.intLonDeg || atmReg.intLonDeg == 0)		continue;

			coefMap[basis.regionID][basis.index] = (1 - dlat / atmReg.intLatDeg) * (1 - dlon / atmReg.intLonDeg);

			acum += coefMap[basis.regionID][basis.index];
		}

		irrGrdCoef.clear();

		for (auto& [regId, coef] : coefMap)
		for (auto& [grdId, c] : coef)
			irrGrdCoef[regId][grdId] = c / acum;

		currRecLatDeg = recLatDeg;
		currRecLonDeg = recLonDeg;
	}

	auto& basis = localBasisVec[Index];

	if (irrGrdCoef.find(basis.regionID) == irrGrdCoef.end())
		return 0;

	if (irrGrdCoef[basis.regionID].find(basis.index) == irrGrdCoef[basis.regionID].end())
		return 0;

	return irrGrdCoef[basis.regionID][basis.index];
}

int configIonModelLocal_(
	Trace& trace)
{
	localBasisVec.clear();

	int ind = 0;

	for (auto& [Sat, satNav]	: nav.satNavMap)
	for (auto& [iatm, atmReg]	: nav.ssrAtm.atmosRegionsMap)
	{
		if	(  atmReg.gridType >= 0
			&& atmReg.ionoGrid)
		for (auto& [igrid, latgrid] : atmReg.gridLatDeg)
		{
			LocalBasis basis;
			basis.regionID	= iatm;
			basis.Sat		= Sat;
			basis.type		= E_BasisType::GRIDPOINT;
			basis.index		= igrid;

			localBasisVec[ind] = basis;
			ind++;
		}
		else
		for (int i = 0; i < atmReg.ionoPolySize; i++)
		{
			LocalBasis basis;
			basis.regionID	= iatm;
			basis.Sat		= Sat;
			basis.type		= E_BasisType::POLYNOMIAL;
			basis.index		= i;

			localBasisVec[ind] = basis;
			ind++;
		}
	}

	if (localBasisVec.empty())
		return 0;

	acsConfig.ionModelOpts.numBasis			= localBasisVec.size();
	acsConfig.ionModelOpts.estimate_sat_dcb	= false;
	acsConfig.ionModelOpts.layer_heights.clear();
	acsConfig.ionModelOpts.layer_heights.push_back(0);

	// tracepdeex(2,trace, "\nIONO_BASIS ind reg sat type ind");
	// for (auto [j, basis] : localBasisVec)
	// 	tracepdeex(2,trace, "\nIONO_BASIS %3d %2d %s %s %3d ", j, basis.regionID, basis.Sat.id().c_str(), (basis.type==+E_BasisType::POLYNOMIAL)?"poly":"grid", basis.index);
	// tracepdeex(2,trace, "\n");

	return acsConfig.ionModelOpts.numBasis;
}

/** Checks if the Ionosphere Piercing Point falls in area of coverage.
Return true if there is a region containing the IPP, false if out of coverage
*/
bool ippCheckLocal(
	GTime		time, 			///< time of observations (not used)
	VectorPos&	ionPP)			///< Ionospheric piercing point to be updated
{
	auto& RegMaps = nav.ssrAtm.atmosRegionsMap;

	for (auto& [iatm,atmReg] : RegMaps)
	{
		if (ionPP.latDeg() < atmReg.minLatDeg)			continue;
		if (ionPP.latDeg() > atmReg.maxLatDeg)			continue;

		double recLonDeg = ionPP.lonDeg();
		double midLonDeg = (atmReg.minLonDeg + atmReg.maxLonDeg) / 2;

		if		((recLonDeg - midLonDeg) > 180)		recLonDeg -= 360;
		else if	((recLonDeg - midLonDeg) <-180)		recLonDeg += 360;

		if (recLonDeg > atmReg.maxLonDeg)				continue;
		if (recLonDeg < atmReg.minLonDeg)				continue;

		return true;
	}

	return false;
}

double ionCoefPolynomial(
	Trace&			trace,
	SSRAtmRegion&	atmReg,
	double			latDeg,
	double			lonDeg,
	int 			ind)
{
	if (atmReg.maxLatDeg	<=	atmReg.minLatDeg)						return 0;
	if (atmReg.maxLonDeg	<=	atmReg.minLonDeg)						return 0;
	if (latDeg				>	atmReg.maxLatDeg)						return 0;
	if (latDeg				<	atmReg.minLatDeg)						return 0;

	double recLonDeg = lonDeg;
	double midLonDeg = (atmReg.minLonDeg + atmReg.maxLonDeg) / 2;
	if		((recLonDeg - midLonDeg) >  180)		recLonDeg -= 360;
	else if	((recLonDeg - midLonDeg) < -180)		recLonDeg += 360;

	if (recLonDeg > atmReg.maxLonDeg)									return 0;
	if (recLonDeg < atmReg.minLonDeg)									return 0;

	double latdiff = (latDeg	- atmReg.gridLatDeg[0])/(atmReg.maxLatDeg-atmReg.minLatDeg);
	double londiff = (recLonDeg	- atmReg.gridLonDeg[0])/(atmReg.maxLonDeg-atmReg.minLonDeg);

	tracepdeex (4, trace, "\nPolinomial basis %d: %.4f, %.4f", ind, atmReg.gridLatDeg[0], atmReg.gridLonDeg[0]);

	switch (ind)
	{
		case 0: 	return 1;
		case 1: 	return 2*latdiff;
		case 2: 	return 2*londiff;
		case 3: 	return 4*latdiff * londiff;
		case 4: 	return 3*latdiff * latdiff;
		case 5: 	return 3*londiff * londiff;
		default:	return 0;
	}
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

	double recLatDeg = obs.ippMap[0].latDeg;
	double recLonDeg = obs.ippMap[0].lonDeg;

	switch (basis.type)
	{
		case +E_BasisType::POLYNOMIAL: return ionCoefPolynomial (trace, atmReg, recLatDeg, recLonDeg, basis.index);
		case +E_BasisType::GRIDPOINT:
		{
			if (atmReg.gridType == 0)
				return IrregGridCoef(ind, recLatDeg, recLonDeg);

			double dlatDeg = fabs(recLatDeg - atmReg.gridLatDeg[basis.index]);
			double dlonDeg = fabs(recLonDeg - atmReg.gridLonDeg[basis.index]);

			if (dlatDeg > atmReg.intLatDeg || atmReg.intLatDeg == 0)		return 0;
			if (dlonDeg > atmReg.intLonDeg || atmReg.intLonDeg == 0)		return 0;

			tracepdeex (4, trace, "\nGridded basis: %.4f, %.4f", atmReg.gridLatDeg[basis.index], atmReg.gridLonDeg[basis.index]);

			return (1 - dlatDeg / atmReg.intLatDeg) * (1 - dlonDeg / atmReg.intLonDeg);		//todo aaron use bilinear interpolation function?
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
	//Discard old data
	for (auto& [regID,regData] : nav.ssrAtm.atmosRegionsMap)
	for (auto& [sat  ,satData] : regData.stecData)
	for (auto it = satData.begin(); it != satData.end();)
	{
		double dt = (kfState.time - it->first).to_double();
		if(abs(dt) > acsConfig.ssrInOpts.local_stec_valid_time)
			it = satData.erase(it);
		else
			it++;
	}

	// Copy new data
	for (auto [key, index] : kfState.kfIndexMap)
	{
		if (key.type != KF::IONOSPHERIC)
			continue;

		auto& basis				= localBasisVec[key.num];
		auto& atmReg			= nav.ssrAtm.atmosRegionsMap[basis.regionID];
		atmReg.stecUpdateTime	= kfState.time;
		auto& stecRecord		= atmReg.stecData[basis.Sat][kfState.time];

		switch (basis.type)
		{
			case E_BasisType::POLYNOMIAL:
			{
				double val;
				double var;
				kfState.getKFValue(key, val, &var);

				double fact = 1;
				double latFact = (atmReg.maxLatDeg-atmReg.minLatDeg);
				double lonFact = (atmReg.maxLonDeg-atmReg.minLonDeg);
				switch (basis.index)
				{
					case 0: 								break;
					case 1: 	fact = latFact/2;			break;
					case 2: 	fact = lonFact/2;			break;
					case 3: 	fact = latFact*lonFact/4;	break;
					case 4: 	fact = latFact*latFact/3;	break;
					case 5: 	fact = lonFact*lonFact/3;	break;
					default:	var  = 1e6;					break;
				}

				if (var > 100*SQR(acsConfig.ssrOpts.max_stec_sigma))
				{
					stecRecord.poly[basis.index] = -9999;
					stecRecord.sigma = 1e6;
				}
				else
				{
					stecRecord.poly[basis.index] = fact * val;
					if (stecRecord.sigma < 1e6)
						stecRecord.sigma = acsConfig.ssrOpts.max_stec_sigma;
				}

				break;
			}
			case E_BasisType::GRIDPOINT:
			{
				double val;
				double var;
				kfState.getKFValue(key, val,&var);

				double sigma = sqrt(var);
				if (sigma < acsConfig.ssrOpts.max_stec_sigma)
				{
					stecRecord.grid[basis.index] = val;

					if (sigma > stecRecord.sigma)
						stecRecord.sigma = sigma;
				}
				break;
			}
		}
	}

	// Divide between gridmap and polynomial, if necessary
	for (auto& [regID, regData] : nav.ssrAtm.atmosRegionsMap)
	{
		if (regData.gridType < 0)		continue;
		if (!regData.ionoGrid)			continue;
		if (regData.ionoPolySize <= 0)	continue;

		// Grid plus polynomial corrections
		for (auto& [sat, satData] : regData.stecData)
		{
			if (satData.find(kfState.time) == satData.end())
				continue;
			auto& stecData	= satData[kfState.time];

			stecData.poly.clear();

			int ngrid = stecData.grid.size();
			if ( ngrid <= 1)
				continue;

			int npoly = (regData.ionoPolySize < (ngrid - 1)) ? regData.ionoPolySize : (ngrid - 1);
			if (npoly == 2)		npoly = 1;
			if (npoly == 5)		npoly = 4;

			VectorXd y = VectorXd::Zero(ngrid);
			MatrixXd H = MatrixXd::Zero(ngrid, npoly);

			int nind = 0;
			map <int,int> gridMap;
			for (auto& [ind, stec] : stecData.grid)
			{
				y(nind)=stec;
				H(nind, 0)	= 1;
				if (npoly == 1)
					continue;

				double dLat = regData.gridLatDeg[ind] - regData.gridLatDeg[0];
				double dLon = regData.gridLonDeg[ind] - regData.gridLonDeg[0];
				H(nind, 1)	= dLat;
				H(nind, 2)	= dLon;
				if (npoly < 4)
					continue;

				H(nind, 3)	= dLat * dLon;
				if (npoly < 6)
					continue;

				H(nind, 4)	= dLat * dLat;
				H(nind, 5)	= dLon * dLon;

				gridMap[ind] = nind;
				nind++;
			}

			MatrixXd Q = (H.transpose()*H).inverse();
			VectorXd x = Q * H.transpose() * y;
			VectorXd v = y - H * x;

			for (int i=0; i < npoly; i++)
				stecData.poly[i] = x[i];

			for (auto& [ind, stec] : stecData.grid)
				stec = v[gridMap[ind]];
		}
	}
}

bool getCmpSSRIono(
	Trace&		trace,		///< Debug trace
	GTime		time,		///< GPS time
	SSRAtm&		ssrAtm,		///< SSR Atmospheric corrections
	Vector3d&	rRec,		///< receiver position
	double& 	iono,		///< ionoapheric delay (in TECu)
	double&		var,		///< ionoapheric delay (in TECu^2)
	SatSys	 	Sat)		///< Satellite
{
	GObs obs;
	obs.Sat = Sat;
	iono	= 0;
	var		= 0;

	VectorPos pos = ecef2pos(rRec);
	map<int,double> numer;
	map<int,double> acmVar;
	double denom=0;
	for (auto& [regID,regData] : ssrAtm.atmosRegionsMap)
	{
		if (regData.stecData.find(Sat) == regData.stecData.end())
			continue;
		auto& satData = regData.stecData[Sat];

		GTime tAtm;
		for (auto it = satData.begin(); it != satData.end();)
		{
			GTime tim = it->first;
			double dt = (time-tim).to_double();
			if ( dt > acsConfig.ssrInOpts.local_stec_valid_time)
				it = satData.erase(it);
			else
			{
				if (tAtm == GTime::noTime()
				|| abs(dt) < abs((time-tAtm).to_double()))
					tAtm = tim;
				it++;
			}
		}

		if (tAtm == GTime::noTime())
			continue;

		auto& stecData = satData[tAtm];

		double recLonDeg = pos.lonDeg();
		double midLonDeg = (regData.minLonDeg + regData.maxLonDeg) / 2;
		if		((recLonDeg - midLonDeg) >  180)		recLonDeg -= 360;
		else if	((recLonDeg - midLonDeg) < -180)		recLonDeg += 360;

		if (stecData.grid.size() <= 1)		// No grid Map data
		{
			if (stecData.poly.size() <= 0)					continue;
			if (pos.latDeg()	> regData.maxLatDeg)		continue;
			if (pos.latDeg()	> regData.minLatDeg)		continue;
			if (recLonDeg		> regData.maxLonDeg)		continue;
			if (recLonDeg		< regData.minLonDeg)		continue;

			var  = stecData.sigma;
			iono = stecData.poly[0];

			double dLat = pos.latDeg()	- regData.gridLatDeg[0];
			double dLon = recLonDeg		- regData.gridLonDeg[0];

			if (stecData.poly.size() > 2)
			{
				iono += stecData.poly[1] * dLat;
				iono += stecData.poly[2] * dLon;
			}

			if (stecData.poly.size() > 3)
				iono += stecData.poly[3] * dLat * dLon;

			if (stecData.poly.size() > 5)
			{
				iono += stecData.poly[4] * dLat * dLat;
				iono += stecData.poly[5] * dLon * dLon;
			}
			return true;
		}

		if (regData.intLatDeg <= 0)			continue;
		if (regData.intLonDeg <= 0)			continue;


		if (stecData.poly.size() > 0)
		{
			for (auto& [ind, stec] : stecData.grid)
			{
				if (stec < -9000)
					continue;

				double dLat = regData.gridLatDeg[ind] - regData.gridLatDeg[0];
				double dLon = regData.gridLonDeg[ind] - regData.gridLonDeg[0];

				stec += stecData.poly[0];

				if (stecData.poly.size() > 2)
				{
					stec += stecData.poly[1] * dLat;
					stec += stecData.poly[2] * dLon;
				}
				if (stecData.poly.size() > 3)
					stec += stecData.poly[3] * dLat * dLon;

				if (stecData.poly.size() > 5)
				{
					stec += stecData.poly[4] * dLat * dLat;
					stec += stecData.poly[5] * dLon * dLon;
				}
			}
			stecData.poly.clear();
		}

		for (auto& [ind, stec] : stecData.grid)
		{
			if (stec < -9000)
				continue;

			double dLat = abs(pos.latDeg()	- regData.gridLatDeg[ind]) / regData.intLatDeg;
			double dLon = abs(recLonDeg		- regData.gridLonDeg[ind]) / regData.intLonDeg;

			if (dLat >= 1)	continue;
			if (dLon >= 1)	continue;

			// tracepdeex(2, std::cout,"Iono grid %s %2d, %2d:  %.4f %.4f,  %.4f\n", Sat.id().c_str(), regID, ind, dLat, dLon, stec);
			double coef = (1-dLat)*(1-dLon);

			acmVar	[64 * regID + ind]	= SQR(	coef * stecData.sigma);
			numer	[64 * regID + ind] 	= 		coef * stec;
			denom += coef;
		}
	}

	if (numer.size() <= 0)
		return false;

	for (auto [ind, val] : numer)
	{
		iono += val;
		var  += acmVar[ind];
	}
	iono /= denom;
	var  /= SQR(denom);

	return true;
}
