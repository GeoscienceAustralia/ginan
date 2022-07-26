
#ifndef __PPP_HPP__
#define __PPP_HPP__

#include <map>

using std::map;



#include "algebra.hpp"
#include "satStat.hpp"
#include "gTime.hpp"

//forward declarations
struct Station;
struct vmf3_t;
struct gptgrid_t;
struct PhaseCenterData;
using StationMap = map<string, Station>;




/** Solution of user mode processing functinos
*/
struct Solution
{
	/* solution type */
	GTime				time;       							///< time (GPST)
	map<int, double>	dtRec_m; 								///< receiver clock bias to time systems (m)
	map<int, double>	dtRec_m_ppp_old; 						///< previous receiver clock bias to time systems (m)
	map<int, double>	dtRec_m_pppp_old; 						///< previous receiver clock bias to time systems (m)
	map<int, double>	deltaDt_net_old;						///< previous receiver clock bias to time systems (m)
	map<int, double>	pppdtRec_m;								///< receiver clock bias to time systems (s)
	int					stat;									///< solution status (SOLQ_???)
	int					numSats;								///< number of valid satellites
	KFState				sppState;								///< SPP filter object
	double				dop[4];
	Vector3d			sppRRec			= Vector3d::Zero();		///< Position vector from spp
	Vector3d			pppRRec			= Vector3d::Zero();		///< Position vector from ppp
	
};

void removeUnmeasuredAmbiguities(
	Trace&				trace,
	KFState&			kfState,
	map<KFKey, bool>	measuredStates);

struct KalmanModel;
struct StationOptions;

InitialState initialStateFromConfig(
	KalmanModel&	kalmanModel,
	int				index = 0);

/* precise point positioning -------------------------------------------------*/
void pppos(
	Trace&		trace,
	ObsList&	obsList,
	Station&	rec);

void pppoutstat(
	Trace&		trace,
	KFState&	kfState);

void pppomc(
	Trace&		trace,
	ObsList&	obsList,
	gptgrid_t&	gptg,
	Station&	rec,
	vmf3_t*		vmf3,
	double*		orog);

void sppos(
	Trace&		trace,
	ObsList&	obsList,
	Solution&	sol);

void testeclipse(
	ObsList&	obsList);

void pppCorrections(
	Trace&		trace,
	ObsList&	obsList,
	Vector3d&	rRec,
	Station&	rec);
	
void PPP(
	Trace&			trace,		
	StationMap&		stations,	
	KFState&		kfState,	
	gptgrid_t&		gptg,		
// 	vmf3_t*			vmf3,		
	double*			orography);	

void corr_meas(
	Trace&		trace,
	Obs&		obs,
	E_FType		ft,
	double		dAntRec,
	double		dAntSat,
	double		phw,
	Station&	rec,
	double		mjd,
	bool		oldSchool = true);

double sbstropcorr(
	GTime			time,
	Vector3d&		rRec,
	double			el,
	double*			var = nullptr);

double measVar(
	int				sys,
	double			el,
	E_FType			ft,
	int				type,
	StationOptions&	stationOpts);

int model_phw(
	GTime		time,
	Obs&		obs,
	Vector3d&	rRec,
	double&		phw);

double satNadir(
	Vector3d&			rs,
	Vector3d&			rr);

double trop_model_prec(
	GTime		time,
	double*		pos,
	double*		azel,
	double*		tropStates,
	double*		dTropDx,
	double&		var);

int model_iono(
	GTime		time,
	double*		pos,
	double*		azel,
	double		ionoState,
	double&		dion,
	double&		var);

void outputDeltaClocks(
	StationMap& stationMap);

void outputApriori(
	StationMap& stationMap);

void outputPPPSolution(
	string		filename,
	Station&	rec);

void selectAprioriSource(
	Station&	rec,
	bool&		sppUsed);

void postFilterChecks(
	KFMeas&	kfMeas);

bool deweightMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool deweightStationMeas(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool countSignalErrors(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool incrementPhaseSignalError(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	int			index);

bool resetPhaseSignalError(
	KFMeas&		kfMeas,
	int			index);

bool resetPhaseSignalOutage(
	KFMeas&		kfMeas,
	int			index);

bool deweightByState(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey);

bool clockGlitchReaction(
	Trace&		trace,
	KFState&	kfState,
	KFMeas&		kfMeas,
	KFKey&		kfKey);




void stationPPP(
			Trace&				netTrace,		
			Station&			rec,			
/*	const*/	KFState&			kfState,		
			KFMeasEntryList&	kfMeasEntryList,
			gptgrid_t&			gptg,			
// 			vmf3_t*				vmf3,			
			double*				orography);		


void stationPseudo(
			Trace&				netTrace,			
			Station&			rec,				
/*	const*/	KFState&			kfState,			
			KFMeasEntryList&	kfMeasEntryList);	

Matrix3d stationEopPartials(
	Vector3d&	rRec);

#endif
