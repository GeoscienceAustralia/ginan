
#ifndef __PPP_HPP__
#define __PPP_HPP__

#include <unordered_map>
#include <map>

using std::unordered_map;
using std::map;


#include "algebra.hpp"
#include "satStat.hpp"
#include "gTime.hpp"
#include "ppp.hpp"

//forward declarations
struct Station;
struct vmf3_t;
struct gptgrid_t;


/** Solution of user mode processing functinos
*/
struct Solution
{
	/* solution type */
	GTime				time;       							///< time (GPST)
	map<int, double>	dtRec_m; 								///< receiver clock bias to time systems (m)
	map<int, double>	dtRec_m_ppp_old; 						///< previous receiver clock bias to time systems (m)
	map<int, double>	deltaDt_net_old;						///< previous receiver clock bias to time systems (m)
	map<int, double>	pppdtRec_m;								///< receiver clock bias to time systems (s)
	int					stat;									///< solution status (SOLQ_???)
	int					numSats;								///< number of valid satellites
	KFState				sppState;								///< SPP filter object
	Vector3d			sppRRec			= Vector3d::Zero();		///< Position vector from spp
	Vector3d			pppRRec			= Vector3d::Zero();		///< Position vector from ppp
};

/**	Legacy processing options from rtklib
*/
struct prcopt_t
{
	Vector3d	antdel		= Vector3d::Zero();		///< antenna delta {rov_e,rov_n,rov_u}
	string		anttype; 							///< antenna type
	double		odisp[2][6*11] = {};				///< ocean tide loading parameters {rov,base} */	//todo aaron, check orientation
};

struct rtk_t
{
	KFState		pppState;					///< RTK control/result type
	Solution	sol;						///< RTK solution
	double		tt;							///< time difference between current and previous (s)
	pcvacs_t*	pcvrec = nullptr;
	unordered_map<SatSys, SatStat> satStatMap;
	prcopt_t	opt;						///< processing options
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
	rtk_t&		rtk,
	ObsList&	obsList,
	Station&	refstat);

void pppoutstat(
	Trace&		trace,
	KFState&	kfState,
	bool		rts = false,
	int			stat= 0,
	int			nsat= 0);

void pppomc(
	Trace&		trace,
	rtk_t&		rtk,
	ObsList&	obsList,
	gptgrid_t&	gptg,
	ClockJump&	cj,
	Station&	rec,
	vmf3_t*		vmf3,
	double*		orog);

/* standard positioning ------------------------------------------------------*/
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
	rtk_t&		rtk,
	Station&	rec);

void corr_meas(
	Trace&		trace,
	Obs&		obs,
	E_FType		ft,
	double		el,
	double		dAntRec,
	double		dAntSat,
	double		phw,
	ClockJump&	cj,
	Station&	rec);

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

int model_trop(
	GTime		time,
	double*		pos,
	double*		azel,
	double*		tropStates,
	double*		dTropDx,
	double&		dTrp,
	double&		var);

int model_iono(
	GTime		time,
	double*		pos,
	double*		azel,
	double		ionoState,
	double&		dion,
	double&		var);

void satantpcv(
	Vector3d&			rs,
	Vector3d&			rr,
	pcvacs_t&			pcv,
	map<int, double>&	dAntSat,
	double*				nad);

void selectAprioriSource(
	Station&	rec,
	bool&		sppUsed);

bool deweightMeas(
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

#endif
