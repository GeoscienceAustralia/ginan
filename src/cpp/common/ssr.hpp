
#pragma once

#include <set>

#include "eigenIncluder.hpp"
#include "gTime.hpp"
#include "trace.hpp"
// #include "gMap.hpp"

#define SSR_UNAVAILABLE		-9999

struct SatPos;

const double uraSsr[] =
{
	0,
	0.25,
	0.5,
	0.75,
	1,
	1.25,
	1.5,
	1.75,
	2,
	2.75,
	3.5,
	4.25,
	5,
	5.75,
	6.5,
	7.25,
	8,
	10.25,
	12.5,
	14.75,
	17,
	19.25,
	21.5,
	23.75,
	26,
	32.75,
	39.5,
	46.25,
	53,
	59.75,
	66.5,
	73.25,
	80,
	100.25,
	120.5,
	140.75,
	161,
	181.25,
	201.5,
	221.75,
	242,
	302.75,
	363.5,
	424.25,
	485,
	545.75,
	606.5,
	667.25,
	728,
	910.25,
	1092.5,
	1274.75,
	1457,
	1639.25,
	1821.5,
	2003.75,
	2186,
	2732.75,
	3279.5,
	3826.25,
	4373,
	4919.75,
	5466.5,
	6013.25
};

// SSR message metadata
struct SSRMeta
{
	int				epochTime1s		= 0;
	GTime			receivedTime	= {};
	int				updateIntIndex	= -1;
	int				multipleMessage	= 0;
	unsigned int	referenceDatum	= 0;
	unsigned int	provider		= 0;
	unsigned int	solution		= 0;
	unsigned int	numSats			= 0;
};

struct SSREph
{
	SSRMeta 	ssrMeta	= {};
	GTime		t0		= {};
	double		udi		=  0;				///< update interval
	int			iod		= -1;
	int			iode	= -1;				///< issue of data
	int			iodcrc	= -1;
	Vector3d	deph 	= Vector3d::Zero();	///<     delta orbit {radial,along,cross} (m)
	Vector3d	ddeph	= Vector3d::Zero();	///< dot delta orbit {radial,along,cross} (m/s)
};

struct SSRClk
{
	SSRMeta 	ssrMeta	= {};
	GTime		t0		= {};
	double		udi		= 0;				///< update interval
	int			iod		= -1;
	double		dclk[3]	= {};				///< delta clock {c0,c1,c2} (m,m/s,m/s^2)
};

struct SSRUra
{
	SSRMeta 	ssrMeta	= {};
	GTime		t0		= {};
	double		udi		= 0;				///< update interval
	int			iod		= -1;
	int			ura		= 0;				///< URA indicator
};

struct SSRHRClk
{
	SSRMeta 	ssrMeta	= {};
	GTime		t0		= {};
	double		udi		= 0;				///< update interval
	int			iod		= -1;
	double		hrclk	= 0;				///< high-rate clock corection (m)
};

struct BiasVar
{
	double		bias	= 0;				///< biases (m)
	double		var		= 0;				///< biases variance (m^2)
};

struct SSRBias
{
	SSRMeta			ssrMeta	= {};
	GTime			t0		= {};
	double			udi		= 0;				///< update interval
	int				iod		= -1;
	unsigned int	nbias	= 0;
	map<E_ObsCode, BiasVar> obsCodeBiasMap;
	map<int, double> ionDCBOffset;
};

struct SSRCodeBias : SSRBias
{

};

struct SSRPhase
{
	int				dispBiasConistInd	= -1;
	int				MWConistInd			= -1;
	double			yawAngle			= 0;
	double			yawRate				= 0;
};

struct SSRPhaseCh
{
	unsigned int	signalIntInd		= -1;
	unsigned int	signalWLIntInd		= -1;
	unsigned int	signalDisconCnt		= -1;
};

struct SSRPhasBias : SSRBias
{
	SSRPhase	ssrPhase; 					///< Additional data for SSR phase messages
	map<E_ObsCode, SSRPhaseCh> ssrPhaseChs;	///< Additional data for SSR phase messages, for each channel
};

struct SphComp
{
	int			layer;
	int			order;
	int			degree;
	E_TrigType	trigType;
	double		value;
	double		variance;
};

struct SSRVTEClayer
{
	double height	= 0;
	int maxOrder	= 0;
	int maxDegree	= 0;
	map<int, SphComp> sphHarmonic;
};

struct SSRAtmGlobal
{
	GTime					time;
	int 					numberLayers;
	map<int, SSRVTEClayer>	layers;
	double					vtecQuality;
	int 					iod = -1;
};

struct SSRSTECData
{
	int 				iod		= -1;
	double				sigma	= 0.1;				/* STEC maps accuracy in TECu */
	map<int, double>	poly;						/* STEC polynomials in TECu (deg) */
	map<int, double>	grid;						/* STEC gridmaps in TECu */
};

struct SSRTropData
{
	double				sigma = 0;
	map<int, double>	polyDry;						/* ZHD in meters (deg) */
	map<int, double>	gridDry;						/* ZHD in meters */
	map<int, double>	gridWet;						/* ZWD in meters */
};

struct SSRAtmRegion
{
	int regionDefIOD		= -1;
	map<int, double>	gridLatDeg;
	map<int, double>	gridLonDeg;

	double	minLatDeg		= 0;
	double	maxLatDeg		= 0;
	double	intLatDeg		= 0;

	double	minLonDeg		= 0;
	double	maxLonDeg		= 0;
	double	intLonDeg		= 0;

	int		gridType		= -1;
	int		tropPolySize	= -1;
	int		ionoPolySize	= -1;
	bool	ionoGrid		= false;
	bool	tropGrid		= false;

				map<GTime, SSRTropData, std::greater<GTime>>	tropData;
	map<SatSys,	map<GTime, SSRSTECData, std::greater<GTime>>>	stecData;
	GTime	stecUpdateTime;
};

struct SSRAtm
{
	SSRMeta											ssrMeta;
	map<GTime,	SSRAtmGlobal, std::greater<GTime>>	atmosGlobalMap;
	map<int,	SSRAtmRegion>						atmosRegionsMap;
};

struct EphValues
{
	GTime			time;
	unsigned int	iode	= -1;
	Vector3d		brdcPos	= Vector3d::Zero();
	Vector3d		brdcVel	= Vector3d::Zero();
	Vector3d		precPos	= Vector3d::Zero();
	Vector3d		precVel	= Vector3d::Zero();

	double			ephVar	= 0;
};

struct ClkValues
{
	GTime			time;
	unsigned int	iode	= -1;
	double			brdcClk	= 0;
	double			precClk	= 0;
};

struct SSREphInput
{
	bool		valid = false;
	EphValues	vals[2];
};

struct SSRClkInput
{
	bool		valid = false;
	ClkValues	vals[2];
};

/* SSR correction type */
struct SSRMaps
{
	map<GTime, SSRCodeBias,		std::greater<GTime>>	ssrCodeBias_map;
	map<GTime, SSRPhasBias,		std::greater<GTime>>	ssrPhasBias_map;
	map<GTime, SSRClk,			std::greater<GTime>>	ssrClk_map;
	map<GTime, SSREph,			std::greater<GTime>>	ssrEph_map;
	map<GTime, SSRHRClk,		std::greater<GTime>>	ssrHRClk_map;
	map<GTime, SSRUra,			std::greater<GTime>>	ssrUra_map;

	int refd_;					///< sat ref datum (0:ITRF,1:regional)
	unsigned char update_;		///< update flag (0:no update,1:update)
};

struct SSROut
{
	GTime			epochTime;

	SSRPhasBias		ssrPhasBias;
	SSRCodeBias		ssrCodeBias;

	SSRClkInput		clkInput;
	SSREphInput		ephInput;

	SSRClk			ssrClk;
	SSREph			ssrEph;

	SSRHRClk		ssrHRClk;
	SSRUra			ssrUra;

	bool ephUpdated		= false;
	bool clkUpdated		= false;
	bool hrclkUpdated	= false;
	bool phaseUpdated	= false;
	bool codeUpdated	= false;
	bool uraUpdated		= false;
};

struct KFState;

int uraToClassValue(double ura);
double ephVarToUra(double ephVar);

void prepareSsrStates(
	Trace&				trace,
	KFState&			kfState,
	KFState&			ionState,
	GTime				time);

void writeSsrOutToFile(
	int					epochNum,
	std::set<SatSys>	sats);


bool ssrPosDelta(
	GTime			time,
	GTime			ephTime,
	SatPos&			satPos,
	const SSRMaps&	ssrMaps,
	Vector3d&		dPos,
	int&			iodPos,
	int&			iodEph,
	GTime&			validStart,
	GTime&			validStop);

bool ssrClkDelta(
	GTime			time,
	GTime			ephTime,
	SatPos&			satPos,
	const SSRMaps&	ssrMaps,
	double&			dclk,
	int&			iodClk,
	GTime&			validStart,
	GTime&			validStop);


