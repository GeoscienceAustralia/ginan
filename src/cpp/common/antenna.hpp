
#ifndef ANTENNA_H
#define ANTENNA_H


#include <string>
#include <vector>
#include <list>
#include <map>

using std::string;
using std::vector;
using std::list;
using std::map;

#include "eigenIncluder.hpp"


#include "streamTrace.hpp"
#include "constants.h"
#include "gTime.hpp"
#include "enums.h"



typedef map<E_FType, Vector3d> PcoMapType;

struct pcvacs_t
{
	/* antenna parameter type */
	int nf;						/* number of frequencies */
	string type;				/* antenna type */
	string code;				/* serial number or satellite code */
	string svn;					/* SVN in satellites */
	string cospar;				/* Cospar code satellites */
	double aziDelta;			/* azimuth increment (degree) */
	double zenStart;
	double zenStop;
	double zenDelta;
	int nz;						/* number of zenith intervals */
	int naz;					/* number of non-azimuth dependent intervals */

	double tf[6];				/* valid from YMDHMS */
	double tu[6];				/* valid until YMDHMS */
	PcoMapType			pcoMap;			/* phase centre offsets (m) */
	map<int, 			vector<double>>		PCVMap1D;
	map<int, map<int,	vector<double>>>	PCVMap2D;
};
typedef list<pcvacs_t> PcvList;


//forward declaration for pointer below
struct SatNav;
struct SatSys;
struct nav_t;

void satantoff(
	Trace&		trace,
	GTime		time,
	Vector3d&	rs,
	SatSys&		Sat,
	SatNav*		satNav_ptr,
	Vector3d&	dant,
	PcoMapType*	pcoMap_ptr);

void recpcv(pcvacs_t *pc, int freq, double el, double azi, double& pcv);
void recpco(pcvacs_t *pc, int freq, Vector3d& pco);
void satpcv(pcvacs_t *pc, double nadir, double *pcv);


pcvacs_t* findAntenna(
	string		code, 
	double		tc[6],
	nav_t&		nav);

int readantexf(
	string file, 
	nav_t& nav);

void radome2none(
	string& antenna_type);

void interp_satantmodel(
	pcvacs_t&			pcv,
	double				nadir,
	map<int, double>&	dAntSat);

#endif
