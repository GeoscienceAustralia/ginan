
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
#include "gTime.hpp"
#include "enums.h"


struct PhaseCenterData
{
	/* antenna parameter type */
	E_FType	ft;
	string	type;					///< antenna type 
	string	code;					///< serial number or satellite code 
	string	svn;					///< SVN in satellites 
	string	cospar;					///< Cospar code satellites 
	string	calibModel;				///< name of the antenna calibration model 
	double	aziDelta;				///< azimuth increment (degree) 
	double	zenStart;
	double	zenStop;
	double	zenDelta;
	int		nz;						///< number of zenith intervals 
	int		naz;					///< number of non-azimuth dependent intervals 

	double tf[6];					///< valid from YMDHMS 
	double tu[6];					///< valid until YMDHMS 
	
				vector<double>	PCVMap1D;
	map<int,	vector<double>>	PCVMap2D;
};

//forward declaration for pointer below
struct SatSys;
struct nav_t;

void satAntOff(
	Trace&				trace,
	GTime				time,
	Vector3d&			rSat,
	SatSys& 			Sat,
	map<int, double>&	lamMap,
	Vector3d&			dAnt,
	SatStat*			satStat_ptr = nullptr);
	
Vector3d satAntOff(
	Trace&				trace,
	GTime				time,
	Vector3d&			rSat,
	SatSys& 			Sat,
	E_FType 			ft,
	SatStat*			satStat_ptr = nullptr);

Vector3d antPco(
	string		id,
	E_FType		ft,
	GTime		time,
	bool		interp = false);

double antPcv(
	string		id,
	E_FType		ft,
	GTime		time,
	double		aCos,
	double		azi = 0);



bool findAntenna(
	string				code,
	GTime				time,
	nav_t&				nav,
	E_FType				ft,
	PhaseCenterData**	pcd_ptr_ptr = nullptr);

int readantexf(
	string file, 
	nav_t& nav);

void radome2none(
	string& antenna_type);

#endif
