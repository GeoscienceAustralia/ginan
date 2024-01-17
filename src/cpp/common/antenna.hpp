
#pragma once


#include <string>
#include <vector>
#include <map>

using std::string;
using std::vector;
using std::map;

#include "eigenIncluder.hpp"
#include "azElMapData.hpp"
#include "satStat.hpp"
#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"

struct PhaseCenterData : AzElMapData<double>
{
	/* antenna parameter type */
	E_FType	ft;
	string	type;					///< antenna type
	string	code;					///< serial number or satellite code
	string	svn;					///< SVN in satellites
	string	cospar;					///< Cospar code satellites
	string	calibModel;				///< name of the antenna calibration model

	double tf[6];					///< valid from YMDHMS
	double tu[6];					///< valid until YMDHMS
};

struct PhaseCenterOffset
{
	Vector3d	satPco = Vector3d::Zero();
	Vector3d	recPco = Vector3d::Zero();
};

//forward declaration for pointer below
struct SatSys;
struct AttStatus;
struct Navigation;

VectorEcef satAntOff(
	Trace&				trace,
	GTime				time,
	AttStatus&			attStatus,
	SatSys& 			Sat,
	map<int, double>&	lamMap);

Vector3d antPco(
	string		id,
	E_Sys		sys,
	E_FType		ft,
	GTime		time,
	double&		var,
	E_Radio		radio,
	bool		interp = false);

double antPcv(
	string		id,
	E_Sys		sys,
	E_FType		ft,
	GTime		time,
	AttStatus&	attStatus,
	VectorEcef	e,
	double*		az_ptr	= nullptr,
	double*		zen_ptr	= nullptr);

bool findAntenna(
	string				code,
	E_Sys				sys,
	GTime				time,
	Navigation&			nav,
	E_FType				ft,
	PhaseCenterData**	pcd_ptr_ptr = nullptr);

void readantexf(
	string		file,
	Navigation&	nav);

void radome2none(
	string& antenna_type);

