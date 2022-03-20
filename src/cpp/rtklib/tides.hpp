
#ifndef __TIDES_HPP__
#define __TIDES_HPP__

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "gTime.hpp"


//forward declaration
struct ERP;

/* earth tide models ---------------------------------------------------------*/

void sunmoonpos_eci(
	GTime			tut,
	Vector3d*		rsun_ptr,
	Vector3d*		rmoon_ptr);

void sunmoonpos(
	GTime			tutc,
	ERPValues&		erpv,
	Vector3d*		rsun	= nullptr,
	Vector3d*		rmoon	= nullptr,
	double*			gmst	= nullptr);

void tidedisp(
	Trace&			trace,
	GTime			tutc,
	Vector3d&		recPos,
	ERP&			erp,
	const double*	otlDisplacement,
	Vector3d&		dr,
	Vector3d*		solid_ptr	= nullptr,
	Vector3d*		olt_ptr		= nullptr,
	Vector3d*		pole_ptr	= nullptr);



#endif
