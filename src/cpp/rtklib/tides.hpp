
#pragma once

#include "eigenIncluder.hpp"
#include "gTime.hpp"
#include "trace.hpp"


//forward declaration
struct ERP;

void tideDisp(
	Trace&			trace,
	GTime			time,
	Vector3d&		recPos,
	ERP&			erp,
	const double*	otlDisplacement,
	Vector3d&		dr,
	Vector3d*		solid_ptr	= nullptr,
	Vector3d*		olt_ptr		= nullptr,
	Vector3d*		pole_ptr	= nullptr);


