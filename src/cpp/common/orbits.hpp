
#pragma once


#include "eigenIncluder.hpp"
#include "trace.hpp"

VectorEci keplers2Inertial(
			Trace&		trace,
	const	Vector6d&	keplers0);

bool inertial2Keplers(
			Trace&		trace,
	const	VectorEci&	r,
	const	VectorEci&	v,
			Vector6d&	keplers);

VectorEci propagateEllipse(
			Trace&		trace,
			GTime		time,
			double		dt, 
			VectorEci&	rSat,
			VectorEci&	vSat, 
			VectorEcef&	ecef, 
			VectorEcef*	vSatEcef_ptr = nullptr);
