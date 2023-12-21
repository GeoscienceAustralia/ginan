
#pragma once

#include "eigenIncluder.hpp"

struct OrbitOptions;

Vector3d  applyBoxwingSrp(
	const OrbitOptions&	orbitOptions,
	const Vector3d&		eD,
	const Vector3d&		eX,
	const Vector3d&		eY,
	const Vector3d&		eZ);

