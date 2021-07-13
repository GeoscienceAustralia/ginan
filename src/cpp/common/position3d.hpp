
#ifndef __POSITION3D_HPP__
#define __POSITION3D_HPP__


#include "eigenIncluder.hpp"
#include "common.hpp"


struct Position3D
{
	Vector3d R;
	Vector3d llhvec;

	Position3D()
	{

	}

	Position3D(Vector3d& ecef)
	{
		R = ecef;
	}

	Position3D(double* ecef)
	{
		for (int i = 0; i < 3; i++)
		{
			R(i) = ecef[i];
		}
	}

	operator Vector3d&()
	{
		return R;
	}

	operator Vector3d() const
	{
		return R;
	}

	double* llh()
	{
		ecef2pos(R.data(), llhvec.data());
		return llhvec.data();
	}
};


#endif
