
#pragma once

#include <array>

using std::array;

#include "centerMassCorrections.hpp"
#include "eigenIncluder.hpp"
#include "constants.hpp"
#include "attitude.hpp"
#include "iers2010.hpp"
#include "gTime.hpp"
#include "erp.hpp"
#include "sofam.h"
#include "sofa.h"


struct Sofa
{
	static void		iauXys	(MjDateTT	mjdTT, double& x, double& y, double& s)		{			iauXys00a	(DJ00, mjdTT.	to_j2000(), &x, &y, &s);						}
	static double	iauGmst	(MjDateUt1	mjdUt1,	MjDateTT mjdTT)						{	return	iauGmst06	(DJ00, mjdUt1.	to_j2000(), DJ00, mjdTT.	to_j2000());		}
	static double	iauGmst	(MjDateUt1	mjdUt1)										{	return	iauGmst06	(DJ00, mjdUt1.	to_j2000(), DJ00, mjdUt1.	to_j2000());		}
	static double	iauEra	(MjDateUt1	mjdUt1)										{	return	iauEra00	(DJ00, mjdUt1.	to_j2000());									}
	static double	iauSp	(MjDateTT	mjdTT)										{	return	iauSp00		(DJ00, mjdTT.	to_j2000());									}
	static double	iauFal	(MjDateTT	mjdTT)										{	return	iauFal03	(mjdTT.to_j2000() / 365.25 / 100);	}
	static double	iauFalp	(MjDateTT	mjdTT)										{	return	iauFalp03	(mjdTT.to_j2000() / 365.25 / 100);	}
	static double	iauFaf	(MjDateTT	mjdTT)										{	return	iauFaf03	(mjdTT.to_j2000() / 365.25 / 100);	}
	static double	iauFad	(MjDateTT	mjdTT)										{	return	iauFad03	(mjdTT.to_j2000() / 365.25 / 100);	}
	static double	iauFaom	(MjDateTT	mjdTT)										{	return	iauFaom03	(mjdTT.to_j2000() / 365.25 / 100);	}
	static int		iauEpv	(MjDateTT	mjdTT, double pvh[2][3], double pvb[2][3])	{	return	iauEpv00	(DJ00, mjdTT.	to_j2000(), pvh, pvb);	}
	static void		iauMoon	(MjDateTT	mjdTT, double pv [2][3])					{	return	iauMoon98	(DJ00, mjdTT.	to_j2000(), pv);		}
};

struct XFormData
{
	double xp_pm	= 0;
	double yp_pm	= 0;
	double ut1_pm	= 0;
	double lod_pm	= 0;
	double xp_o		= 0;
	double yp_o		= 0;
	double ut1_o	= 0;
	double sp		= 0;
	double era		= 0;
};

void eci2ecef(
	GTime				time,
	const ERPValues&	erpVal,
	Matrix3d&			U,
	Matrix3d*			dU_ptr = nullptr);

void pos2enu(
	const	VectorPos&	pos,
			double*		E);

VectorEnu	ecef2enu(
	const	VectorPos&	pos,
	const	VectorEcef&	r);

VectorEcef	enu2ecef(
	const	VectorPos&	pos,
	const	VectorEnu&	e);

Matrix3d rotBasisMat(
	Vector3d&	eX,
	Vector3d&	eY,
	Vector3d&	eZ);


VectorPos	ecef2pos(
	const VectorEcef&	r);

VectorEcef	pos2ecef(
	const VectorPos&	pos);

VectorEcef	body2ecef(
	const	AttStatus&	attStatus,
	const	Vector3d&	rBody);

Vector3d ecef2body(
	AttStatus&	attStatus,
	VectorEcef&	ecef,
	MatrixXd*	dEdQ_ptr = nullptr);



struct FrameSwapper
{
	static array<FrameSwapper, 2> cacheArr;

	GTime		time0;
	ERPValues	erpv;

	Matrix3d	i2t_mat;
	Matrix3d	di2t_mat;
	Vector3d	translation = Vector3d::Zero();

	void setCache(
		int cache)
	{
		cacheArr[cache] = *this;
	}

	FrameSwapper()
	{

	}

	FrameSwapper(
				GTime		time,
		const	ERPValues&	erpv);

	VectorEcef	operator()(
		const	VectorEci	rEci,
		const	VectorEci*	vEci_ptr	= nullptr,
				VectorEcef*	vEcef_ptr	= nullptr)
	{
		if	(  vEci_ptr
			&& vEcef_ptr)
		{
			auto& vEci	= *vEci_ptr;
			auto& vEcef	= *vEcef_ptr;

			vEcef	=  i2t_mat * vEci
					+ di2t_mat * rEci;
		}

		return (Vector3d) (i2t_mat	* rEci + translation);
	}

	VectorEci	operator()(
		const	VectorEcef	rEcef,
		const	VectorEcef*	vEcef_ptr	= nullptr,
				VectorEci*	vEci_ptr	= nullptr)
	{
		if	(  vEcef_ptr
			&& vEci_ptr)
		{
			auto& vEcef	= *vEcef_ptr;
			auto& vEci	= *vEci_ptr;

			vEci 	=  i2t_mat.transpose() * vEcef
					+ di2t_mat.transpose() * rEcef;
		}

		return (Vector3d) (i2t_mat.transpose()	* ((Vector3d)rEcef - translation));
	}

	VectorEci	operator()(
		const	VectorEcef	rEcef,
		const	GTime		time)
	{
		VectorEci eci = operator()(rEcef);

		double dt = (time - time0).to_double();

		eci += dt * di2t_mat.transpose() * rEcef;

		return eci;
	}
};
