
#pragma once

#include "eigenIncluder.hpp"

struct Legendre
{
	Legendre();
	
	Legendre(int nmax);
	
	void setNmax(int n);
	void init();
	void calculate(double X);
	int nmax;

	MatrixXd Pnm;
	MatrixXd dPnm;

	MatrixXd anm;
	MatrixXd bnm;
	MatrixXd fnm;
};

Vector3d accelSourcePoint(
	const	Vector3d	observer, 
	const	Vector3d	origin, 
	const	double		GM,
			Matrix3d*	dAdPos_ptr = nullptr);

Vector3d accelCentralForce(
	const	Vector3d	observer, 
	const	double		GM,
			Matrix3d*	dAdPos_ptr = nullptr);

Vector3d accelSPH(const Vector3d r, const MatrixXd C, const MatrixXd S, const int n, const double GM);

Vector3d accelJ2(
	const	double		C20,
	const	Matrix3d	eci2ecf,
			Vector3d	bodyPos, 
			double		GM);

