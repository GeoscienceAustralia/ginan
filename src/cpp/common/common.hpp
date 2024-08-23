
#pragma once

#include <memory>

/* constants/macros */
#define SQR(x)      ((x)*(x))
#define POW4(x)     ((x)*(x)*(x)*(x))
#define SQRT(x)     ((x)<=0.0?0.0:sqrt(x))
#define ROUND(x)    (int)floor((x)+0.5)
#define SWAP(x,y)   do {double tmp_; tmp_=x; x=y; y=tmp_;} while (0)
#define SGN(x)      ((x)<=0.0?-1.0:1.0)

#include "eigenIncluder.hpp"
#include "gTime.hpp"
#include "erp.hpp"
#include "enums.h"

using std::multimap;
using std::vector;
using std::pair;

struct SatSys;
struct SatPos;
struct AzEl;

struct Average
{
	double	mean	= 0;
	double	var		= 0;
};

struct descope
{
	//used to temporarily prevent accessing things that shouldnt be by throwing compiler errors
};

void lowPassFilter(
	Average&	avg,
	double		meas,
	double		procNoise,
	double		measVar = 1);

void wrapPlusMinusPi(
	double&	angle);

void wrap2Pi(
	double&	angle);

double geodist(Vector3d& rs, Vector3d& rr, Vector3d& e);

double sagnac(
	Vector3d&	rSource,
	Vector3d&	rDest,
	Vector3d	vel = Vector3d::Zero());


void satazel(
	const	VectorPos&	pos,
	const	VectorEcef&	e,
			AzEl&		azel);

unsigned int crc24q (const unsigned char *buff, int len);

struct Dops
{
	double gdop = 0;
	double pdop = 0;
	double hdop = 0;
	double vdop = 0;
};

Dops dopCalc(
	const	vector<AzEl>&	azels);

bool satFreqs(
	E_Sys		sys,
	E_FType&	frq1,
	E_FType&	frq2,
	E_FType&	frq3);

int sisaToSva(double sisa);
double svaToSisa(int sva);
int uraToSva(double ura);
double svaToUra(int sva);

void updateLamMap(
	const	GTime&	time,
			SatPos&	obs);



/** An iterator that trys to cast elements to the desired type before using them
 */
template<
	typename OUTTYPE,
	typename INTYPE,
	typename VOIDTYPE>
struct IteratorType
{
	typename INTYPE::iterator	ptr_ptr;
	typename INTYPE::iterator	endPtr_ptr;

	IteratorType(
		typename INTYPE::iterator	startPtr_ptr,
		typename INTYPE::iterator	endPtr_ptr)
	:	ptr_ptr		(startPtr_ptr),
		endPtr_ptr	(endPtr_ptr)
	{
		ptr_ptr--;
		incrementUntilGood();
	}

	bool operator !=(IteratorType rhs)
	{
		return ptr_ptr != rhs.ptr_ptr;
	}

	OUTTYPE& operator*()
	{
		return static_cast<OUTTYPE&>(**ptr_ptr);
	}

	void incrementUntilGood()
	{
		while (1)
		{
			++ptr_ptr;
			if (ptr_ptr == endPtr_ptr)
				return;

			try
			{
				(void) dynamic_cast<OUTTYPE&>(**ptr_ptr);
				//no throw, sucess, stop
				return;
			}
			catch(...){}
		}
	}

	void operator++()
	{
		incrementUntilGood();
	}
};

/** An iterator that trys to cast elements to the desired type before using them
 */
template<
	typename OUTTYPE,
	typename INTYPE,
	typename KEYTYPE>
struct MapIteratorType
{
	typename INTYPE::iterator	ptr_ptr;
	typename INTYPE::iterator	endPtr_ptr;

	MapIteratorType(
		typename INTYPE::iterator	startPtr_ptr,
		typename INTYPE::iterator	endPtr_ptr)
	:	ptr_ptr		(startPtr_ptr),
		endPtr_ptr	(endPtr_ptr)
	{
		if (ptr_ptr == endPtr_ptr)
			return;

		try
		{
			(void) dynamic_cast<OUTTYPE&>(*ptr_ptr->second);
			//no throw, sucess, stop
			return;
		}
		catch(...){}

		incrementUntilGood();
	}

	bool operator !=(MapIteratorType rhs)
	{
		return ptr_ptr != rhs.ptr_ptr;
	}

	const pair<const KEYTYPE&, OUTTYPE&> operator*()
	{
		auto& thing = *ptr_ptr->second;
		return {ptr_ptr->first, dynamic_cast<OUTTYPE&>(thing)};
	}

	void incrementUntilGood()
	{
		while (1)
		{
			++ptr_ptr;
			if (ptr_ptr == endPtr_ptr)
				return;

			try
			{
				(void) dynamic_cast<OUTTYPE&>(*ptr_ptr->second);
				//no throw, sucess, stop
				return;
			}
			catch(...){}
		}
	}

	void operator++()
	{
		incrementUntilGood();
	}
};

/** An object just for templating the other functions without over-verbosity
 */
template <
	template<typename,typename,typename> typename	ITERATOR,
	typename										TYPE,
	typename										KEYTYPE,
	typename										INTYPE>
struct Typer
{
	INTYPE& baseContainer;

	Typer(
		INTYPE& baseContainer)
	: baseContainer (baseContainer)
	{

	}

			ITERATOR<TYPE, INTYPE, KEYTYPE>		begin()			{ return ITERATOR<TYPE, INTYPE, KEYTYPE>(baseContainer.begin(),	baseContainer.end());	}
	const	ITERATOR<TYPE, INTYPE, KEYTYPE>		begin()	const	{ return ITERATOR<TYPE, INTYPE, KEYTYPE>(baseContainer.begin(),	baseContainer.end());	}
			ITERATOR<TYPE, INTYPE, KEYTYPE>		end()			{ return ITERATOR<TYPE, INTYPE, KEYTYPE>(baseContainer.end(),	baseContainer.end());	}
    const	ITERATOR<TYPE, INTYPE, KEYTYPE>		end()	const	{ return ITERATOR<TYPE, INTYPE, KEYTYPE>(baseContainer.end(),	baseContainer.end());	}
};


/** Use only a subset of a vector that can be cast to a desired type
 * \private
 */
template<
	typename OUT,
	typename ENTRY
	>
Typer<
	IteratorType,
	OUT,
	void,
	vector<ENTRY>>
only(
	vector<ENTRY>& in)
{
	return Typer<IteratorType,		OUT,	void,		vector<ENTRY>				>(in);
}


/** Use only a subset of a map that can be cast to a desired type
 */
template<
	typename OUT,
	typename KEYTYPE,
	typename VALUE>
Typer<
	MapIteratorType,
	OUT,
	KEYTYPE,
	multimap<KEYTYPE, VALUE>>
only(
	multimap<KEYTYPE, VALUE>& in)
{
	return Typer<MapIteratorType,	OUT,	KEYTYPE,	multimap<KEYTYPE, VALUE>	>(in);
}


extern int		epoch;
extern GTime	tsync;
