/*
* Modified version of satRefSys.cpp by O. Montenbruck, E. Gill
* Transformations betweeen celestial and terrestrial reference systems
*/

#ifndef INC_SATREFSYS_HPP
#define INC_SATREFSYS_HPP

#include "eigenIncluder.hpp"
#include "constants.hpp"


struct IERS 
{
	double UT1_TAI	= 0;				// UT1-TAI time difference [s]
	double UTC_TAI	= 0;				// UTC-TAI time difference [s]
	double xp		= 0;				// Pole coordinate [rad]
	double yp		= 0;				// Pole coordinate [rad]
	double lod		= 0;				// length of day [s/d]
	
	
	static constexpr double  TT_TAI	= +32.184;				//  TT-TAI time difference 32.184s
	static constexpr double GPS_TAI	= -19;					// GPS-TAI time difference -19s

	static constexpr double TAI_GPS	= -GPS_TAI;				// TAI-GPS time difference 19s
	static constexpr double  TT_GPS	= TT_TAI + TAI_GPS;		//  TT-GPS time difference 51.184s
	
	IERS()
	{
		
	}
	
	IERS(
		double UT1_UTC,					// Set Earth rotation parameters
		double UTC_TAI,					// (UT1-UTC [s],UTC-TAI [s],
		double x_pole,					//  x [radian], y [radian])
		double y_pole,
		double lod)						//length of day [second/day]
	{
		this->UT1_TAI	= UT1_UTC + UTC_TAI;
		this->UTC_TAI	= UTC_TAI;
		this->xp		= x_pole;
		this->yp		= y_pole;
		this->lod		= lod;
	}
	
	double UTC_GPS() {return UTC_TAI	- GPS_TAI;		}		// UTC_GPS time difference [s]
	double UT1_GPS() {return UT1_TAI	- GPS_TAI;		}		// UT1-GPS time difference [s]

	double  TT_UTC() {return  TT_TAI	- UTC_TAI;		}		//  TT-UTC time difference [s]
	double GPS_UTC() {return GPS_TAI	- UTC_TAI;		}		// GPS-UTC time difference [s]
	double UT1_UTC() {return UT1_TAI	- UTC_TAI;		}		// UT1-UTC time difference [s]
// 	double TAI_UTC(double Mjd_UTC);								// TAI-UTC time difference [s]
};


#endif 
