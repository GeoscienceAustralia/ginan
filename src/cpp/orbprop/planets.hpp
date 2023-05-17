/* JPL planetary and lunar ephemerides */

#pragma once

#include "eigenIncluder.hpp"
#include "constants.hpp"
#include "gTime.hpp"
#include "enums.h"
#include "erp.hpp"

/**         
*   If nutations are wanted, set ntarg = 14.                      
*   For librations, set ntarg = 15. set ncent= 0.                 
*   For TT-TDB,  set ntarg = 17.                                  
*   I've not actually seen an ntarg = 16 case yet.)               
*/
enum E_SolarSysStarType 
{
	eMercury	= 1,   ///< Mercury
	eVenus		= 2,   ///< Venus
	eEarth		= 3,   ///< Earth
	eMars		= 4,   ///< Mars
	eJupiter	= 5,   ///< Jupiter
	eSaturn		= 6,   ///< Saturn
	eUranus		= 7,   ///< Uranus
	eNeptune	= 8,   ///< Neptune
	ePluto		= 9,   ///< Pluto
	eMoon		= 10,  ///< Moon
	eSun		= 11,  ///< Sun

	eSolarSysBarycenter		= 12,   ///< Solar System barycenter
	eEarthMoonBaryCenter	= 13,   ///< Earth Moon barycenter
	eJplNutation			= 14,   ///< nutations (longitude and obliq)     
	eJPLLnrLibration		= 15,   ///< Lunar Librations
	eJPLLunarMantle			= 16,   ///< Lunar Mantle omega_x,omega_y,omega_z
	eJPLTT_TDB				= 17,   ///< TT-TDB, if on eph. file

	eSelfDefinedStarType	= 99,
};


bool jplEphPos(
		struct jpl_eph_data*	jplEph_ptr,	
		MjDateTT				mjdTT,		
		E_ThirdBody				thirdBody,
		Vector3d&				pos,
		Vector3d*				vel_ptr = nullptr);

bool planetPosEcef(
	GTime		time,
	E_ThirdBody	thirdBody,
	VectorEcef&	ecef,
	ERPValues	erpv = {});

//From DE440
static map<const E_ThirdBody, double> GM_values =
{
	{E_ThirdBody::MERCURY,	22031.868551e9		},
	{E_ThirdBody::VENUS,	324858.592000e9		},
	{E_ThirdBody::EARTH,	0.3986004415E+15	},
	{E_ThirdBody::MARS,		42828.375816e9		},
	{E_ThirdBody::JUPITER,	126712764.100000e9	},
	{E_ThirdBody::SATURN,	37940584.841800e9	},
	{E_ThirdBody::URANUS,	5794556.400000e9	},
	{E_ThirdBody::NEPTUNE,	6836527.100580e9	},
	{E_ThirdBody::PLUTO,	975.500000e9		},
	{E_ThirdBody::MOON,		4.9027989e12		},
	{E_ThirdBody::SUN,		1.32712440018e20	},
};

double sunVisibility(
	Vector3d&	rSat,	
	Vector3d&	rSun,	
	Vector3d&	rMoon);	

