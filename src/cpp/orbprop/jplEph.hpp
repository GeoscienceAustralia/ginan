/* JPL planetary and lunar ephemerides */

#ifndef JPLEPH_HPP
#define JPLEPH_HPP

#include "eigenIncluder.hpp"

/*
*                          
*   5 = jupiter          12 = solar-system barycenter             
*   6 = saturn           13 = earth-moon barycenter               
*   7 = uranus           14 = nutations (longitude and obliq)     
*                        15 = librations, if on eph. file         
*                        16 = lunar mantle omega_x,omega_y,omega_z
*                        17 = TT-TDB, if on eph. file             
*   If nutations are wanted, set ntarg = 14.                      
*   For librations, set ntarg = 15. set ncent= 0.                 
*   For TT-TDB,  set ntarg = 17.                                  
*   I've not actually seen an ntarg = 16 case yet.)               
*/
enum E_SolarSysStarType
{
	eMercury                =   1,   ///< Mercury
	eVenus                  =   2,   ///< Venus
	eEarth                  =   3,   ///< Earth
	eMars                   =   4,   ///< Mars
	eJupiter                =   5,   ///< Jupiter
	eSaturn                 =   6,   ///< Saturn
	eUranus                 =   7,   ///< Uranus
	eNeptune                =   8,   ///< Neptune
	ePluto                  =   9,   ///< Pluto
	eMoon                   =   10,  ///< Moon
	eSun                    =   11,  ///< Sun

	eSolarSysBarycenter     =   12,   ///< Solar System barycenter
	eEarthMoonBaryCenter    =   13,   ///< Earth Moon barycenter
	eJplNutation            =   14,   ///< Nutations
	eJPLLnrLibration        =   15,   ///< Lunar Librations
	eJPLLunarMantle         =   16,   ///< Lunar Mantle omega_x,omega_y,omega_z
	eJPLTT_TDB              =   17,   ///< TT-TDB, if on eph. file

	eSelfDefinedStarType    =   99,
};


bool jplEphPos(
	struct jpl_eph_data*	jplEph_ptr,
	double					jd,			
	E_ThirdBody				thirdBody,	
	Vector3d&				pos,		
	Vector3d*				vel_ptr = nullptr);	


#endif //JPLEPH_HPP
