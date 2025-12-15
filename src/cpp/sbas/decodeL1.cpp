#include "common/acsConfig.hpp"
#include "common/navigation.hpp"
#include "sbas/sbas.hpp"

void decodeSBASMessage(Trace& trace, GTime time, SBASMessage& mess, Navigation& nav)
{
    int type = mess.type;
    if (type == 0)
        type = acsConfig.sbsInOpts.mt0;

    switch (type)
    {
        // case  1:	decodeL1SBASMask(frameTime,nav,mess.data);		break;	// Satellite mask
        // case  2:	decodeL1FastCorr(frameTime,nav,mess.data,0);	break;	// Fast Corrections sat
        // 1-13 case  3:	decodeL1FastCorr(frameTime,nav,mess.data,13);	break;	// Fast
        // Corrections sat 14-26 case  4:	decodeL1FastCorr(frameTime,nav,mess.data,26);	break;
        // // Fast Corrections sat 27-39 case  5:	decodeL1FastCorr(frameTime,nav,mess.data,39);
        // break;	// Fast Corrections sat 40-51 case  6:
        // decodeL1UDREBase(frameTime,nav,mess.data);		break;	// UDRE case  7:
        // decodeL1FastDegr(          nav,mess.data);		break;	// Fast Correction degradation
        // case  9:	decodeL1GEO_Navg(frameTime,nav,mess.data,mess.prn);		break;	// GEO satellite
        // position data (Ephemeris) case 10:	decodeL1SlowDegr(          nav,mess.data);
        // break;	// Slow Correction degradation case 12:
        // decodeL1SBASTime(frameTime,nav,mess.data);		break;	// SBAS Network Time nav,message
        // case 17:	decodeL1GEO_Almn(frameTime,nav,mess.data);		break;	// GEO satellite
        // position data (Almanac) *not supported case 18:	decodeL1IonoGrid( nav,mess.data);
        // break;	// Ionosphere Grid points definition case 24:
        // decodeL1MixdCorr(frameTime,nav,mess.data);		break;	// Fast & Slow Correction
        // degradation   *not supported case 25:	decodeL1SlowCorr(frameTime,nav,mess.data);
        // break;	// Slow corrections case 26:	decodeL1IonoGIVD(frameTime,nav,mess.data);
        // break;	// Ionosphere Correction at IGP case 27:
        // decodeL1UDRERegn(frameTime,nav,mess.data);		break;	// UDRE Region definition case
        // 28:	decodeL1UDRECovr(frameTime,nav,mess.data);		break;	// UDRE covariance matrix
        // case 62:
        case 63:
            break;
        default:
            tracepdeex(5, std::cout, "\nSBAS_MT%02d, not supported", type);
            break;
    }
    return;
}
