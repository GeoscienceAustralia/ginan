#include "common/ionModels.hpp"
#include <math.h>
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/enums.h"
#include "common/satStat.hpp"

/** ionosphere mapping function
 */
double ionmapf(
    const VectorPos& pos,    ///< receiver position in geocentric spherical coordinates
    const AzEl&      azel,   ///< satellite azimuth/elevation angle (rad)
    E_IonoMapFn      mapFn,  ///< model of mapping function
    double           hion    ///< layer height (km)
)
{
    double alpha = 1;
    switch (mapFn)
    {
        case E_IonoMapFn::SLM:  // fallthrough
        case E_IonoMapFn::MLM:
            break;              // same to SLM but need to call the function multiple times
        case E_IonoMapFn::MSLM:
            alpha = 0.9782;
            break;
        case E_IonoMapFn::KLOBUCHAR:
            return 1 + 16 * pow(0.53 - azel.el / PI, 3);
    }

    double rp = RE_MEAN / (RE_MEAN + hion) * sin(alpha * (PI / 2 - azel.el));

    return 1 / sqrt(1 - SQR(rp));
}

/* ionospheric pierce point position -------------------------------------------
 * compute ionospheric pierce point (ipp) position and slant factor
 * args   : double *pos      I   receiver position {lat,lon,h} (rad,m)
 *          double *azel     I   azimuth/elevation angle {az,el} (rad)
 *          double re        I   earth radius (km)
 *          double hion      I   altitude of ionosphere (km)
 *          double *posp     O   pierce point position {lat,lon,r} (rad,m) in geocentric spherical
 *coordinate system return : slant factor notes  : see ref [2], only valid on the earth surface
 *          fixing bug on ref [2] A.4.4.10.1 A-22,23
 *-----------------------------------------------------------------------------*/
double ionppp(const VectorPos& pos, const AzEl& azel, double re, double hion, VectorPos& posp)
{
    double ri    = re + hion;
    double rp    = re / ri * cos(azel.el);
    double ap    = PI / 2 - azel.el - asin(rp);
    double sinap = sin(ap);
    double tanap = tan(ap);
    double cosaz = cos(azel.az);
    posp[0]      = asin(sin(pos.lat()) * cos(ap) + cos(pos.lat()) * sinap * cosaz);

    if ((pos.lat() > +70 * D2R && +tanap * cosaz > tan(PI / 2 - pos.lat())) ||
        (pos.lat() < -70 * D2R && -tanap * cosaz > tan(PI / 2 + pos.lat())))
    {
        posp.lon() = pos.lon() + PI - asin(sinap * sin(azel.az) / cos(posp.lat()));
    }
    else
    {
        posp.lon() = pos.lon() + asin(sinap * sin(azel.az) / cos(posp.lat()));
    }

    posp[2] = ri * 1000;  // geocentric radius

    return 1 / sqrt(1 - SQR(rp));
}
