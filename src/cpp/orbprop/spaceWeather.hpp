#pragma once

#include <string>
#include <vector>
/** Space Weather data struct
 */
struct SpaceWeatherData
{
    int year  = 0;
    int month = 0;
    int day   = 0;
    int bsrn  = 0;  ///< Bartels Solar Rotation Number. A sequence of 27-day intervals counted
                    ///< continuously from 1832 Feb 8.
    int nd    = 0;  ///< Number of Day within the Bartels 27-day cycle (01-27)
    int kp[8] = {0};
    ;               ///< Planetary 3-hour Range Index (Kp) (kp1 to kp8)
    int sum = 0;  ///< Sum of the 8 Kp indices for the day: Kp has values of 0o, 0+, 1-, 1o, 1+, 2-,
                  ///< 2o, 2+, ... , 8o, 8+, 9-, 9o,
    ///<                                      which are expressed in steps of one third unit.
    ///<                                      These values are multiplied by 10 and rounded to an
    ///<                                      integer value
    int    ap[8] = {0};  ///< Planetary Equivalent Amplitude (Ap) (ap1 to ap8)
    int    avg   = 0;    ///< Arithmetic average of the 8 Ap indices for the day
    double cp =
        0;  ///< Planetary Daily Character Figure: A qualitative estimate of overall level of
            ///< magnetic activity for the day determined from the sum of the 8 Ap indices.
    ///<                                     Cp ranges, in steps of one-tenth, from 0 (quiet) to 2.5
    ///<                                     (highly disturbed).
    int c9 =
        0;  ///< A conversion of the 0-to-2.5 range of the Cp index to one digit between 0 and 9
    int isn = 0;  ///< International Sunspot Number. Records contain the Zurich number through 1980
                  ///< Dec 31 and the International Brussels number thereafter.
    double adj_f10_7 =
        0.0;  ///< 10.7-cm Solar Radio Flux (F10.7) adjusted to 1 AU: Measured at Ottawa at 1700 UT
              ///< daily from 1947 Feb 14 until 1991 May 31 and measured at
    ///<                                                    Penticton at 2000 UT from 1991 Jun 01
    ///<                                                    on. Expressed in units of 10-22 W/m2/Hz.
    int q = 0;  ///< Flux Qualifier: (0: flux required no adjustment;
    ///<                  1: flux required adjustment for burst in progress at time of measurement;
    ///<                  2: a flux approximated by either interpolation or extrapolation;
    ///<                  3: no observation;
    ///<                  4: CelesTrak interpolation of missing data)
    double adj_ctr81 = 0.0;  ///< Centered 81-day arithmetic average of F10.7 (adjusted)
    double adj_lst81 = 0.0;  ///< Last 81-day arithmetic average of F10.7 (adjusted)
    double obs_f10_7 = 0.0;  ///< Observed (unadjusted) value of F10.7
    double obs_ctr81 = 0.0;  ///< Centered 81-day arithmetic average of F10.7 (observed)
    double obs_lst81 = 0.0;  ///< Last 81-day arithmetic average of F10.7 (observed)
};

struct SpaceWeather
{
    void                          read(std::string filepath);
    std::vector<SpaceWeatherData> SpaceWeather;
};

extern SpaceWeather spaceWeatherData;
bool                dateMatches(const SpaceWeatherData& data, int year, int month, int day);