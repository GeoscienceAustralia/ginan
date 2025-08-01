#pragma once

#include <map>
#include <string>
#include <vector>
#include "common/common.hpp"
#include "common/gTime.hpp"

using std::map;
using std::string;
using std::vector;

constexpr char eopComments[][16] = {"XP (MAS)", "YP (MAS)", "UT1(MTS)"};

/** earth rotation parameter data type
 */
struct ERPValues
{
    GTime time;

    double xp     = 0;  ///< pole offset (rad)
    double yp     = 0;  ///< pole offset (rad)
    double ut1Utc = 0;  ///< ut1-utc (s)
    double lod    = 0;  ///< delta length of day (s/day)

    double xpr = 0;     ///< pole offset rate (rad/day)
    double ypr = 0;     ///< pole offset rate (rad/day)

    double xpSigma     = 0;
    double ypSigma     = 0;
    double xprSigma    = 0;
    double yprSigma    = 0;
    double ut1UtcSigma = 0;
    double lodSigma    = 0;

    bool isPredicted = false;
    bool isFiltered  = false;

    ERPValues operator+(const ERPValues& rhs);
    bool      operator==(const ERPValues& rhs) const;
    ERPValues operator*(const double scalar);

    string toString() const;
    string toReadableString() const;
};

struct ERP
{
    vector<map<GTime, ERPValues>> erpMaps;

    ERPValues filterValues;
};

struct KFState;

void readErp(string filename, ERP& erp);

ERPValues getErp(ERP& erp, GTime time, bool useFilter = true);

void writeErp(string filename, ERPValues& erp);

void writeErpFromNetwork(string filename, KFState& kfState);

ERPValues getErpFromFilter(const KFState& kfState);

Matrix3d receiverEopPartials(Vector3d& rRec);
