#include <iostream>
#include <fstream>
#include <ctime>
#include "common.hpp"
#include "navigation.hpp"
#include "acsConfig.hpp"
#include "forceModels.hpp"
using namespace std;

MatrixXd cnm = MatrixXd::Zero(361, 361), snm = MatrixXd::Zero(361, 361);

void inputEGMCoe(MatrixXd &cnm, MatrixXd &snm)
{
    ifstream egmCoeFile;
    int j = 0, n = 360;
    double val;

    std::string strEGMCoeFile = "GGM03S.txt";
    // this code opens the Earth gravity model (coefficients) file
    // egmCoeFile.open(strEGMCoeFile);
    std::ifstream egmCoeFile(strEGMCoeFile);
    double val = 0;
    while (j <= n)
    {
        for (int i = 0; i <= j; i++)
        {
            egmCoeFile >> val;
            egmCoeFile >> val;
            egmCoeFile >> val;
            cnm(j, i) = val;
            egmCoeFile >> val;
            snm(j, i) = val;
            egmCoeFile >> val;
            egmCoeFile >> val;
        }
        j++;
    }
    egmCoeFile.close();
}

int main()
{
    tracelevel(5);

    %%
	BOOST_LOG_TRIVIAL(info)
	<< "Testing Earth central body gravity" << std::endl;

    AuxParam  Aux;     // Auxiliary parameters          

    double cd[6] = {2019, 7, 18, 0, 0, 0.0 };
    GTime gst = epoch2time(cd);
    double jd = ymdhms2jd(cd);
    double mjd = jd - 2400000.5;

	BOOST_LOG_TRIVIAL(info)
	<< "The testing date " << std::endl;
    
    //reading sp3 orbit data
    acsConfig.parse();
    
    //reading erp/eop data
    std::string erpfile = "./examples/products/igs20627.erp";
    BOOST_LOG_TRIVIAL(info)
	<< "Loading ERP file " << erpfile << std::endl;

    erp_t erp;
    readerp(erpfile, &erp);

    ERPValues erpv;
    geterp(&erp, gst, erpv);

    Matrix3d i2tMatrix = Matrix3d::Identity();
    eci2ecef(gst, erpv, i2tMatrix, nullptr);

	Aux.Area_drag       = 55.64;  // [m^2]
	Aux.Area_solar      = 88.4;   // [m^2]
	Aux.mass            = 8000.0; // [kg]
	Aux.CR              = 1.0;
	Aux.CD              = 2.7;
	Aux.n_a             = 20;
	Aux.m_a             = 20;
	Aux.n_g             = 2;
	Aux.m_g             = 2;    
	Aux.flagSun         = false;
	Aux.flagMoon        = false;
    Aux.flagPlanets     = false;
	Aux.SRad            = false;
	Aux.Drag            = false;    
    Aux.flagTides       = false;
    Aux.flagRelativity  = false;


}
