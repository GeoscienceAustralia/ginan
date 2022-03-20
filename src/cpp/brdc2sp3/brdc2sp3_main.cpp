#include <unordered_map>
#include <iostream>
#include <string>
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/log/trivial.hpp>

#include "streamTrace.hpp"
#include "navigation.hpp"
#include "constants.hpp"
#include "station.hpp"
#include "antenna.hpp"
#include "satSys.hpp"
#include "tides.hpp"
#include "rinex.hpp"
#include "gTime.hpp"

#define J2_GLO   1.0826257E-3     /* 2nd zonal harmonic of geopot   ref [2] */

#define OMGE_GLO 7.292115E-5      /* earth angular velocity (rad/s) ref [2] */
#define OMGE_GAL 7.2921151467E-5  /* earth angular velocity (rad/s) ref [7] */
#define OMGE_CMP 7.292115E-5      /* earth angular velocity (rad/s) ref [9] */

#define RTOL_KEPLER 	1E-14         /* relative tolerance for Kepler equation */
#define MAX_ITER_KEPLER 30        /* max number of iteration of Kelpler */
#define GLOSTEP   		60.0

#define SIN_5 -0.0871557427476582 /* sin(-5.0 deg) */
#define COS_5  0.9961946980917456 /* cos(-5.0 deg) */

nav_t	nav = {};
string			GNSSinp="GE";
map<SatSys,int> BS_Satel_List;
int 			BS_Start_Week		= 0;
double			BS_Start_TOW		= 0.0;
double			BS_Start_Epoc[6]	= {0};
int 			BS_Epoch_Num		= 0;
double			BS_Epoch_Inter		= 900.0;
map<E_Sys,double> BS_Max_Dtime;

void helpstring(void)
{
	fprintf(stdout, " brdc2sp3 -ant <ATX file> -inp <RINEX NAV file> [options]\n");
	fprintf(stdout, " Options [default]:\n");
	fprintf(stdout, "     -gns string   GNSS to be processed: G: GPS, R:GLONASS, E:Galileo, C: Beidou, J: QZSS, A: all [GE]\n");
	fprintf(stdout, "     -int float    Time spacing of SP3 entries [900.0]\n");
	fprintf(stdout, "     -out string   File name for SP3 output [BS_orbits.sp3]\n");
	fprintf(stdout, "     -*dt max_dt   set max_dt for constellation (*: 'G','R','E','J' or 'C') [86400.0]\n");
	fprintf(stdout, "     -clst         Select ephemeris with closest Toe (within max_dt seconds) [Latest valid ephemeris]\n");
	fprintf(stdout, "     -inav         I/NAV only for Galileo [use both F/NAV and I/NAV]\n");
	fprintf(stdout, "     -fnav         F/NAV only for Galileo [use both F/NAV and I/NAV]\n");
	fprintf(stdout, "     -frst         Filter out satellites with no/bad data in first epoc [not filtered]\n");
} 

int BS_satantoff( GTime time, SatSys Sat, Vector3d& rs, Vector3d& dant, int gloind = 0)
{
	double lam1, lam2;

	char id[5];
	Sat.getId(id);
	
	PhaseCenterData* pcsat = findAntenna(Sat.id(), time, nav);

	if (pcsat == nullptr) return -1;
	auto& pcoMap = pcsat->pcoMap;
	
	E_FType j = F1;
	E_FType k = F2;
	double glonfrq1 = FREQ1_GLO + DFRQ1_GLO*gloind;
	double glonfrq2 = FREQ2_GLO + DFRQ2_GLO*gloind;
	switch (Sat.sys)
	{
		case E_Sys::GPS: lam1 = CLIGHT/FREQ1; 		lam2 = CLIGHT/FREQ2;			break;
		case E_Sys::GLO: lam1 = CLIGHT/glonfrq1; 	lam2 = CLIGHT/glonfrq2;			break;
		case E_Sys::QZS: lam1 = CLIGHT/FREQ1; 		lam2 = CLIGHT/FREQ2;			break;
		case E_Sys::GAL: lam1 = CLIGHT/FREQ1; 		lam2 = CLIGHT/FREQ5;	k=F5;	break;
		case E_Sys::BDS: lam1 = CLIGHT/FREQ1_CMP; 	lam2 = CLIGHT/FREQ2_CMP; 		break;
		default:     	 return -2; 
	}
	
	/* sun position in ecef */
	Vector3d rsun;
	double gmst;
	ERPValues erpv;
	sunmoonpos(gpst2utc(time), erpv, &rsun, nullptr, &gmst);

	/* unit vectors of satellite fixed coordinates */
	Vector3d r = -rs;
	Vector3d ez = r.normalized();
	r = rsun - rs;
	Vector3d es = r.normalized();
	r = ez.cross(es);
	Vector3d ey = r.normalized();
	Vector3d ex = ey.cross(ez);
	
	double gamma	= lam2*lam2/lam1/lam1;
	double C1		= gamma	/ (gamma - 1);
	double C2		= -1	/ (gamma - 1);

	/* iono-free LC */
	for (int i = 0; i < 3; i++)	
	{
		/* ENU to NEU */
		Vector3d pcoJ;
		Vector3d pcoK;
		if (pcoMap.find(j) == pcoMap.end())	pcoJ = Vector3d::Zero();
		else								pcoJ = pcoMap[j];
		if (pcoMap.find(k) == pcoMap.end())	pcoK = Vector3d::Zero();
		else								pcoK = pcoMap[k];
		double dant1	= pcoJ[1] * ex(i)
						+ pcoJ[0] * ey(i)
						+ pcoJ[2] * ez(i);	//todo aaron, matrix
		double dant2	= pcoK[1] * ex(i)
						+ pcoK[0] * ey(i)
						+ pcoK[2] * ez(i);
						
		fprintf( stdout,"\n  ATX: %s %11.6f %11.6f %11.6f  %11.6f %11.6f %11.6f", Sat.id().c_str(), pcoJ[0], pcoJ[1], pcoJ[2], pcoK[0], pcoK[1], pcoK[2]);

		//dant(i)	= C1 * dant1
		//		+ C2 * dant2;
		dant(i) = dant1;
	}
	return 0;
}

Eph* BS_seleph( GTime time, SatSys Sat, int iode, nav_t& nav_, int opt=0)
{
	double valid = 0;
	
	switch (Sat.sys) 
	{
		case E_Sys::QZS:	valid = MAXDTOE_QZS	+ 1; break;
		case E_Sys::GAL:	valid = MAXDTOE_GAL	+ 1; break;
		case E_Sys::BDS:	valid = MAXDTOE_CMP	+ 1; break;
		default: 			valid = MAXDTOE		+ 1; break;
	}
	
	auto& ephList = nav_.ephMap[Sat];
	Eph* chosen = nullptr;
	GTime latestToe = GTime::noTime();
	double max_dtime = BS_Max_Dtime[Sat.sys];
	for (auto& [dummy, eph] : ephList)
	{
		if 	( iode >= 0 )
		{
			if(iode == eph.iode) return &eph;
			else continue;
		}
		//fprintf(stdout, "    %s %s\n",Sat.id().c_str(),eph.toe.to_string(0).c_str());
		if(opt == 0)
		{
			if (fabs(eph.toe - time) > valid)
				continue;
			if (eph.toe > latestToe)
			{
				chosen	= &eph;
				latestToe = eph.toe;
			}
		}
		if(opt==1)
		{
			double dtime=max_dtime;
			if (Sat.sys == +E_Sys::GPS)	dtime = fabs(eph.toe - (time + 3600));
			else						dtime = fabs(eph.toe - time);
			
			if ( max_dtime > dtime)
			{
				chosen	= &eph;
				latestToe = eph.toe;
				max_dtime = dtime;
			}
		}

	}
	
	return chosen;
}

Geph* BS_selgeph( GTime time, SatSys Sat, int iode, nav_t& nav_, int opt=0)
{
	double valid = MAXDTOE_GLO	+ 1;
	
	auto& gephList = nav_.gephMap[Sat];
	Geph* chosen = nullptr;
	GTime latestToe = GTime::noTime();
	double max_dtime = BS_Max_Dtime[E_Sys::GLO];
	for (auto& [dummy, geph] : gephList)
	{
		if 	( iode >= 0 )
		{
			if(iode == geph.iode) 	return &geph;
			else 					continue;
		}
		//fprintf(stdout, "    %s %s\n",Sat.id().c_str(),geph.toe.to_string(0).c_str());
		if(opt==0)
		{
			if (fabs(geph.toe - time) > valid)
				continue;
			if (geph.toe > latestToe)
			{
				chosen	= &geph;
				latestToe = geph.toe;
			}
		}
		if(opt==1)
		{
			double dtime = fabs(geph.toe - time);
			if ( max_dtime > dtime)
			{
				chosen	= &geph;
				latestToe = geph.toe;
				max_dtime = dtime;
			}
		}
	}
	
	return chosen;
}

/* glonass orbit differential equations --------------------------------------*/
void deq(const double* x, double* xdot, Vector3d& acc)
{
	double a, b, c, r2 = dot(x, x, 3), r3 = r2 * sqrt(r2), omg2 = OMGE_GLO*OMGE_GLO;

	if (r2 <= 0.0)
	{
		xdot[0] = xdot[1] = xdot[2] = xdot[3] = xdot[4] = xdot[5] = 0.0;
		return;
	}

	/* ref [2] A.3.1.2 with bug fix for xdot[4],xdot[5] */
	a = 1.5 * J2_GLO * MU_GLO * RE_GLO*RE_GLO / r2 / r3; /* 3/2*J2*mu*Ae^2/r^5 */
	b = 5.0 * x[2] * x[2] / r2;            /* 5*z^2/r^2 */
	c = -MU_GLO / r3 - a * (1.0 - b);      /* -mu/r^3-a(1-b) */
	xdot[0] = x[3];
	xdot[1] = x[4];
	xdot[2] = x[5];
	xdot[3] = (c + omg2) * x[0] + 2.0 * OMGE_GLO * x[4] + acc[0];
	xdot[4] = (c + omg2) * x[1] - 2.0 * OMGE_GLO * x[3] + acc[1];
	xdot[5] = (c - 2.0 * a) * x[2] + acc[2];
}

/* glonass position and velocity by numerical integration --------------------*/
void glorbit(double t, double* x, Vector3d& acc) 
{
	double k1[6], k2[6], k3[6], k4[6], w[6];
	int i;

	deq(x, k1, acc); for (i = 0; i < 6; i++) w[i] = x[i] + k1[i] * t / 2;
	deq(w, k2, acc); for (i = 0; i < 6; i++) w[i] = x[i] + k2[i] * t / 2;
	deq(w, k3, acc); for (i = 0; i < 6; i++) w[i] = x[i] + k3[i] * t;
	deq(w, k4, acc);

	for (i = 0; i < 6; i++)
		x[i] += (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i]) * t / 6;
}

int BS_geph2pos( GTime	time, Geph* geph, Vector3d& rs, double*	dts)
{
	double t = time - geph->toe;
	*dts	=  geph->taun 
			+  geph->gamn * t;

	double x[6] = {0};
	for (int i = 0; i < 3; i++)	
	{
		x[i  ] = geph->pos[i];
		x[i+3] = geph->vel[i];
	}

	for (double tt = t < 0 ? -GLOSTEP : GLOSTEP; fabs(t) > 1E-9; t -= tt)	
	{
		if (fabs(t) < GLOSTEP) tt=t;
		glorbit(tt, x, geph->acc);
	}

	for (int i = 0; i < 3; i++) rs(i) = x[i];
	
	return 0;
}

int BS_eph2pos( GTime	time, Eph* eph, Vector3d& rs, double*	dts) {

	if (eph->A <= 0) return -1;

	double tk = time - eph->toe;
	int prn = eph->Sat.prn;
	int sys = eph->Sat.sys;

	double mu;
	double omge;
	switch (sys)
	{
		case E_Sys::GAL: mu = MU_GAL; omge = OMGE_GAL; break;
		case E_Sys::BDS: mu = MU_CMP; omge = OMGE_CMP; break;
		default:     	 mu = MU_GPS; omge = OMGE;     break;
	}

	double M = eph->M0 + (sqrt(mu / (eph->A * eph->A * eph->A)) + eph->deln) * tk;

	double E	= M;
	double Ek	= 0;
	int n;
	for (n = 0; fabs(E - Ek) > RTOL_KEPLER && n < MAX_ITER_KEPLER; n++)
	{
		Ek = E;
		E -= (E - eph->e * sin(E) - M) / (1 - eph->e * cos(E));
	}

	if (n >= MAX_ITER_KEPLER)
	{
		fprintf(stderr,"WARNING for %s ephemeris: iteration divergent", eph->Sat.id().c_str());
		return -1;
	} 

	double sinE  = sin(E);
	double cosE  = cos(E);
	double u 	 = atan2(sqrt(1 - eph->e * eph->e) * sinE, cosE - eph->e) + eph->omg;
	double r 	 = eph->A * (1 - eph->e * cosE);
	double i	 = eph->i0 + eph->idot * tk;
	double sin2u = sin(2 * u);
	double cos2u = cos(2 * u);

	u 			+= eph->cus * sin2u + eph->cuc * cos2u;
	r 			+= eph->crs * sin2u + eph->crc * cos2u;
	i 			+= eph->cis * sin2u + eph->cic * cos2u;

	double x	 = r * cos(u);
	double y	 = r * sin(u);
	double cosi  = cos(i);

	/* beidou geo satellite */
	if (sys == +E_Sys::BDS && prn <= 5)
	{
		double O	= eph->OMG0
					+ eph->OMGd * tk
					- omge * eph->toes;
		double sinO = sin(O);
		double cosO = cos(O);
		double xg = x * cosO - y * cosi * sinO;
		double yg = x * sinO + y * cosi * cosO;
		double zg = y * sin(i);
		double sino = sin(omge * tk);
		double coso = cos(omge * tk);
		rs(0) = +xg * coso + yg * sino * COS_5 + zg * sino * SIN_5;
		rs(1) = -xg * sino + yg * coso * COS_5 + zg * coso * SIN_5;
		rs(2) = -yg * SIN_5 + zg * COS_5;
	}
	else
	{
		double O	= eph->OMG0
					+ (eph->OMGd - omge) * tk
					- omge * eph->toes;
		double sinO = sin(O);
		double cosO = cos(O);
		rs(0) = x * cosO - y * cosi * sinO;
		rs(1) = x * sinO + y * cosi * cosO;
		rs(2) = y * sin(i);
	}

	tk = time - eph->toc;
	*dts	= eph->f0
			+ eph->f1 * tk
			+ eph->f2 * tk * tk;
	return 0;
}


void print_SP3header(FILE* fpout)
{
	/* line one */
	fprintf(fpout,"#dP%4.0f %2.0f %2.0f %2.0f %2.0f %11.8f ", BS_Start_Epoc[0], BS_Start_Epoc[1], BS_Start_Epoc[2], BS_Start_Epoc[3], BS_Start_Epoc[4], BS_Start_Epoc[5]);
	fprintf(fpout,"%7d ORBIT WGS84 BCT   GA", BS_Epoch_Num);
	
	/* Line two */
	double mjdate = 7.0*BS_Start_Week + BS_Start_TOW/86400.0 + 44244.0;
	fprintf(fpout,"\n## %4d %15.8f %5.0f %15.13f", BS_Start_Week, BS_Start_TOW, mjdate, mjdate-floor(mjdate));
	
	/* satellite lines */
	int NumSatLeft = 85, satind=0, satinline=0;
	if(BS_Satel_List.size()>85) NumSatLeft = BS_Satel_List.size();
	auto it = BS_Satel_List.begin();
	while(satind < NumSatLeft || satinline > 0)
	{
		if(satind == 0) fprintf(fpout, "\n+  %3d   ", (int)BS_Satel_List.size());
		else if(satinline == 0) fprintf(fpout, "\n+        ");
		
		if(it==BS_Satel_List.end()) fprintf(fpout, "  0");
		else
		{
			fprintf(fpout,"%s",it->first.id().c_str());
			it++;
		}
		
		satind++;
		if(satinline<16) satinline++;
		else satinline = 0;
	}
	
	/* accuracy lines */
	satind = 0;
	satinline = 0;
	while(satind<NumSatLeft)
	{
		if(satinline == 0) fprintf(fpout, "\n++       ");
		
		fprintf(fpout, "  0");
		
		satind++;
		if(satinline<16) satinline++;
		else satinline = 0;
	}
	
	/* char variable lines */
	if(GNSSinp.size() > 1) fprintf(fpout, "\n%%c M ");
	else fprintf(fpout, "\n%%c %s ", GNSSinp.c_str());
	fprintf(fpout, " cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc");
	fprintf(fpout, "\n%%c cc cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc");
	
	/* float variable lines */
	fprintf(fpout, "\n%%f  1.2500000  1.025000000  0.00000000000  0.000000000000000");
	fprintf(fpout, "\n%%f  0.0000000  0.000000000  0.00000000000  0.000000000000000");
	
	/* float variable lines */
	fprintf(fpout, "\n%%i    0    0    0    0      0      0      0      0         0");
	fprintf(fpout, "\n%%i    0    0    0    0      0      0      0      0         0");
	
	fprintf(fpout, "\n/* Created using Ginan at: %s. ", timeget().to_string(0).c_str());
	fprintf(fpout, "\n/* WARNING: For Geoscience Australia's internal use only");	
}

void print_SP3epoch(FILE* fpout, GTime tsync, map<SatSys, vector<double>>& temp_eph)
{
	double ep[6];
	time2epoch(tsync, ep);
	fprintf(fpout,"\n*  %4.0f %2.0f %2.0f %2.0f %2.0f %11.8f ", ep[0], ep[1], ep[2], ep[3], ep[4], ep[5]);
	
	for(auto& [sat, neph] : BS_Satel_List){
		if(temp_eph.find(sat) == temp_eph.end()) 
			fprintf(fpout, "\nP%s%14.6f%14.6f%14.6f 999999.999999                    ", sat.id().c_str(), 0.0, 0.0, 0.0);
		else
		{
			auto vect = temp_eph[sat];
			fprintf(fpout, "\nP%s%14.6f%14.6f%14.6f%14.6f                    ", sat.id().c_str(), vect[0]/1000, vect[1]/1000, vect[2]/1000, vect[3]*1e6);	
		}
	}
}


int main(int argc, char **argv)
{
	boost::log::core::get()->set_filter (boost::log::trivial::severity >= boost::log::trivial::info);
	string atxfile = "igs14.atx";
	vector<string> navfiles;
	int GALSelection = 7;
	/* Argument parsing */
	string inpfile;
    string outfile = "BS_orbits.sp3";
    bool fstreq = false;
    BS_Max_Dtime[E_Sys::GPS] = 86400.0;
    BS_Max_Dtime[E_Sys::GLO] = 86400.0;
    BS_Max_Dtime[E_Sys::GAL] = 86400.0;
    BS_Max_Dtime[E_Sys::QZS] = 86400.0;
    BS_Max_Dtime[E_Sys::BDS] = 86400.0;
    int ephopt = 0;
    for (int i = 1; i < argc; i++) 
    {
        if      (!strcmp(argv[i], "-inp") && i+1 < argc)
        {
        	inpfile.assign(argv[++i]);
        	navfiles.push_back(inpfile);
        } 
        else if (!strcmp(argv[i], "-out") && i+1<argc) outfile.assign(argv[++i]);
        else if (!strcmp(argv[i], "-ant") && i+1<argc) atxfile.assign(argv[++i]);
        else if (!strcmp(argv[i], "-gns") && i+1<argc) GNSSinp.assign(argv[++i]);
        else if (!strcmp(argv[i], "-int") && i+1<argc) BS_Epoch_Inter = atof(argv[++i]);
    	else if (!strcmp(argv[i], "-Gdt") && i+1<argc) BS_Max_Dtime[E_Sys::GPS] = atof(argv[++i]);
    	else if (!strcmp(argv[i], "-Rdt") && i+1<argc) BS_Max_Dtime[E_Sys::GLO] = atof(argv[++i]);
    	else if (!strcmp(argv[i], "-Edt") && i+1<argc) BS_Max_Dtime[E_Sys::GAL] = atof(argv[++i]);
    	else if (!strcmp(argv[i], "-Jdt") && i+1<argc) BS_Max_Dtime[E_Sys::QZS] = atof(argv[++i]);
    	else if (!strcmp(argv[i], "-Cdt") && i+1<argc) BS_Max_Dtime[E_Sys::BDS] = atof(argv[++i]);
    	else if (!strcmp(argv[i], "-clst"))			   ephopt = 1;
    	else if (!strcmp(argv[i], "-inav"))            GALSelection = 5;
        else if (!strcmp(argv[i], "-fnav"))  		   GALSelection = 2;
        else if (!strcmp(argv[i], "-frst"))			   fstreq=true;
        else if (!strcmp(argv[i], "-help"))
        {
        	helpstring();
        	exit(0);
        } 
    }
	if(BS_Epoch_Inter <= 0.0) BS_Epoch_Inter = 900.0;
	
	bool pass = readantexf(atxfile, nav);
	if(!pass)
	{
		fprintf(stderr, "ERROR: invalid ANTEX file %s\n", atxfile.c_str());
		exit(0);
	}
	std::cout << "ATX file " << atxfile << std::endl;
	
	map<E_Sys,bool> GNSS_on;
    if (GNSSinp.find("A") != std::string::npos) GNSSinp = "GRECJ"; //All constellations
    if (GNSSinp.find("G") != std::string::npos) GNSS_on[E_Sys::GPS] = true; //GPS
    if (GNSSinp.find("R") != std::string::npos) GNSS_on[E_Sys::GLO] = true; //GLONASS
    if (GNSSinp.find("E") != std::string::npos) GNSS_on[E_Sys::GAL] = true; //Galileo
    if (GNSSinp.find("C") != std::string::npos) GNSS_on[E_Sys::BDS] = true; //Beidou
    if (GNSSinp.find("J") != std::string::npos) GNSS_on[E_Sys::QZS] = true; //QZSS
    if (GNSS_on.empty())
	{
        fprintf(stderr, "No GNSS selected\n");
        exit(0);
    }
	std::cout << "Processed GNSS " << GNSSinp << std::endl;
    
	char			type = 'N';
	ObsList 		obsList;
	RinexStation	sta;
	double			version = 0;
	E_Sys			sys;
	int 			tsys;
	map<E_Sys, vector<CodeType>>	sysCodeTypes;
	int 			nfil = 0;
	
	for (auto& file : navfiles)
	{
		std::ifstream	inputStream;
		inputStream.open(file, std::ifstream::in);
		int info = readrnx(inputStream, type , obsList, nav, &sta, version, sys, tsys, sysCodeTypes);
		if (info == false)
		{
			std::cout << "ERROR: invalid BRDC file " << file.c_str() << std::endl;
		}
		else{
			std::cout << "Processed file " << file << " " << type << " " << version << " " << std::endl;
			nfil++;		
			info = readrnx(inputStream, type , obsList, nav, &sta, version, sys, tsys, sysCodeTypes);
		} 
	}
	
	if(nfil == 0)
	{
		std::cout << "No valid Nav. files" << std::endl;
		exit(0);
	}
	
	FILE* sp3fp=fopen(outfile.c_str(),"wt");
	if(!sp3fp) 
	{
		std::cout << "Cannot open output file: " << outfile << std::endl;
		exit(0);
	}
	std::cout << "Output file: " << outfile << std::endl;
	
	GTime tstart = GTime::noTime();
	GTime tfinsh = GTime::noTime();
	
	for(auto& [satint,ephlist] : nav.ephMap)
	{
		SatSys sat;
		sat.fromHash(satint);
		if(!GNSS_on[sat.sys]) continue;
		int neph = 0;
		for(auto it = nav.ephMap[satint].begin(); it != nav.ephMap[satint].end();)
		{
			auto [dummy, eph] = *it;
			double dtoc = eph.toe - eph.toc;
			double dttr = eph.toe - eph.ttr;
			bool alert = false;
			
			if(dtoc !=0.0) alert = true;
			if(fabs(dttr)>10000.0) alert = true;
			
			if(sys == +E_Sys::GAL && !(eph.code & GALSelection)) alert=true;		/* INAVs messages, change from 5 to 2 for FNAVs (we dont want to mix the two) */
			
			if(alert)
			{
				it=nav.ephMap[satint].erase(it);
			}
			else
			{
				//fprintf(stdout, "%s, %s, %d, %.1f, %s\n", sat.id().c_str(), eph.toe.to_string(0).c_str(), eph.iode, dtoc, eph.ttr.to_string(0).c_str());
				neph++;
				if (tstart == GTime::noTime() || tstart > eph.toe)		tstart = eph.toe;
				if (tfinsh == GTime::noTime() || tfinsh < eph.toe)		tfinsh = eph.toe;
				it++;
			}
		}
		if(neph>0) BS_Satel_List[sat]=neph;
	}
	
	if(GNSS_on[E_Sys::GLO]) for(auto& [satint,gephlist] : nav.gephMap)
	{
		SatSys sat;
		sat.fromHash(satint);
		int neph = 0;
		for(auto it = nav.gephMap[satint].begin(); it != nav.gephMap[satint].end();){
			auto& [dummy, geph] = *it;
			double dtoc = geph.toe - geph.tof;
			bool alert = false;
			
			//if(dtoc !=0.0) alert=true;
			//if(fabs(dttr)>10000.0) alert=true;
			
			if(alert){
				it = nav.gephMap[satint].erase(it);
				continue;
			}
			//fprintf(stdout, "%s %s %d, %.1f, %d\n", sat.id().c_str(), geph.toe.to_string(0).c_str(), geph.iode, dtoc, geph.svh);
			
			neph++;
			if (tstart == GTime::noTime() || tstart > geph.toe ) 	tstart = geph.toe;
			if (tfinsh == GTime::noTime() || tfinsh < geph.toe ) 	tfinsh = geph.toe;
			it++;
		}
		
		if (neph > 0)
			BS_Satel_List[sat] = neph;
	}

	double StarTow	= time2gpst(tstart, &BS_Start_Week);
	//BS_Start_TOW	= BS_Epoch_Inter*floor(StarTow/BS_Epoch_Inter);
	BS_Start_TOW	= 3600.0 * floor(StarTow / 3600.0);
	GTime tsync 	= gpst2time(BS_Start_Week, BS_Start_TOW);
	BS_Epoch_Num	= (int)(floor((tfinsh - tsync)/BS_Epoch_Inter) + 1);
	time2epoch(tsync, BS_Start_Epoc);

	fprintf(stdout, "\nTstart= %s, Tini= %s, Nepoc=%d\n", tstart.to_string(0).c_str(), tsync.to_string(0).c_str(), BS_Epoch_Num);

	if(fstreq)for(auto it = BS_Satel_List.begin(); it != BS_Satel_List.end();)
	{
		SatSys sat = it->first;
		bool nofst = false;
		Vector3d rs(0, 0, 0);
		Vector3d dant(0, 0, 0);
		double dts;	
		if(sat.sys == +E_Sys::GLO)
		{
			Geph* gephp=BS_selgeph( tsync, sat, -1, nav);
			if(!gephp) nofst = true;
			else if(BS_geph2pos( tsync, gephp, rs, &dts) < 0) nofst = true;
			else if(rs.norm() < RE_WGS84) nofst = true;
			else if(BS_satantoff( tsync, sat, rs, dant, gephp->frq ) < 0) nofst = true;
			else nofst = false;
		}
		else
		{
			Eph* ephp=BS_seleph( tsync, sat, -1, nav);
			if(!ephp) nofst = true;
			else if(BS_eph2pos( tsync, ephp, rs, &dts) < 0) nofst = true;
			else if(rs.norm() < RE_WGS84) nofst = true;
			else if(BS_satantoff( tsync, sat, rs, dant, 0 ) < 0) nofst = true;
			else nofst = false;
		}
			
		if(nofst){
			it = BS_Satel_List.erase(it);
		}	
		else{
			++it;
		}
	}

	print_SP3header(sp3fp);
	
	for(int epc=0; epc<BS_Epoch_Num; epc++){
		fprintf(stdout, "\n%s", tsync.to_string(0).c_str());
		map<SatSys,vector<double>> temp_eph;
		for(auto& [sat,neph] : BS_Satel_List){
			Vector3d rs(0, 0, 0);
			Vector3d dant(0, 0, 0);
			double dts;
			
			if(sat.sys == +E_Sys::GLO){
				Geph* gephp=BS_selgeph( tsync, sat, -1, nav, ephopt);
				if(!gephp) continue;
				if(BS_geph2pos( tsync, gephp, rs, &dts) < 0) continue;
				if(rs.norm() < RE_WGS84) continue;
				if(BS_satantoff( tsync, sat, rs, dant, gephp->frq ) < 0) continue;
				fprintf(stdout, " %s ", sat.id().c_str());
			}
			else{
				Eph* ephp=BS_seleph( tsync, sat, -1, nav, ephopt);
				if(!ephp) continue;
				if(BS_eph2pos( tsync, ephp, rs, &dts) < 0) continue;
				if(rs.norm() < RE_WGS84) continue;
				if(BS_satantoff( tsync, sat, rs, dant, 0 ) < 0) continue;
				fprintf(stdout, " %s ", sat.id().c_str());
			}
			
			vector<double> sateph;
			sateph.push_back(rs(0) - dant(0));
			sateph.push_back(rs(1) - dant(1));
			sateph.push_back(rs(2) - dant(2));
			sateph.push_back(dts);
			
			//fprintf(stdout, "\n%s %11.6f %11.6f %11.6f", sat.id().c_str(), dant(0),dant(1),dant(2));
			
			temp_eph[sat] = sateph;
		}
		
		if (temp_eph.size() > 0) 
			print_SP3epoch(sp3fp, tsync, temp_eph);
		
		tsync = tsync + BS_Epoch_Inter;
	}
	
	fprintf(sp3fp, "\nEOF");
	
	return(EXIT_SUCCESS);
}
