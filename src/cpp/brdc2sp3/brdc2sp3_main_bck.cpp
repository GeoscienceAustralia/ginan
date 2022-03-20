#include <unordered_map>
#include <sys/time.h>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <memory>
#include <chrono>
#include <thread>
#include <string>

#include "omp.h"

using namespace std::literals::chrono_literals;
using std::chrono::system_clock;
using std::chrono::time_point;
using std::this_thread::sleep_for;
using std::string;

#include <boost/log/utility/setup/console.hpp>
#include <boost/log/trivial.hpp>
#include <boost/filesystem.hpp>

#include "acsNtripBroadcast.hpp"
#include "minimumConstraints.hpp"
#include "networkEstimator.hpp"
#include "peaCommitVersion.h"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "corrections.hpp"
#include "streamTrace.hpp"
#include "writeClock.hpp"
#include "acsConfig.hpp"
#include "acsStream.hpp"
#include "ntripSocket.hpp"
#include "testUtils.hpp"
#include "biasSINEX.hpp"
#include "station.hpp"
#include "summary.hpp"
#include "antenna.hpp"
#include "preceph.hpp"
#include "satStat.hpp"
#include "common.hpp"
#include "orbits.hpp"
#include "gTime.hpp"
#include "mongo.hpp"
#include "debug.hpp"
#include "enums.h"
#include "ppp.hpp"
#include "snx.hpp"
#include "trop.h"
#include "vmf3.h"

static const char *help[]={
    "",
    " usage: brdc2sp3 -inp navfile -atx navfile [-out sp3file] [-int time_int] [-gns constel]",
    "",
    " -?        print help",
    " -inp navfile   input navigation file [required]",
    " -atx atxfile   antenna files",
    " -out sp3file   output sp3file [brdc%W%D.sp3]",
    " -int time_int  time interval of SP3 output",
    " -gns constel   Constellation in SP3 files, 'A': all, 'G':GPS, 'R':GLONASS, 'E':Galileo, 'C': Beidou, 'J': QZSS ['GR']"
};

static void printhelp(void)
{
    int i;
    for (i=0;i<(int)(sizeof(help)/sizeof(*help));i++) fprintf(stderr,"%s\n",help[i]);
    exit(0);
}

nav_t		nav		= {};

int main(int argc, char **argv)
{
    string inpfile;
    string outfile="brdc%W%D.sp3";
    string GNSSinp="GR";
    map<E_Sys,bool> GNSS_on;
    double tint=300.0;
    int n=0;
    for (int i=1, n=0;i<argc;i++) {
        if      (!strcmp(argv[i],"-inp")&&i+1<argc) inpfile.assign(argv[++i]);
        else if (!strcmp(argv[i],"-out")&&i+1<argc) outfile.assign(argv[++i]);
        else if (!strcmp(argv[i],"-gns")&&i+1<argc) GNSSinp.assign(argv[++i]);
        else if (!strcmp(argv[i],"-int")&&i+1<argc) tint=atof(argv[++i]);
        else if (*argv[i]=='-') printhelp();
    }
   
    if(inpfile.empty()){
        fprintf(stderr,"Input file empty\n");
        exit(0);
    }
   
    if (GNSSinp.find("A")!=std::string::npos) GNSSinp="GRECJ"; //All constellations
    if (GNSSinp.find("G")!=std::string::npos) GNSS_on[E_Sys::GPS]=true; //GPS
    if (GNSSinp.find("R")!=std::string::npos) GNSS_on[E_Sys::GLO]=true; //GLONASS
    if (GNSSinp.find("E")!=std::string::npos) GNSS_on[E_Sys::GAL]=true; //Galileo
    if (GNSSinp.find("C")!=std::string::npos) GNSS_on[E_Sys::BDS]=true; //Beidou
    if (GNSSinp.find("J")!=std::string::npos) GNSS_on[E_Sys::QZS]=true; //QZSS
    if (GNSS_on.size()==0){
        fprintf(stderr,"No GNSS selected\n");
        exit(0);
    }
  
    std::cout << "BRDC2SP3 looking for " << GNSSinp << " satellites in" << inpfile << std::endl;
   
    //FileRinexStream rinexStream(inpfile);
    //rinexStream.parse();
}
