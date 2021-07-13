/*!
 * Functions to compute the load
 * @author Sébastien Allgeyer
 * @date 26/2/21
 *
 */

#include "loading.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>
#include <cmath>

const double PI = std::atan(1.0)*4;


using namespace std;

double interpolate( vector<double> &xData, vector<double> &yData, double x, bool extrapolate )
{
	int size =  xData.size();
	int i = 0;// find left end of interval for interpolation

	if (x==0) return (double) 0.0;
	if ( x >= xData[size - 2] )                                                 // special case: beyond right end
	{
		i = size - 2;
	}
	else
	{
		while ( x > xData[i+1] ) i++;
	}
	double xL = xData[i], yL = yData[i], xR = xData[i+1], yR = yData[i+1];      // points on either side (unless beyond ends)
	if ( !extrapolate )                                                         // if beyond ends of array and not extrapolating
	{
		if ( x < xL ) yR = yL;
		if ( x > xR ) yL = yR;
	}
	double dydx = ( yR - yL ) / ( xR - xL );                                    // gradient
	double ret = yL + dydx * ( x - xL );
	return  ret;                                            // linear interpolation
};

loading::loading() {};

loading::loading(std::string fname):
		fileName(fname)
{
	return ;
};

void loading::set_name(std::string name) {fileName=name; return;};

void loading::read() {
	n_green = 50; //@TODO read from the file.
	dist.reserve(n_green);
	Gz.reserve(n_green);
	Gh.reserve(n_green);
	ifstream infile(fileName);
	string line;
	int i=0;
	while (getline(infile, line))
	{
		istringstream iss(line);
		double d, gz, gzh, gh, ghz;
		if (!(iss >> d >> gz >> gzh >> gh >> ghz)) { break; } // error
		dist.push_back(d ) ;
		Gz.push_back(gz / (6371e3 * dist[i] * PI/180.0) * 1e-12)   ;
		Gh.push_back(gh / (6371e3 * dist[i] * PI/180.0) * 1e-12)   ;
		dist[i] *= PI/180.0;
		//cout << dist[i] << " " << Gz[i] << " " <<Gh[i] <<"\n";
		i++;
	}
	return ;
}

double loading::interpolate_gz(double x) {
	return interpolate( dist, Gz,  x, false );
}

double loading::interpolate_gh(double x) {
	return interpolate( dist, Gh,  x, false );
}