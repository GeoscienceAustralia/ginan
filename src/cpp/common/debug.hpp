
#ifndef __DEBUG_HPP__
#define __DEBUG_HPP__


void artificialSlip(
	Trace&	trace,
	Obs&	obs,
	lc_t&	lcBase,
	char*	strprefix);

void	doDebugs();
#endif


#ifndef CSVTOMATRIX_H
#define CSVTOMATRIX_H

#include <iostream>
#include <vector>
#include <map>
#include <eigen3/Eigen/Dense>
#include <fstream>
#include<string>
#include<sstream>
#include<stdio.h>

using namespace Eigen;
using namespace std;

template<class M>
class csvToMatrix
{
public :
	csvToMatrix();
	M load_csv( const string) ;



};
#endif // CSVTOMATRIX_H

