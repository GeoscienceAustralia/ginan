
#ifndef WRITECLK_HPP
#define WRITECLK_HPP


struct KFState;

void outputReceiverClocks(
	string&		filename,
	KFState&	kfState,
	double*		epoch);

void outputSatelliteClocks(
	string&		filename,
	KFState&	kfState,
	double*		epoch);

void outputClockfileHeader(
	string&		filename,
	KFState&	kfState,
	double*		epoch);


#endif
