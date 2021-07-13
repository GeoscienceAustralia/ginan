
#ifndef __RTS_SMOOTHING_HPP__
#define __RTS_SMOOTHING_HPP__

#include <map>

using std::map;

#include "algebra.hpp"


KFState RTS_Process(
	KFState&	kfState, 
	bool		write = false);

void RTS_Output(
	KFState&	kfState,
	string		clockFilename = "");


#endif
