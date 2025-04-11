
#pragma once

#include <mutex>

using std::mutex;

#include "common/constants.hpp"
#include "common/trace.hpp"
#include "common/gTime.hpp"

struct SBASMessage
{
	int				prn		= -1;
	int				freq	= 1;
	int				type	= 63;
	string			message;
	unsigned char	data[32];
};

extern mutex					sbasMessagesMutex;
extern map<GTime, SBASMessage>	sbasMessages;


void writeEMSdata(
	Trace&	trace,
	string	biasfile);

