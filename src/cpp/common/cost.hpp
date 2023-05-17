
#include <string>
#include "gTime.hpp"

struct KFState;
struct Station;

using std::string;

void outputCost(
	string		filename,
	Station&	rec,
	GTime		time,
	KFState&	kfState);

