
#include <string>
#include "gTime.hpp"

struct KFState;
struct Station;

using std::string;

void outputCost(
	string		filename,
	KFState&	kfState,
	Station&	rec);

