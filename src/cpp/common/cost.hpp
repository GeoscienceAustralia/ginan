
#include <string>
#include "common/gTime.hpp"

struct KFState;
struct Receiver;

using std::string;

void outputCost(
	string		filename,
	KFState&	kfState,
	Receiver&	rec);

