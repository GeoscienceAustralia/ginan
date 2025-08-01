#include <string>
#include "common/gTime.hpp"

using std::string;

struct KFState;
struct Receiver;

void outputCost(string filename, KFState& kfState, Receiver& rec);
