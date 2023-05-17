
#pragma once

#include <vector>

using std::vector;

typedef void (*apiCallback)	();

extern vector<apiCallback> oncePerEpochCallbacks;

void callbacksOncePerEpoch();

