

// #pragma GCC optimize ("O0")

#include "api.hpp"


vector<apiCallback> oncePerEpochCallbacks;

void callbacksOncePerEpoch()
{
	for (auto& callback : oncePerEpochCallbacks)
	{
		callback();
	}
}
