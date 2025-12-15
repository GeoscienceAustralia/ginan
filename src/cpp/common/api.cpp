// #pragma GCC optimize ("O0")

#include "common/api.hpp"

vector<apiCallback> oncePerEpochCallbacks;

void callbacksOncePerEpoch()
{
    for (auto& callback : oncePerEpochCallbacks)
    {
        callback();
    }
}
