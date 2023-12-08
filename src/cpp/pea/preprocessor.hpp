
#pragma once

struct Network;
struct Station;

void preprocessor(
	Network&	net,
	Station&	rec,
	bool		realEpoch = false);

void obsVariances(
	ObsList& obsList);

