
#pragma once

struct Network;
struct Receiver;

void preprocessor(
	Network&	net,
	Receiver&	rec,
	bool		realEpoch = false);

void obsVariances(
	ObsList& obsList);

