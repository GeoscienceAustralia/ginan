
#pragma once

struct Receiver;

void preprocessor(
	Trace&		trace,
	Receiver&	rec,
	bool		realEpoch = false);

void obsVariances(
	ObsList& obsList);

