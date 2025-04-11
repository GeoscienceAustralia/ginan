
#pragma once

struct Receiver;

void preprocessor(
	Trace&		trace,
	Receiver&	rec,
	bool		realEpoch	= false,
	KFState*	kfState_ptr	= nullptr,
	KFState*	remote_ptr	= nullptr);

void obsVariances(
	ObsList& obsList);

