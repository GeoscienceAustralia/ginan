
#pragma once

struct KFState;
struct Receiver;

void writePOS(
	string		filename,
	KFState&	kfState,
	Receiver&	rec);
