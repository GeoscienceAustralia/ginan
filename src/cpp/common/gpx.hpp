
#pragma once

struct KFState;
struct Receiver;

void writeGPX(
	string		filename,
	KFState&	kfState,
	Receiver&	rec);
