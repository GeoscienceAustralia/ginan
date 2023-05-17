//
// Created by Sébastien Allgeyer on 11/3/22.
//

#ifndef CODE_TESTING_MAINPOD_HPP
#define CODE_TESTING_MAINPOD_HPP

#include <atomic>
#include <mutex>
#include "orbit.hpp"
#include "gTime.hpp"
#include "enums.h"
#include "eigenIncluder.hpp"
#include "stateSV.hpp"

using namespace std;

const E_ThirdBody orbiting = E_ThirdBody::EARTH;

struct mainPod {
public:
	static mainPod* Instance(int argc, char * argv[]);
	static mainPod* Instance(){ return pInstance;};
	orb_t orbit_data;
	void run();
	std::map<E_ThirdBody, Vector6d> planets;
	std::map<const std::string, struct stateSV> satellite;
	GTime currentTime;
    double currentTime_d;

private:
	mainPod(int argc, char * argv[]) {init( argc,  argv);};
	void init(int argc, char* argv[]);
	static atomic<mainPod*> pInstance;
	static mutex m_;
	void oncePerEpoch();
	void oncePerEpochPerSV();


};


#endif //CODE_TESTING_MAINPOD_HPP
