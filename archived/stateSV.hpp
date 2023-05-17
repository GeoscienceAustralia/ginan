//
// Created by Sébastien Allgeyer on 15/3/22.
//

#ifndef CODE_TESTING_STATESV_HPP
#define CODE_TESTING_STATESV_HPP

#include "eigenIncluder.hpp"
#include <string>
#include <map>

struct stateSV {
	stateSV();
	Vector3d posI;
	Vector3d velI;
	Vector3d accI;
	Vector6d posVelI;

	Vector3d posE;
	Vector3d velE;
	std::string name;

	void operator()(Vector6d & init, Vector6d &update, const double mjdinsec);

	void computeAcceleration(Vector6d & init, const double mjdinsec);
//	void set_in
};



#endif //CODE_TESTING_STATESV_HPP
