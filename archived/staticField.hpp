//
// Created by Sébastien Allgeyer on 24/2/22.
//

#ifndef GINAN_STATICFIELD_H
#define GINAN_STATICFIELD_H

#include <string>

using std::string;

#include <eigen3/Eigen/Core>
#include <eigen3/Eigen/Dense>

/** Structure for variable and function related to the static gravity field
 * @todo time variable static gravity field
 */
struct StaticField {
	StaticField(){};
	StaticField(
			std::string     filename_,
			int             degmax_ );
	void readegm();
	void summary();
	string filename;
	Eigen::MatrixXd gfct;
	Eigen::MatrixXd gfctC;
	Eigen::MatrixXd gfctS;
	int degMax;
	string modelName;
	double earthGravityConstant;
	double earthRadius;
	int maxDegree;
	bool tideFree;
	string norm;
};


#endif //GINAN_STATICFIELD_H
