#pragma once

#include <iosfwd>

#include "common/eigenIncluder.hpp"
#include "common/enums.h"
#include "common/gTime.hpp"
#include "common/zhangIarGainAudit.hpp"

struct KFState;
using Trace = std::ostream;

/** Run the feedback-free E29-A2/B2 real-network mathematical closure audit
 * on the exact posterior and integer rows already constructed by E24a. */
bool traceZhangE29RealMathClosure(
	Trace& trace,
	const KFState& posterior,
	E_Sys system,
	GTime time,
	const MatrixXd& covarianceF0,
	const MatrixXd& covarianceWideLane,
	const ZhangIarFunctional& parConstraints,
	const ZhangIarFunctional& fullConstraints);
