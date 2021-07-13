
#ifndef __TIDES_HPP__
#define __TIDES_HPP__

#include "eigenIncluder.hpp"
#include "streamTrace.hpp"
#include "gTime.hpp"


//forward declaration
struct erp_t;

/* earth tide models ---------------------------------------------------------*/

void sunmoonpos(GTime tutc, const double *erpv, double *rsun, double *rmoon, double *gmst);
void tidedisp(Trace& trace, GTime tutc, Vector3d&rr, const erp_t *erp,  const double *odisp, Vector3d& dr);



#endif
