/*!
 * Functions to compute the load
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */

#pragma once

#include "tide.h"
#include "input_otl.h"
#include "loading.h"

void load_1_point(tide *tide_info, otl_input *input, loading load,  int idx);
void write_BLQ(otl_input *input);
void write_BLQ(otl_input *input, int code);


