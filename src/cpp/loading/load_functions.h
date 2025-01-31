/*!
 * Functions to compute the load
 * @author Sébastien Allgeyer
 * @date 5/3/21
 *
 */

#pragma once

#include "loading/tide.h"
#include "loading/input_otl.h"
#include "loading/loading.h"

void load_1_point(tide *tide_info, otl_input *input, loading load,  int idx);
void write_BLQ(otl_input *input);
void write_BLQ(otl_input *input, int code);


