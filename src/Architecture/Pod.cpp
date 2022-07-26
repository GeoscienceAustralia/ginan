

typedef void* input;

void Pod_Configuration()		///< Yaml formatted file containing options for processing data
{
	
}


void Produce_Orbits_From_Pseudo_Observations()
{
	
}

void Produce_Orbits_From_Corrections()
{
	
}

void Pod_Run_1()
{
	Pod_Configuration();
	
	
	Produce_Orbits_From_Pseudo_Observations();
	
}

void	Configure_Orbit_Corrections_Pod()
{
	Pod_Configuration();
}

void Pod_Run_2()
{
	Pod_Configuration();
	
	Produce_Orbits_From_Corrections();
}

/** Use available data to estimate the precise orbit trajectories of satellites.
 * The Pod comprises precise models of forces and accelerations that determine the path of orbiting satellites.
 * It can use pseudoobservations to estimate initial contitions for orbits, or propagate forward and backward from initial
 * contidtions to produce position estimates at any point in time.
 */
void Pod()
{
	Pod_Configuration();
}
void Configure_Orbit_Propagation_Pod()
{
	Pod_Configuration();
}
void Propagate_Orbits_With_Pod()
{
	Pod();
}
void Propagate_Corrected_Orbits_With_Pod()
{
	Pod();
}

