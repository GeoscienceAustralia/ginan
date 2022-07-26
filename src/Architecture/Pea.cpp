

/** Read a .yaml formatted configuration file.
 * The Pea has a multitude of options that can be set for any use-cases.
 * To see a list of available options in the heirarchy, their default values, and descriptions, execute `./pea -Y`
 * Example configurations for different Pea use-cases can be seen in the high-level Configure_X blocks.
 */
void Pea_Configuration()
{
	
}

/** Provide satellite orbit data
 */
void SP3_Files(){}

/** Provide antenna phase center and offset data
 */ 
void ANTEX_Files(){}

/** Provide receiver station coordinates and metadata
 */
void SINEX_Files(){}

/** Provide GNSS observation data from human-readable files
 */
void RINEX_Files(){}

/** Provide GNSS observation data from low-bandwidth binary streams
 */
void RTCM_Streams(){}

/** Data files are checked for new data each epoch before processing commences.
 * Synchronisation between observation sources is performed to align measurements to a common epoch.
 */
void Input_Data()
{
	SP3_Files();
	ANTEX_Files();
	SINEX_Files();
	RINEX_Files();
	RTCM_Streams();
}

/** Used to detect cycle slips between distinct sets of observations
 */
void Slip_Detection()
{
	
}

/** Used to initialise unknown parameters
 */
void Least_Squares()
{
	
}

/** Perform simple quality control checks and positioning routines.
 */
void Preprocessor()
{
	Slip_Detection();
	Single_Point_Positioning();
}

void Compute_Measurement_Residuals()
{
	
}

void Statistical_Checks()
{
	
}

void Model_Error_Remediation()
{
	
}

void Output_RTS_Precursors()
{
	
}

void Output_Mongo_Data()
{
	
}

void State_Update()
{
	
}

/** Used to combine measurements and apriori values to produce an 'optimal' estimation of parameters
 */
void Kalman_Filter()
{
	Output_RTS_Precursors();
	Output_Mongo_Data();
	
	Least_Squares();
	Statistical_Checks();
	Model_Error_Remediation();
	State_Update();
}

/** Determine the contribution to pseudoranges from physical distances
 */
void Model_Geometry()
{
	
}

/** Determine the contribution to pseudoranges from refractive delays due to the atmosphsere
 */
void Model_Atmosphere()
{
	
}

/** Determine the contribution to pseudoranges from effects as specified by correction products
 */
void Apply_Corrections()
{
	
}

/** Estimate the integer number of cycles present in each phase measurement that provides a coherent system of measurements.
 */
void Ambiguity_Resolution()
{
	
}

void Orbit_Integration()
{
	
}

void State_Kinematics_And_Process_Noise()
{
	
}

void State_Removal()
{
	
}

void State_Transition_Predictions()
{
	State_Removal();
	State_Kinematics_And_Process_Noise();
	Orbit_Integration();
}

void Measurement_Augmentation()
{
	
}

/** Use precise scientific models and corrections from analysis centers to produce the best possible parameter estimates.
 */
void Precise_Point_Positioning()
{
	State_Transition_Predictions();

	Model_Geometry();
	Model_Atmosphere();
	Apply_Corrections();
	Compute_Measurement_Residuals();
	
	Measurement_Augmentation();
	
	Kalman_Filter();
}

/** Use simple geometric and atmospheric models to estimate parameters
 * \copydoc sppos()
 */
void Single_Point_Positioning()
{
	Model_Geometry();
	Model_Atmosphere();
	
	Least_Squares();
}

/** Output results to standardised file formats
 */
void Output_To_Files()
{
	SP3_Files();
	SINEX_Files();
}

/** Output results to standardised stream formats
 */
void Output_To_Caster()
{
	RTCM_Streams();
}

/** Output results
 */
void Output_Pea_Products()
{
	Output_To_Files();
	Output_To_Caster();
}

void Minimum_Constraints()
{
	
}

void RTS_Smoothing()
{
	
}

void Solution_Augmentation()
{
	Ambiguity_Resolution();
	Minimum_Constraints();
	RTS_Smoothing();
}

/** Use available data to estimate unknown parameters of interest.
 * The Pea is an application-specific kalman-filter estimator. It comprises functions to read and write many 
 * standardised GNSS products, and models of phenomena that apply to orbits, positions, and signals related to GNSS positioning.
 */
void Pea()
{
	Pea_Configuration();
	Input_Data();
	Preprocessor();
	Precise_Point_Positioning();
	Solution_Augmentation();
	Output_Pea_Products();
}


/** The Pea is used to estimate the errors in predicted orbits.
 * The estimated error values can be re-introduced into the Pod to produce more accurate orbit estimates.
 */
void Pea_Orbit_Corrections()
{
	Pea();
}

/** The Pea is used to estimate the values of receiver and sattelite clocks, and the internal biases that correspond to these estimates.
 * With precise estimates of biases and clocks, receivers not included in the calculations can improve their positioning accuracy by eliminating uncertainty in the parameters used.
 */
void Pea_Clock_And_Bias_Corrections()
{
	Pea();
}


void Download_ANTEX_Files()
{
	ANTEX_Files();
}

void Download_SP3_Files()
{
	SP3_Files();
}

void Configure_Orbit_Corrections_Pea()
{
	Pea_Configuration();
}

void Correct_Orbits_With_Pea()
{
	Pea();
}

void Configure_Clock_Corrections_Pea()
{
	Pea_Configuration();
}

void Estimate_Clocks_With_Pea()
{
	Pea();
}

