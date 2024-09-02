
/** \file

High level operational flows for Ginan__()


*/

#include "architectureDocs.hpp"




FileType JSON__()
{

}


FileType IGS_Files__()
{
	DOCS_REFERENCE(CLK__);
	DOCS_REFERENCE(SP3__);
	DOCS_REFERENCE(BSX__);
	DOCS_REFERENCE(ATX__);
	DOCS_REFERENCE(SNX__);
	DOCS_REFERENCE(RNX__);
	DOCS_REFERENCE(OBX__);
}




Architecture State_Propagation__()
{
	DOCS_REFERENCE(Orbit_Integrator__);
}

Architecture Error_Handling__()
{

}

/** Linear algebra library.
 * A de-facto standard that provides scientific-familiar notation for linear algebra calculations.
 * Eigen automatically performs calculations over multiple processor cores when possible, vastly increasing comutation throughput.
 * It supports both dense and sparse matrices, slicing of subsets of matrices, and various decomposition and inversion methods - all of which are used extensively throughout Ginan.
 */
Library Eigen__()
{

}

/** C++ Extensions.
 * Boost is a de-facto standard library that includes extensions for the C++ STL.
 *
 * Ginan uses boost modules including
 * - Logging : Extensible logging with severity
 * - Runge Kutta : Differential equation propagators
 * - Binary Archive : Automatic binary storage/retrieval of complex variables and containers
 * - ASIO : TCP/IP networking
 */
Library Boost__()
{
	DOCS_REFERENCE(Binary_Archive__);
}

/** YAML file-format parser.
 * The Pea makes great use of the heirarchical structure provided by YAML configuration files. This library agnostically parses files, ensuring compatibility with the YAML standard,
 * and provides structured output for the Peas configuration functions to traverse.
 */
Library Yaml_Cpp__()
{

}

/** Analytic reference frame and timing routines.
 */
Library Sofa__()
{

}

Library JPL_Planetary_Ephemerides__()
{

}

/** C++ MongoDB API
 */
Library MongoCxx__()
{

}

/** Automated source code documentation generator.
 * In addition to generating documentation from source code, it is used to produce architectural flow-charts of high-level software features.
 */
Library Doxygen__()
{

}

/** GNSS processing toolkit.
 * Ginan was once based heavily on the RTKLib library, but has been converted to c++, and modified heavily to be object-oriented and suit the processing schemes used in Ginan.
 */
Library RTKLib__()
{

}

Library IERS2010__()
{

}

/** 3rd-party libraries extend functionality.
 *
 * Open source de-facto industry standard libraries are used to ensure long-term support into the future.
 * They provide functionality which is outside the scope of the Ginan project.
 *
 */
Architecture Libraries__()
{
	DOCS_REFERENCE(Eigen__);
	DOCS_REFERENCE(Yaml_Cpp__);
	DOCS_REFERENCE(Boost__);
	DOCS_REFERENCE(Sofa__);
	DOCS_REFERENCE(JPL_Planetary_Ephemerides__);
	DOCS_REFERENCE(IERS2010__);
	DOCS_REFERENCE(MongoCxx__);
	DOCS_REFERENCE(Doxygen__);
	DOCS_REFERENCE(RTKLib__);
}

Architecture Augmentation__()
{
	DOCS_REFERENCE(RTS_Smoothing__);
}

/** Ginan's main processing executable.
 * Written in C++, the Pea is a highly-configurable, application-specific, robust, kalman filter. It
 * - allows for vast configuration using the YAML configuration files,
 * - contains parsers for nearly all file-formats used in GNSS positioning,
 * - contains models and estimators for all major phyical phenomena affecting satellites, receivers, and the atmosphere between them,
 * - has an extended-kalman-filter, with automatic bookkeeping, for estimation of parameters of interest,
 * - can detect mismodelled states and measurements, and respond robustly to errors after attributing their source,
 * - is capable of smoothing results using optimal sets of stored available data, and
 * - can output nearly every applicable file format, making it a complete solution to GNSS processing.
 */
Architecture Pea__()
{
	DOCS_REFERENCE(Libraries__);
	DOCS_REFERENCE(Config__);
	DOCS_REFERENCE(Input_Files__);
	DOCS_REFERENCE(Streams_And_Synchronisation__);
	DOCS_REFERENCE(Preprocessing__);
	DOCS_REFERENCE(State_Propagation__);
	DOCS_REFERENCE(Main_Filter__);
	DOCS_REFERENCE(Outputs__);
}

/** Exploratory Data Analysis tool.
 * Allows the user to retrieve information from a mongo database for plotting.
 * The tool runs in a web-browser and allows for plotting of states, measurements and their residuals, and various time-series comparisons.
 * The datasets may be truncated to eliminate periods of unconverged data, and have polynomial trends calculated and removed.
 */
Architecture Ginan_EDA__()
{
	DOCS_REFERENCE(Mongo_Database__);
}

/** Download files required by processing.
 * The utility allows the user to specify a date range and data of interest which will be downloaded from CDDIS, GA archives, and other sources.
 */
Architecture Auto_Download__()
{

}

/** Utility scripts and visualisations
 */
Architecture Python_Scripts__()
{
	DOCS_REFERENCE(Ginan_EDA__);
	DOCS_REFERENCE(Auto_Download__);
}

/** Ginan is a toolkit for processing GNSS measurements to produce precise positioning products.
 * As a system it comprises a C++ executable known as the Pea, with additional scripts and databases used for visualisation and post-processing data.
 */
Architecture Ginan__()
{
	DOCS_REFERENCE(Pea__);
	DOCS_REFERENCE(Mongo_Database__);
	DOCS_REFERENCE(Python_Scripts__);
}
