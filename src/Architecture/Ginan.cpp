
/** \file

High level operational flows for Ginan__()


*/

#include "architectureDocs.hpp"


/** Retrieve data from streams and process together once per epoch.
 *
 * In order to be capable of real-time and post-processed execution, the pea treats all observation inputs as streams.
 * These streams are created with a stream source type, and a parser, with the separation of these strategies allowing for far more reusable code,
 * and immediate extension of inputs from simply files, to Files, HTTP, TCP, Serial, Pipes etc.
 *
 * The fundamental tick of the pea is the `epoch_interval`, which drives all major processing steps.
 * Once an initial epoch has been designated the pea requests data for the next epoch from each stream.
 * The streams either respond with the data, or with a flag indicating the data may be available later, or will never be available.
 *
 * Various configuration parameters define the exact flow through the synchronisation code, however once all data is available, or no more is expected,
 * or a timeout has occurred, the epoch is considered synchronised and processing is performed.
 *
 * Unlike observation streams, which provide data up until the requested epoch time, other streams such as navigation streams parse all data available to them,
 * and output any values to global maps, which may be later accessed as required by other components.
 *
 * The synchronisation process parses all streams sequentially, without multithreading, so map collisions are not expected.
 */
Architecture Streams_And_Synchronisation__()
{

}


FileType SP3__()
{

}

FileType ATX__()
{

}

FileType SNX__()
{

}

FileType OBX__()
{

}

FileType RNX__()
{

}

FileType IGS_Files__()
{
	DOCS_REFERENCE(SP3__);
	DOCS_REFERENCE(ATX__);
	DOCS_REFERENCE(SNX__);
	DOCS_REFERENCE(RNX__);
	DOCS_REFERENCE(OBX__);
}

Input Input_Files__()
{
	DOCS_REFERENCE(IGS_Files__);
}

Architecture Preprocessing__()
{

}

Architecture State_Propagation__()
{

}

/** Kalman Filter.
 *
 * $$ K = HPH^\intercal + R $$ fgdfg
 */
ParallelArchitecture Kalman_Filter__()
{
	DOCS_REFERENCE(Binary_Archive__);
}

Architecture Error_Handling__()
{

}

ParallelArchitecture UDUC_Measurements__()
{

}

Architecture Combinators__()
{

}

Output Outputs__()
{
	DOCS_REFERENCE(Trace_Files__);
	DOCS_REFERENCE(IGS_Files__);
	DOCS_REFERENCE(Mongo_Database__);
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

/** YAML configuration.
 */
Library Yaml_Cpp__()
{

}

Library Sofa__()
{

}

/** 3rd-party libraries extend functionality.
 */
Architecture Libraries__()
{
	DOCS_REFERENCE(Eigen__);
	DOCS_REFERENCE(Yaml_Cpp__);
	DOCS_REFERENCE(Boost__);
	DOCS_REFERENCE(Sofa__);
}

Architecture Augmentation__()
{
	DOCS_REFERENCE(RTS_Smoothing__);
}

/** Ginan's main processing executable.
 * The Pea is a highly-configurable, application-specific, robust, kalman filter. It
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


Architecture Python_Scripts__()
{
	DOCS_REFERENCE(Ginan_EDA__);
	DOCS_REFERENCE(Auto_Download__);
}

/** Ginan is a toolkit for processing GNSS measurements to produce precise positioning products
 */
Architecture Ginan__()
{
	DOCS_REFERENCE(Pea__);
	DOCS_REFERENCE(Mongo_Database__);
	DOCS_REFERENCE(Python_Scripts__);
}
