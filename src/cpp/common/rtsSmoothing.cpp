
// #pragma GCC optimize ("O0")

#include <map>

using std::map;

#include <boost/log/trivial.hpp>

#include "rinexNavWrite.hpp"
#include "eigenIncluder.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.hpp"
#include "metaData.hpp"
#include "algebra.hpp"
#include "ppp.hpp"
#include "gpx.hpp"

void postRTSActions(
	bool		final,				///< This is a final answer, not intermediate - output to files
	KFState&	kfState,			///< State to get filter traces from
	StationMap*	stationMap_ptr)		///< Pointer to map of stations
{
	if	(	final
		&&	acsConfig.output_erp)
	{
		writeERPFromNetwork(kfState.metaDataMap[ERP_FILENAME_STR + SMOOTHED_SUFFIX], kfState);
	}

	if	(   final
		&&  acsConfig.output_clocks
		&&( acsConfig.clocks_receiver_source	== +E_Ephemeris::KALMAN
		  ||acsConfig.clocks_satellite_source	== +E_Ephemeris::KALMAN))
	{
		auto kfState2 = kfState;	//todo aaron, delete this after fixing something else, tryPrepareFilterPointers damages the state
		tryPrepareFilterPointers(kfState2, stationMap_ptr);

		auto filenameSysMap = getSysOutputFilenames(acsConfig.clocks_filename + SMOOTHED_SUFFIX, kfState2.time);		//todo aaron, this sucks

		for (auto [filename, sysMap] : filenameSysMap)
		{
			outputClocks(filename, acsConfig.clocks_receiver_source, acsConfig.clocks_satellite_source, kfState2.time, sysMap, kfState2, stationMap_ptr);
		}
	}

	if	(	final
		&&	acsConfig.output_trop_sinex
		&&	acsConfig.trop_data_source == +E_Ephemeris::KALMAN)
	{
		outputTropSinex(kfState.metaDataMap[TROP_FILENAME_STR + SMOOTHED_SUFFIX], kfState.time, *stationMap_ptr, "MIX", true);		//todo aaron, no site specific version here
	}

	if (final)
	{
		std::ofstream ofs(kfState.metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX], std::ofstream::out | std::ofstream::app);
		kfState.outputStates(ofs, " RTS");
	}

	if	(   acsConfig.output_mongo_states
		&&( final
		  ||acsConfig.output_intermediate_rts))
	{
		mongoStates(kfState, acsConfig.mongo_rts_suffix);
	}
	
	if (acsConfig.output_gpx)
	{
		writeGPX(kfState.metaDataMap[GPX_FILENAME_STR + SMOOTHED_SUFFIX], "", kfState);
	}

// 	pppoutstat(ofs, archiveKF, true);
}

/** Output filter states from a reversed binary trace file
*/
void RTS_Output(
	KFState&	kfState,			///< State to get filter traces from
	StationMap*	stationMap_ptr)		///< Pointer to map of stations
{
	string reversedStatesFilename = kfState.rts_basename + BACKWARD_SUFFIX;
	
	long int startPos = -1;
	
	BOOST_LOG_TRIVIAL(info) 
	<< "Outputting RTS products...";
	
	map<string, string> metaDataMap = kfState.metaDataMap;
	
	while (1)
	{
		E_SerialObject type = getFilterTypeFromFile(startPos, reversedStatesFilename);

		BOOST_LOG_TRIVIAL(debug) 
		<< "Outputting " << type._to_string() << " from file position " << startPos << std::endl;
		
		switch (type)
		{
			default:
			{
				std::cout << "UNEXPECTED RTS OUTPUT TYPE";
				return;
			}		
			
			case E_SerialObject::METADATA:
			{
				bool pass = getFilterObjectFromFile(type, metaDataMap, startPos, reversedStatesFilename);
				if (pass == false)
				{
					std::cout << "BAD RTS OUTPUT read";
					return;
				}
				
				break;
			}
			
			case E_SerialObject::MEASUREMENT:
			{
				KFMeas archiveMeas;
				bool pass = getFilterObjectFromFile(type, archiveMeas, startPos, reversedStatesFilename);
				if (pass == false)
				{
					std::cout << "BAD RTS OUTPUT read";
					return;
				}
				
				std::ofstream ofs(kfState.metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX], std::ofstream::out | std::ofstream::app);
				if (!ofs)
				{
					std::cout << "BAD RTS Write to " << kfState.metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX];
					break;
				}
				
				if (acsConfig.output_residuals)
				{
					outputResiduals(ofs, archiveMeas, -1, 0, archiveMeas.obsKeys.size());
				}
				
				if (acsConfig.output_mongo_measurements)
				{
					mongoMeasResiduals(archiveMeas.time, archiveMeas.obsKeys, archiveMeas.V, archiveMeas.VV, archiveMeas.R, acsConfig.mongo_rts_suffix);
				}
				
				break;
			}

			case E_SerialObject::FILTER_PLUS:
			{
				KFState archiveKF;
				bool pass = getFilterObjectFromFile(type, archiveKF, startPos, reversedStatesFilename);
				
				if (pass == false)
				{
					std::cout << "BAD RTS OUTPUT READ";
					return;
				}
				
				archiveKF.metaDataMap = metaDataMap;
				
				archiveKF.dx = VectorXd::Zero(archiveKF.x.rows());
				
				if (acsConfig.ambrOpts.NLmode != +E_ARmode::OFF)
				{
					TempDisabler td(acsConfig.output_mongo_measurements);
				
					KFState ARRTScopy = archiveKF;
					int nfix = smoothdAmbigResl(ARRTScopy);
					postRTSActions(true, ARRTScopy, stationMap_ptr);
				}
				else
				{
					postRTSActions(true, archiveKF, stationMap_ptr);
				}
				
				break;
			}
		}

		if (startPos == 0)
		{
			return;
		}
		if (startPos < 0)
		{
			BOOST_LOG_TRIVIAL(error) 
			<< "Oopsie " << std::endl;
			
			return;
		}
	}
}

KFState RTS_Process(
	KFState&	kfState,
	bool		write,
	StationMap*	stationMap_ptr)
{
	if (kfState.rts_lag == 0)
	{
		return KFState();
	}
	
	MatrixXd transitionMatrix;

	KFState	kalmanMinus;
	KFState	smoothedKF;
	KFMeas	measurements;

	bool smoothedXready = false;
	bool smoothedPready = false;

	string inputFile	= kfState.rts_basename + FORWARD_SUFFIX;
	string outputFile	= kfState.rts_basename + BACKWARD_SUFFIX;

	if (write)
	{
		std::ofstream ofs(outputFile,	std::ofstream::out | std::ofstream::trunc);
	}

	long int startPos = -1;
	int lag = 0;					//todo aaron, change lag to use times rather than iterations, iterations may be multiple per epoch
	while (lag != kfState.rts_lag)
	{
		E_SerialObject type = getFilterTypeFromFile(startPos, inputFile);

// 		std::cout << std::endl << "Found " << type._to_string();
		
		if (type == +E_SerialObject::NONE)
		{
			break;
		}

		switch (type)
		{	
			default:
			{
				std::cout << "Unknown rts type" << std::endl;
				break;
			}
			case E_SerialObject::METADATA:
			{
				bool pass = getFilterObjectFromFile(type, smoothedKF.metaDataMap, startPos, inputFile);
				if (pass == false)
				{
					std::cout << "CREASS" << std::endl;
					return KFState();
				}
				
				if (write)
				{
					spitFilterToFile(smoothedKF.metaDataMap,	E_SerialObject::METADATA, outputFile);
				}
				
				break;
			}
			case E_SerialObject::MEASUREMENT:
			{
				bool pass = getFilterObjectFromFile(type, measurements, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
				}
				
				break;
			}
			case E_SerialObject::TRANSITION_MATRIX:
			{
				TransitionMatrixObject transistionMatrixObject;
				bool pass = getFilterObjectFromFile(type, transistionMatrixObject, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
				}

//				std::cout << "Setting transition matrix " << transistionMatrixObject.rows << std::endl;

// 				transitionMatrix = MatrixXd::Zero(transistionMatrixObject.rows, transistionMatrixObject.cols);

				MatrixXd transition = MatrixXd::Zero(transistionMatrixObject.rows, transistionMatrixObject.cols);
				for (auto& [keyPair, value] : transistionMatrixObject.forwardTransitionMap)
				{
					transition(keyPair.first, keyPair.second) = value;
				}
				
				transitionMatrix = (transitionMatrix * transition).eval();

				break;
			}
			case E_SerialObject::FILTER_MINUS:
			{
				bool pass = getFilterObjectFromFile(type, kalmanMinus, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
				}

				if (smoothedXready == false)
				{
					smoothedXready = true;
				}
				
				transitionMatrix = MatrixXd::Identity(kalmanMinus.x.rows(), kalmanMinus.x.rows());

				break;
			}
			case E_SerialObject::FILTER_PLUS:
			{
				lag++;

				if (write)
				{
					BOOST_LOG_TRIVIAL(info) 
					<< "RTS iterations: " << lag;
				}

				KFState kalmanPlus;
				bool pass = getFilterObjectFromFile(type, kalmanPlus, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
				}

				if (smoothedPready == false)
				{
					kalmanPlus.metaDataMap = kfState.metaDataMap;
					
					smoothedPready	= true;
					smoothedKF		= kalmanPlus;

					if (write)
					{
						spitFilterToFile(smoothedKF,	E_SerialObject::FILTER_PLUS, outputFile);
						spitFilterToFile(measurements,	E_SerialObject::MEASUREMENT, outputFile);
					}

					break;
				}

				if (smoothedXready == false)
				{
					break;
				}

				smoothedKF.time = kalmanPlus.time;

				smoothedKF. P	= (smoothedKF.	P	+ smoothedKF.	P.transpose()).eval() / 2;
				kalmanMinus.P	= (kalmanMinus.	P	+ kalmanMinus.	P.transpose()).eval() / 2;
				kalmanPlus. P	= (kalmanPlus.	P	+ kalmanPlus.	P.transpose()).eval() / 2;

				//get process noise and dynamics
				auto& F = transitionMatrix;

				kalmanMinus.P(0,0) = 1;

				MatrixXd Pinv = kalmanMinus.P.inverse();

				Pinv = (Pinv + Pinv.transpose()).eval()	/ 2;

				MatrixXd Ck = kalmanPlus.P * F.transpose() * Pinv;

				VectorXd deltaX = Ck * (smoothedKF.x - kalmanMinus.x);
				MatrixXd deltaP = Ck * (smoothedKF.P - kalmanMinus.P) * Ck.transpose();
				
				smoothedKF.dx	= deltaX;
				smoothedKF.x	= deltaX + kalmanPlus.x;
				smoothedKF.P	= deltaP + kalmanPlus.P;
				
				if (measurements.H.cols() == deltaX.rows())
				{
					measurements.VV -= measurements.H * deltaX;
				}
				else
				{
					std::cout << "screwy" << std::endl;
				}
				
				smoothedKF.kfIndexMap = kalmanPlus.kfIndexMap;

				if (write)
				{
					spitFilterToFile(smoothedKF,	E_SerialObject::FILTER_PLUS, outputFile);
					spitFilterToFile(measurements,	E_SerialObject::MEASUREMENT, outputFile);
				}
				else
				{
					bool final = false;
					if (lag == kfState.rts_lag)
					{
						final = true;
					}
					
// 					smoothedKF.metaDataMap = kfState.metaDataMap;	//todo aaron check this
					postRTSActions(final, smoothedKF, stationMap_ptr);
					
					if	(  acsConfig.output_mongo_measurements
						&& acsConfig.output_intermediate_rts)
					{
						mongoMeasResiduals(smoothedKF.time, measurements.obsKeys, measurements.V, measurements.VV, measurements.R, acsConfig.mongo_rts_suffix);
					}
				}
				
				break;
			}
		}

		if (startPos == 0)
		{
			break;
		}
	}
	
	if (write)
	{
		RTS_Output(kfState, stationMap_ptr);
	}

	if (lag == kfState.rts_lag)
	{
		//delete the beginning of the history file
		string tempFile	= kfState.rts_basename + FORWARD_SUFFIX + "_temp";
		{
			std::ofstream	tempStream(tempFile,	std::ifstream::binary | std::ofstream::out | std::ofstream::trunc);
			std::fstream	inputStream(inputFile,	std::ifstream::binary | std::ifstream::in);

			inputStream.seekg(0,	inputStream.end);
			long int lengthPos = inputStream.tellg();

			vector<char>	fileContents(lengthPos - startPos);

			inputStream.seekg(startPos,	inputStream.beg);

			inputStream.read(&fileContents[0], lengthPos - startPos);
			tempStream.write(&fileContents[0], lengthPos - startPos);
		}

		std::remove(inputFile.c_str());
		std::rename(tempFile.c_str(), inputFile.c_str());
	}
	
	if	(  kfState.rts_lag < 0
		&& acsConfig.retain_rts_files == false)
	{
		BOOST_LOG_TRIVIAL(info) 
		<< "Removing RTS file: " << inputFile;
		
		std::remove(inputFile.c_str());
		
		BOOST_LOG_TRIVIAL(info) 
		<< "Removing RTS file: " << outputFile;
		
		std::remove(outputFile.c_str());
	}

	if (lag == kfState.rts_lag)
	{
		return smoothedKF;
	}
	else
	{
		return KFState();
	}
}
