

#include <memory>
#include <thread>
#include <map>

using std::this_thread::sleep_for;
using std::make_shared;
using std::shared_ptr;
using std::map;

#include <boost/log/trivial.hpp>

#include "interactiveTerminal.hpp"
#include "minimumConstraints.hpp"
#include "architectureDocs.hpp"
#include "rinexNavWrite.hpp"
#include "eigenIncluder.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "orbexWrite.hpp"
#include "mongoWrite.hpp"
#include "GNSSambres.hpp"
#include "orbitProp.hpp"
#include "acsConfig.hpp"
#include "testUtils.hpp"
#include "constants.hpp"
#include "ionoModel.hpp"
#include "sp3Write.hpp"
#include "metaData.hpp"
#include "algebra.hpp"
#include "sinex.hpp"
#include "cost.hpp"
#include "ppp.hpp"
#include "gpx.hpp"
#include "pos.hpp"

// #pragma GCC optimize ("O0")

/** Rauch-Tung-Striebel Smoothing.
 * Combine estimations using filtered data from before and after each epoch.
 * Complete filter vectors and matrices are stored in a binary file that is able to be read in reverse.
 */
Architecture RTS_Smoothing__()
{
	DOCS_REFERENCE(Binary_Archive__);
}

void postRTSActions(
	bool			final,			///< This is a final answer, not intermediate - output to files
	KFState&		kfStateIn,		///< State to get filter traces from
	ReceiverMap&	receiverMap)	///< map of receivers
{
	//need to copy to not destroy the smoothed filter in those cases this is called from there
	KFState kfState = kfStateIn;

	tryPrepareFilterPointers(kfState, receiverMap);

	if	( final
		||acsConfig.pppOpts.output_intermediate_rts)
	{
		mongoStates(kfState,
					{
						.suffix		= "/RTS",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if (final == false)
	{
		return;
	}

	if (kfState.metaDataMap["SKIP_RTS_OUTPUT"] == "TRUE")
	{
		return;
	}

	std::ofstream pppTrace(kfState.metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX], std::ofstream::out | std::ofstream::app);
	kfState.outputStates(pppTrace, "/RTS");

	if (acsConfig.pivot_receiver != "NO_PIVOT")
	{
		KFState pivotedState = propagateUncertainty(pppTrace, kfState);

		pivotedState.outputStates(pppTrace, "/RTS_PIVOT");

		mongoStates(pivotedState,
					{
						.suffix		= "/RTS_PIVOT",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	//if AR and its not the special case of forward per epoch fixed and held which already has AR in the smoothed version
	if	(	acsConfig.ambrOpts.mode			!= +E_ARmode::OFF
		&&	acsConfig.ambrOpts.once_per_epoch
		&&	acsConfig.ambrOpts.fix_and_hold	== false)
	{
		fixAndHoldAmbiguities(pppTrace, kfState);

		kfState.outputStates(pppTrace, "/RTS_AR");

		mongoStates(kfState,
					{
						.suffix		= "/RTS_AR",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});
	}

	if	(  acsConfig.process_minimum_constraints
		&& acsConfig.minconOpts.once_per_epoch)
	{
		BOOST_LOG_TRIVIAL(info) << " ------- PERFORMING MIN-CONSTRAINTS   --------" << "\n";

		for (auto& [id, rec] : receiverMap)
		{
			rec.minconApriori = rec.aprioriPos;
		}

		MinconStatistics minconStatistics;

		mincon(pppTrace, kfState, &minconStatistics);

		kfState.outputStates(pppTrace, "/RTS_CONSTRAINED");

		mongoStates(kfState,
					{
						.suffix		= "/RTS_CONSTRAINED",
						.instances	= acsConfig.mongoOpts.output_states,
						.queue		= acsConfig.mongoOpts.queue_outputs
					});

		outputMinconStatistics(pppTrace, minconStatistics);
	}

	nav.erp.filterValues = getErpFromFilter(kfState);		//todo aaron, this doesnt react well with remote filter values

	if (acsConfig.output_bias_sinex)
	{
		//todo aaron, this requires another ionospher kfState
// 		writeBiasSinex(nullStream, kfState.time, kfState, kfState.metaDataMap[BSX_FILENAME_STR + SMOOTHED_SUFFIX], receiverMap);
	}

	auto time = kfState.time;

	static GTime clkOutputTime = time.floorTime(acsConfig.clocks_output_interval);
	static GTime obxOutputTime = time.floorTime(acsConfig.orbex_output_interval);
	static GTime sp3OutputTime = time.floorTime(acsConfig.sp3_output_interval);

	{
		if (acsConfig.output_orbit_ics)										{	outputOrbitConfig	(																									kfState, acsConfig.pppOpts.rts_smoothed_suffix);																	}
		if (acsConfig.output_clocks)		while (clkOutputTime <= time)	{	outputClocks		(				kfState.metaDataMap[CLK_FILENAME_STR			+ SMOOTHED_SUFFIX], clkOutputTime, acsConfig.clocks_receiver_sources,	acsConfig.clocks_satellite_sources, kfState, &receiverMap);							clkOutputTime += std::max(acsConfig.epoch_interval, acsConfig.clocks_output_interval);	}
		if (acsConfig.output_orbex)			while (obxOutputTime <= time)	{	outputOrbex			(				kfState.metaDataMap[ORBEX_FILENAME_STR			+ SMOOTHED_SUFFIX], obxOutputTime, acsConfig.orbex_orbit_sources,		acsConfig.orbex_clock_sources, acsConfig.orbex_attitude_sources,	&kfState);		obxOutputTime += std::max(acsConfig.epoch_interval, acsConfig.orbex_output_interval);	}
		if (acsConfig.output_sp3)			while (sp3OutputTime <= time)	{	outputSp3			(				kfState.metaDataMap[SP3_FILENAME_STR			+ SMOOTHED_SUFFIX], sp3OutputTime, acsConfig.sp3_orbit_sources,			acsConfig.sp3_clock_sources,										&kfState);		sp3OutputTime += std::max(acsConfig.epoch_interval, acsConfig.sp3_output_interval);		}
		if (acsConfig.output_trop_sinex)									{	outputTropSinex		(				kfState.metaDataMap[TROP_FILENAME_STR			+ SMOOTHED_SUFFIX], time,			kfState, "MIX", true);																								}
		if (acsConfig.output_ionex)											{	ionexFileWrite		(nullStream,	kfState.metaDataMap[IONEX_FILENAME_STR			+ SMOOTHED_SUFFIX], time,			kfState);																											}
		if (acsConfig.output_erp)											{	writeErpFromNetwork	(				kfState.metaDataMap[ERP_FILENAME_STR			+ SMOOTHED_SUFFIX],					kfState);																											}
		if (acsConfig.output_ionstec)										{	writeIonStec	 	(				kfState.metaDataMap[IONSTEC_FILENAME_STR		+ SMOOTHED_SUFFIX],					kfState);																											}
	}

	for (auto& [id, rec] : receiverMap)
	{
		if (acsConfig.output_cost)											{	outputCost			(				kfState.metaDataMap[COST_FILENAME_STR	+ id	+ SMOOTHED_SUFFIX],					kfState,	rec);		}
		if (acsConfig.output_gpx)											{	writeGPX			(				kfState.metaDataMap[GPX_FILENAME_STR	+ id	+ SMOOTHED_SUFFIX],					kfState,	rec);		}
		if (acsConfig.output_pos)											{	writePOS			(				kfState.metaDataMap[POS_FILENAME_STR	+ id	+ SMOOTHED_SUFFIX],					kfState,	rec);		}
	}
}

/** Output filter states in chronological order from a reversed binary trace file
*/
void RTS_Output(
	KFState&		kfState,			///< State to get filter traces from
	ReceiverMap&	receiverMap)		///< map of receivers
{
	InteractiveTerminal::setMode(E_InteractiveMode::Outputs);

	string reversedStatesFilename = kfState.rts_basename + BACKWARD_SUFFIX;

	long int startPos = -1;

	BOOST_LOG_TRIVIAL(info)
	<< "Outputting RTS products..." << "\n";

	map<string, string> metaDataMap = kfState.metaDataMap;

	while (1)
	{
		E_SerialObject type = getFilterTypeFromFile(startPos, reversedStatesFilename);

		BOOST_LOG_TRIVIAL(debug)
		<< "Outputting " << type._to_string() << " from file position " << startPos << "\n";

		if (type == +E_SerialObject::NONE)
		{
			break;
		}

		switch (type)
		{
			default:
			{
				BOOST_LOG_TRIVIAL(error) << "UNEXPECTED RTS OUTPUT TYPE";
				return;
			}

			case E_SerialObject::METADATA:
			{
				bool pass = getFilterObjectFromFile(type, metaDataMap, startPos, reversedStatesFilename);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(error) << "BAD RTS OUTPUT read";
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
					BOOST_LOG_TRIVIAL(error) << "BAD RTS OUTPUT read";
					return;
				}

				string filename = metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX];

				std::ofstream ofs(filename, std::ofstream::out | std::ofstream::app);
				if	( ofs
					&&acsConfig.output_residuals)
				{
					outputResiduals(ofs, archiveMeas, -1, "/RTS", 0, archiveMeas.obsKeys.size());
				}

				if (acsConfig.mongoOpts.output_measurements)
				{
					mongoMeasResiduals(archiveMeas.time, archiveMeas, acsConfig.mongoOpts.queue_outputs, "/RTS");
				}

				break;
			}

			case E_SerialObject::FILTER_SMOOTHED:
			{
				KFState archiveKF;
				bool pass = getFilterObjectFromFile(type, archiveKF, startPos, reversedStatesFilename);

				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(error) << "BAD RTS OUTPUT READ";
					return;
				}

				archiveKF.metaDataMap = metaDataMap;

				postRTSActions(true, archiveKF, receiverMap);

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
			<< "Oopsie " << "\n";

			return;
		}
	}
}

/** Iterate over stored filter states in reverse and perform filtering.
 * Saves filtered states to a secondary binary file, which is in reverse-chronological order due to the save sequence.
 * Most serial objects that are processed are merely stored or accumulated as prerequisites for the FILTER_PLUS object,
 * which contains the state of the filter immediately after the update step.
 * At that stage, the previously smoothed (next chronologically) filter state is combined with the next filter minus state
 * (immediately before the next chronological update step), any state transitions, and the filter plus state, using the standard rts algorithm.
 * The filtered state and a measurements object which has updated residuals are then stored in a binary file.
 * If intermediate outputs are enabled (rare) it performs some outputs using each filter state, but typically outputs all states chronologically
 * after the reverse running rts procedure has reached the first epoch and all data is available for output in the correct sequence.
 */
void rtsSmoothing(
	KFState&		kfState,
	ReceiverMap&	receiverMap,
	bool			write)
{
	DOCS_REFERENCE(RTS_Smoothing__);

	if (kfState.rts_lag == 0)
	{
		return;
	}

	BOOST_LOG_TRIVIAL(info)
	<< "\n"
	<< "---------------PROCESSING WITH RTS--------------------- " << "\n";

	for (auto& [id, rec] : receiverMap)
		rec.obsList.clear();

	for (auto& [dummy, satNav] : nav.satNavMap)
		satNav.attStatus = {};

	MatrixXd transitionMatrix;

	KFState	kalmanMinus;
	KFState	smoothedKF;
	KFMeas	measurements;

	bool smoothedXready = false;
	bool smoothedPready = false;
	bool skipNextRts	= false;

	string inputFile	= kfState.rts_basename + FORWARD_SUFFIX;
	string outputFile	= kfState.rts_basename + BACKWARD_SUFFIX;

	if (write)
	{
		std::ofstream ofs(outputFile,	std::ofstream::out | std::ofstream::trunc);
	}

	long int startPos = -1;
	double lag = 0;

	GTime epochStartTime = timeGet();

	while (lag != kfState.rts_lag)
	{
		E_SerialObject type = getFilterTypeFromFile(startPos, inputFile);

		BOOST_LOG_TRIVIAL(debug) << "Found " << type._to_string() << "\n";

		if (type == +E_SerialObject::NONE)
		{
			break;
		}

		switch (type)
		{
			default:
			{
				BOOST_LOG_TRIVIAL(error) << "Error: Unknown rts type" << "\n";
				break;
			}
			case E_SerialObject::METADATA:
			{
				skipNextRts = (smoothedKF.metaDataMap["SKIP_PREV_RTS"] == "TRUE");

				bool pass = getFilterObjectFromFile(type, smoothedKF.metaDataMap, startPos, inputFile);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(debug) << "CREASS" << "\n";
					return;
				}

				if (write)
				{
					spitFilterToFile(smoothedKF.metaDataMap, E_SerialObject::METADATA, outputFile, acsConfig.pppOpts.queue_rts_outputs);
				}

				break;
			}
			case E_SerialObject::MEASUREMENT:
			{
				bool pass = getFilterObjectFromFile(type, measurements, startPos, inputFile);
				if (pass == false)
				{
					return;
				}

				break;
			}
			case E_SerialObject::TRANSITION_MATRIX:
			{
				TransitionMatrixObject transistionMatrixObject;
				bool pass = getFilterObjectFromFile(type, transistionMatrixObject, startPos, inputFile);
				if (pass == false)
				{
					return;
				}

				MatrixXd transition = transistionMatrixObject.asMatrix();

				if (transitionMatrix.rows() == 0)		transitionMatrix = transition;
				else 									transitionMatrix = (transitionMatrix * transition).eval();

				break;
			}
			case E_SerialObject::FILTER_MINUS:
			{
				bool pass = getFilterObjectFromFile(type, kalmanMinus, startPos, inputFile);
				if (pass == false)
				{
					return;
				}

				if (smoothedXready == false)
				{
					smoothedXready = true;
				}

				transitionMatrix = MatrixXd::Identity(0, 0);

				break;
			}
			case E_SerialObject::FILTER_PLUS:
			{
				KFState kalmanPlus;
				bool pass = getFilterObjectFromFile(type, kalmanPlus, startPos, inputFile);
				if (pass == false)
				{
					return;
				}

				lag = (kfState.time - kalmanPlus.time).to_double();

				if (write)
				{
					BOOST_LOG_TRIVIAL(info)
					<< "RTS lag: " << lag;
				}

				if (smoothedPready == false)
				{
					kalmanPlus.metaDataMap = kfState.metaDataMap;

					smoothedPready	= true;
					smoothedKF		= kalmanPlus;

					if (write)
					{
						spitFilterToFile(smoothedKF,	E_SerialObject::FILTER_SMOOTHED,	outputFile, acsConfig.pppOpts.queue_rts_outputs);
						spitFilterToFile(measurements,	E_SerialObject::MEASUREMENT,		outputFile, acsConfig.pppOpts.queue_rts_outputs);
					}

					break;
				}

				if (smoothedXready == false)
				{
					break;
				}

				InteractiveTerminal::setMode(E_InteractiveMode::Filtering);

				smoothedKF.time = kalmanPlus.time;

				smoothedKF.P = (smoothedKF.P + smoothedKF.P.transpose()).eval() / 2;

				//get process noise and dynamics
				auto& F = transitionMatrix;

				if	( F.rows() == 0
					&&F.cols() == 0)
				{
					//assume identity state transition if none was performed/required
					F = MatrixXd::Identity(kalmanPlus.P.rows(), kalmanPlus.P.rows());
				}

				MatrixXd FP = F * kalmanPlus.P;

				E_Inverter inverter = acsConfig.pppOpts.rts_inverter;

				auto failInversion = [&]()
				{
					auto oldInverter = inverter;
					inverter = E_Inverter::_from_integral(((int)inverter)+1);

					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: Inverter type " << oldInverter._to_string() << " failed, trying " << inverter._to_string();
				};
				VectorXd deltaX = VectorXd::Zero(kalmanPlus.x.rows());
				MatrixXd deltaP = MatrixXd::Zero(kalmanPlus.P.rows(), kalmanPlus.P.cols());

				map<string, bool> filterChunks;
				for (auto& [id, fcP] : kalmanPlus. filterChunkMap)		filterChunks[id] = true;
				for (auto& [id, fcM] : kalmanMinus.filterChunkMap)		filterChunks[id] = true;

				for (auto& [id, dummy] : filterChunks)
				{
					auto& fcP = kalmanPlus. filterChunkMap[id];
					auto& fcM = kalmanMinus.filterChunkMap[id];

					if	( fcP.numX == 0
						||fcM.numX == 0)
					{
						BOOST_LOG_TRIVIAL(debug) << "Ignoring  chunk " << id;
						continue;
					}

					BOOST_LOG_TRIVIAL(debug) << "Filtering chunk " << id;
					auto Q		= kalmanMinus.P	.block(fcM.begX, fcM.begX, fcM.numX, fcM.numX).triangularView<Eigen::Upper>().transpose();
					auto FP_	= FP			.block(fcM.begX, fcP.begX, fcM.numX, fcP.numX);

					MatrixXd Ck;

					int pass = false;

					auto solve = [&]<typename SOLVER>(SOLVER solver) -> bool
					{
						solver.compute(Q);
						if (solver.info())
						{
							pass = false;
							return pass;
						}

						Ck = solver.solve(FP_).transpose();

						if (solver.info())
						{
							pass = false;
							return pass;
						}

						pass = true;
						return pass;
					};

					while	(inverter != +E_Inverter::FIRST_UNSUPPORTED
							&&(pass == false))
					switch (inverter)
					{
						default:
						{
							BOOST_LOG_TRIVIAL(warning)
							<< "Warning: Inverter type " << acsConfig.pppOpts.rts_inverter._to_string() << " not supported, reverting to LDLT";

							acsConfig.pppOpts.rts_inverter	= E_Inverter::LDLT;
							inverter						= E_Inverter::LDLT;

							continue;
						}
						case E_Inverter::FULLPIVLU:
						case E_Inverter::INV:
						{
							Eigen::FullPivLU<MatrixXd> solver(kalmanMinus.P.block(fcM.begX, fcM.begX, fcM.numX, fcM.numX));

							if (solver.isInvertible() == false)
							{
								failInversion();

								break;
							}

							MatrixXd Pinv = solver.inverse();

							Pinv = (Pinv + Pinv.transpose()).eval() / 2;

							Ck = (FP_.transpose() * Pinv);

							pass = true;

							break;
						}
						case E_Inverter::LLT:		{	solve(Eigen::LLT					<MatrixXd>()); if (pass == false)	failInversion();	break;	}
						case E_Inverter::LDLT:		{	solve(Eigen::LDLT					<MatrixXd>()); if (pass == false)	failInversion();	break;	}
						case E_Inverter::COLPIVHQR:	{	solve(Eigen::ColPivHouseholderQR	<MatrixXd>()); if (pass == false)	failInversion();	break;	}
						case E_Inverter::BDCSVD:	{	solve(Eigen::BDCSVD					<MatrixXd>()); if (pass == false)	failInversion();	break;	}
						case E_Inverter::JACOBISVD:	{	solve(Eigen::JacobiSVD				<MatrixXd>()); if (pass == false)	failInversion();	break;	}
	// 					case E_Inverter::FULLPIVHQR:{	solve(Eigen::FullPivHouseholderQR	<MatrixXd>()); if (pass == false)	failInversion();	break;	}
					}

					if (pass == false)
					{
						BOOST_LOG_TRIVIAL(warning)
						<< "Warning: RTS failed to find solution to invert system of equations, smoothed values may be bad";

						BOOST_LOG_TRIVIAL(debug) << "P-det: " << kalmanMinus.P.determinant();

						kalmanMinus.outputConditionNumber(std::cout);

						BOOST_LOG_TRIVIAL(debug)  << "P:\n" << kalmanMinus.P.format(heavyFmt);
						kalmanMinus.outputCorrelations(std::cout);
						std::cout << "\n";

						//break out of the loop
						lag = kfState.rts_lag;
						break;
					}

					auto deltaX_	= deltaX		 .segment	(fcP.begX,				fcP.numX);
					auto smoothedX	= smoothedKF.	x.segment	(fcM.begX,				fcM.numX);
					auto xMinus		= kalmanMinus.	x.segment	(fcM.begX,				fcM.numX);
					auto deltaP_	= deltaP		 .block		(fcP.begX, fcP.begX,	fcP.numX, fcP.numX);
					auto smoothedP	= smoothedKF.	P.block		(fcM.begX, fcM.begX,	fcM.numX, fcM.numX);
					auto minuxP		= kalmanMinus.	P.block		(fcM.begX, fcM.begX,	fcM.numX, fcM.numX);

					deltaX_ = Ck * (smoothedX - xMinus);
					deltaP_ = Ck * (smoothedP - minuxP) * Ck.transpose();
				}

				smoothedKF.dx	= deltaX;
				smoothedKF.x	= deltaX + kalmanPlus.x;
				smoothedKF.P	= deltaP + kalmanPlus.P;

				if (measurements.H.rows())
				if (measurements.H.cols() == deltaX.rows())
				{
					measurements.VV -= measurements.H * deltaX;
				}
				else
				{
					BOOST_LOG_TRIVIAL(error)  << "RTScrewy" << "\n";
				}

				smoothedKF.kfIndexMap = kalmanPlus.kfIndexMap;

				if (write)
				{
					InteractiveTerminal::setMode(E_InteractiveMode::Outputs);

					spitFilterToFile(smoothedKF,					E_SerialObject::FILTER_SMOOTHED,	outputFile, acsConfig.pppOpts.queue_rts_outputs);
					spitFilterToFile(measurements,					E_SerialObject::MEASUREMENT,		outputFile, acsConfig.pppOpts.queue_rts_outputs);

					if (skipNextRts)
					{
						skipNextRts = false;
						smoothedKF.metaDataMap["SKIP_RTS_OUTPUT"] = "TRUE";
						spitFilterToFile(smoothedKF.metaDataMap,	E_SerialObject::METADATA,			outputFile, acsConfig.pppOpts.queue_rts_outputs);
					}
				}
				else
				{
					bool final = false;
					if (lag >= kfState.rts_lag)
					{
						final = true;
					}

					postRTSActions(final, smoothedKF, receiverMap);

					if (acsConfig.pppOpts.output_intermediate_rts)
					{
						mongoMeasResiduals(smoothedKF.time, measurements, acsConfig.mongoOpts.queue_outputs, "/RTS");
					}
				}

				GTime epochStopTime = timeGet();
				if (write)
				{
					int fractionalMilliseconds = (kalmanPlus.time.bigTime - (long int) kalmanPlus.time.bigTime) * 1000;
					auto boostTime = boost::posix_time::from_time_t((time_t)((PTime)kalmanPlus.time).bigTime) + boost::posix_time::millisec(fractionalMilliseconds);

					BOOST_LOG_TRIVIAL(info)
					<< "Processed epoch"
					<< " - " << boostTime
					<< " (took " << (epochStopTime-epochStartTime) << ")";

					InteractiveTerminal::clearModes(
													(string)" Processing epoch "	+ kalmanPlus.time.to_string(),
													(string)" Last Epoch took "		+ std::to_string((epochStopTime-epochStartTime).to_double()) + "s");
					InteractiveTerminal::setMode(E_InteractiveMode::Syncing);
				}
				epochStartTime = timeGet();

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
		while (spitQueueRunning)
		{
			sleep_for(std::chrono::milliseconds(acsConfig.sleep_milliseconds));
		}

		RTS_Output(kfState, receiverMap);
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

			vector<char> fileContents(lengthPos - startPos);

			inputStream.seekg(startPos,	inputStream.beg);

			inputStream.read(&fileContents[0], lengthPos - startPos);
			tempStream.write(&fileContents[0], lengthPos - startPos);
		}

		std::remove(inputFile.c_str());
		std::rename(tempFile.c_str(), inputFile.c_str());
	}

	if	(  kfState.rts_lag <= 0
		&& acsConfig.retain_rts_files == false)
	{
		BOOST_LOG_TRIVIAL(info)
		<< "Removing RTS file: " << inputFile;

		std::remove(inputFile.c_str());

		BOOST_LOG_TRIVIAL(info)
		<< "Removing RTS file: " << outputFile;

		std::remove(outputFile.c_str());
	}
}
