
// #pragma GCC optimize ("O0")

#include <memory>
#include <thread>
#include <map>

using std::this_thread::sleep_for;
using std::make_shared;
using std::shared_ptr;
using std::map;

#include <boost/log/trivial.hpp>

#include "rinexNavWrite.hpp"
#include "eigenIncluder.hpp"
#include "rinexObsWrite.hpp"
#include "rinexClkWrite.hpp"
#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "binaryStore.hpp"
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


void postRTSActions(
	bool		final,			///< This is a final answer, not intermediate - output to files
	KFState&	kfState,		///< State to get filter traces from
	ReceiverMap&	receiverMap)		///< map of stations
{
	if	( final
		||acsConfig.pppOpts.output_intermediate_rts)
	{
		mongoStates(kfState, {.suffix = "_rts", .instances = acsConfig.mongoOpts.output_states});
	}

	if	( final
		||acsConfig.pppOpts.output_intermediate_rts)
	{
		storeStates(kfState, "_rts");
	}

	if (final == false)
	{
		return;
	}

	if (kfState.metaDataMap["SKIP_RTS_OUTPUT"] == "TRUE")
	{
		return;
	}

	{
		std::ofstream ofs(kfState.metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX], std::ofstream::out | std::ofstream::app);
		kfState.outputStates(ofs, "/RTS");
	}

	if (acsConfig.output_bias_sinex)
	{
		//todo aaron, this requires another ionospher kfState
// 		writeBiasSinex(nullStream, kfState.time, kfState, kfState.metaDataMap[BSX_FILENAME_STR + SMOOTHED_SUFFIX], receiverMap);
	}

	if (acsConfig.output_orbit_ics)
	{
		outputOrbitConfig(kfState, "_smoothed");
	}

	if	(   acsConfig.output_clocks
		&&( acsConfig.clocks_receiver_sources.front()	== +E_Source::KALMAN
		  ||acsConfig.clocks_satellite_sources.front()	== +E_Source::KALMAN))
	{
		auto kfState2 = kfState;	//todo aaron, delete this after fixing something else, tryPrepareFilterPointers damages the state
		tryPrepareFilterPointers(kfState2, receiverMap);

												outputClocks		(kfState.metaDataMap[CLK_FILENAME_STR			+ SMOOTHED_SUFFIX], acsConfig.clocks_receiver_sources, acsConfig.clocks_satellite_sources, kfState2.time, kfState2, &receiverMap);
	}

	{
		if (acsConfig.output_orbex)			{	outputOrbex			(				kfState.metaDataMap[ORBEX_FILENAME_STR			+ SMOOTHED_SUFFIX], kfState.time, acsConfig.orbex_orbit_sources,	acsConfig.orbex_clock_sources, acsConfig.orbex_attitude_sources,	&kfState);	}
		if (acsConfig.output_sp3)			{	outputSp3			(				kfState.metaDataMap[SP3_FILENAME_STR			+ SMOOTHED_SUFFIX], kfState.time, acsConfig.sp3_orbit_sources,		acsConfig.sp3_clock_sources,										&kfState);	}
		if (acsConfig.output_trop_sinex)	{	outputTropSinex		(				kfState.metaDataMap[TROP_FILENAME_STR			+ SMOOTHED_SUFFIX], kfState.time, kfState, "MIX", true);																							}
		if (acsConfig.output_ionex)			{	ionexFileWrite		(nullStream,	kfState.metaDataMap[IONEX_FILENAME_STR			+ SMOOTHED_SUFFIX], kfState.time, kfState);																											}
		if (acsConfig.output_erp)			{	writeErpFromNetwork	(				kfState.metaDataMap[ERP_FILENAME_STR			+ SMOOTHED_SUFFIX], kfState);																														}
		if (acsConfig.output_ionstec)		{	writeIonStec	 	(				kfState.metaDataMap[IONSTEC_FILENAME_STR		+ SMOOTHED_SUFFIX], kfState);																														}
	}

	for (auto& [id, rec] : receiverMap)
	{
		if (acsConfig.output_gpx)			{	writeGPX			(				kfState.metaDataMap[GPX_FILENAME_STR	+ id	+ SMOOTHED_SUFFIX], kfState,	id);		}
		if (acsConfig.output_cost)			{	outputCost			(				kfState.metaDataMap[COST_FILENAME_STR	+ id	+ SMOOTHED_SUFFIX], kfState,	rec);		}
	}
}

/** Output filter states from a reversed binary trace file
*/
void RTS_Output(
	KFState&	kfState,			///< State to get filter traces from
	ReceiverMap&	receiverMap)		///< map of stations
{
	string reversedStatesFilename = kfState.rts_basename + BACKWARD_SUFFIX;

	long int startPos = -1;

	BOOST_LOG_TRIVIAL(info)
	<< "Outputting RTS products..." << std::endl;

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
				if (!ofs)
				{
					BOOST_LOG_TRIVIAL(error) << "BAD RTS Write to " << filename;
					break;
				}

				if (acsConfig.output_residuals)
				{
					outputResiduals(ofs, archiveMeas, -1, "/RTS", 0, archiveMeas.obsKeys.size());

					mongoMeasResiduals(archiveMeas.time, archiveMeas, acsConfig.mongoOpts.queue_outputs, "_rts");
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

				if	(	acsConfig.ambrOpts.mode != +E_ARmode::OFF
					&&	acsConfig.ambrOpts.once_per_epoch
					&&	acsConfig.ambrOpts.fix_and_hold == false)		//this is to separate from already forward fixed and held? todo aaron
				{
					std::ofstream rtsTrace(archiveKF.metaDataMap[TRACE_FILENAME_STR + SMOOTHED_SUFFIX], std::ofstream::out | std::ofstream::app);

					fixAndHoldAmbiguities(rtsTrace, archiveKF);	//this is already a copy, no need to copy again for fix_and_hold

					postRTSActions(true, archiveKF, receiverMap);
				}

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
			<< "Oopsie " << std::endl;

			return;
		}
	}
}

KFState rtsSmoothing(
	KFState&	kfState,
	ReceiverMap&	receiverMap,
	bool		write)
{
	if (kfState.rts_lag == 0)
	{
		return KFState();
	}

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

		BOOST_LOG_TRIVIAL(debug) << "Found " << type._to_string() << std::endl;

		if (type == +E_SerialObject::NONE)
		{
			break;
		}

		switch (type)
		{
			default:
			{
				BOOST_LOG_TRIVIAL(error) << "Error: Unknown rts type" << std::endl;
				break;
			}
			case E_SerialObject::METADATA:
			{
				skipNextRts = (smoothedKF.metaDataMap["SKIP_PREV_RTS"] == "TRUE");

				bool pass = getFilterObjectFromFile(type, smoothedKF.metaDataMap, startPos, inputFile);
				if (pass == false)
				{
					BOOST_LOG_TRIVIAL(debug) << "CREASS" << std::endl;
					return KFState();
				}

				if (write)
				{
					spitFilterToFile(smoothedKF.metaDataMap,	E_SerialObject::METADATA, outputFile, acsConfig.pppOpts.queue_rts_outputs);
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

				transitionMatrix = (transitionMatrix * transistionMatrixObject.asMatrix()).eval();

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
				KFState kalmanPlus;
				bool pass = getFilterObjectFromFile(type, kalmanPlus, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
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

				smoothedKF.time = kalmanPlus.time;

				smoothedKF. P	= (smoothedKF.	P	+ smoothedKF.	P.transpose()).eval() / 2;
				kalmanMinus.P	= (kalmanMinus.	P	+ kalmanMinus.	P.transpose()).eval() / 2;
				kalmanPlus. P	= (kalmanPlus.	P	+ kalmanPlus.	P.transpose()).eval() / 2;

				//get process noise and dynamics
				auto& F = transitionMatrix;

				kalmanMinus.P(0,0) = 1;

				MatrixXd FP = F * kalmanPlus.P;

				auto Q = kalmanMinus.P.triangularView<Eigen::Upper>().transpose();

				int fail1 = true;
				int fail2 = true;
				MatrixXd Ckt;

				E_Inverter inverter = acsConfig.pppOpts.rts_inverter;

				auto failInversion = [&]()
				{
					auto oldInverter = inverter;
					inverter = E_Inverter::_from_integral(((int)inverter)+1);

					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: Inverter type " << oldInverter._to_string() << " failed, trying " << inverter._to_string();
				};

				while	(inverter != +E_Inverter::FIRST_UNSUPPORTED
						&&(  fail1
						  || fail2))
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
					case E_Inverter::INV:
					{
						Eigen::FullPivLU<MatrixXd> solver(Q);

						if (solver.isInvertible() == false)
						{
							fail1 = true;

							failInversion();

							break;
						}

						MatrixXd Pinv = solver.inverse();

						Pinv = (Pinv + Pinv.transpose()).eval()	/ 2;

						Ckt = (kalmanPlus.P * F.transpose() * Pinv).transpose();

						fail1 = false;
						fail2 = false;

						break;
					}
					case E_Inverter::LLT:		{	Eigen::LLT					<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
					case E_Inverter::LDLT:		{	Eigen::LDLT					<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
					case E_Inverter::COLPIVHQR:	{	Eigen::ColPivHouseholderQR	<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
					case E_Inverter::BDCSVD:	{	Eigen::BDCSVD				<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
					case E_Inverter::JACOBISVD:	{	Eigen::JacobiSVD			<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
// 					case E_Inverter::FULLPIVHQR:{	Eigen::FullPivHouseholderQR	<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
// 					case E_Inverter::FULLPIVLU:	{	Eigen::FullPivLU			<MatrixXd> solver;	solver.compute(Q);	fail1 = solver.info();	Ckt = solver.solve(FP);	fail2 = solver.info();	if (fail1 || fail2)	failInversion();	break;	}
				}

				if	(  fail1
					|| fail2)
				{
					BOOST_LOG_TRIVIAL(warning)
					<< "Warning: RTS failed to find solution to invert system of equations, smoothed values may be bad " << fail1 << " " << fail2;

					BOOST_LOG_TRIVIAL(debug) << "P-det: " << kalmanMinus.P.determinant();

					kalmanMinus.outputConditionNumber(std::cout);

					BOOST_LOG_TRIVIAL(debug)  << "P:\n" << kalmanMinus.P.format(heavyFmt);
					kalmanMinus.outputCorrelations(std::cout);
					std::cout << std::endl;

					//break out of the loop
					lag = kfState.rts_lag;
					break;
				}

				MatrixXd Ck = Ckt.transpose();

				VectorXd deltaX = Ck * (smoothedKF.x - kalmanMinus.x);
				MatrixXd deltaP = Ck * (smoothedKF.P - kalmanMinus.P) * Ck.transpose();

				smoothedKF.dx	= deltaX;
				smoothedKF.x	= deltaX + kalmanPlus.x;
				smoothedKF.P	= deltaP + kalmanPlus.P;

				if (measurements.H.cols() == deltaX.rows())
				{
					measurements.VV -= measurements.H * deltaX;		//todo aaron, is this the correct deltaX to be adding here? wrong plus/minus timepoint?
				}
				else
				{
					BOOST_LOG_TRIVIAL(error)  << "RTScrewy" << std::endl;
				}

				smoothedKF.kfIndexMap = kalmanPlus.kfIndexMap;

				if (write)
				{
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

// 					smoothedKF.metaDataMap = kfState.metaDataMap;	//todo aaron check this
					postRTSActions(final, smoothedKF, receiverMap);

					if (acsConfig.pppOpts.output_intermediate_rts)
					{
						mongoMeasResiduals(smoothedKF.time, measurements, acsConfig.mongoOpts.queue_outputs, "_rts");
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

			vector<char>	fileContents(lengthPos - startPos);

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

	if (lag == kfState.rts_lag)
	{
		return smoothedKF;
	}
	else
	{
		return KFState();
	}
}
