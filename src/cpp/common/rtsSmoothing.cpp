

#include <map>

using std::map;

#include "eigenIncluder.hpp"

#include "algebraTrace.hpp"
#include "rtsSmoothing.hpp"
#include "writeClock.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"
#include "mongo.hpp"
#include "ppp.hpp"


bool isPositiveSemiDefinite(MatrixXd& mat)
{
	for (int i = 0; i < mat.rows(); i++)
	for (int j = 0; j < i; j++)
	{
		double a	= mat(i, i);
		double ab	= mat(i, j);
		double b	= mat(j, j);

		if (ab * ab > a * b)
		{
// 			std::cout << "large off diagonals " << std::endl;
// 			return false;
			if (ab > 0) ab = +sqrt(0.99 * a * b);
			else		ab = -sqrt(0.99 * a * b);
			mat(i, j) = ab;
			mat(j, i) = ab;
		}
	}
	return true;
}

KFState RTS_Process(KFState& kfState, bool write)
{
	MatrixXd transistionMatrix;

	KFState kalmanMinus;
	KFState smoothedKF;

	bool smoothedXready = false;
	bool smoothedPready = false;

	string inputFile	= kfState.rts_forward_filename;
	string outputFile	= kfState.rts_filename + BACKWARD_SUFFIX;

	if (write)
	{
		std::ofstream ofs(outputFile,	std::ofstream::out | std::ofstream::trunc);
	}

	long int startPos = -1;
	int lag = 0;
	while (lag != kfState.rts_lag)
	{
		E_SerialObject type = getFilterTypeFromFile(startPos, inputFile);

		if (type == +E_SerialObject::NONE)
		{
			break;
		}

		switch (type)
		{
			case E_SerialObject::TRANSITION_MATRIX:
			{
				TransitionMatrixObject transistionMatrixObject;
				bool pass = getFilterObjectFromFile(type, transistionMatrixObject, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
				}

//				std::cout << "Setting transition matrix " << transistionMatrixObject.rows << std::endl;

				transistionMatrix = MatrixXd::Zero(transistionMatrixObject.rows, transistionMatrixObject.cols);

				for (auto& [keyPair, value] : transistionMatrixObject.forwardTransitionMap)
				{
					transistionMatrix(keyPair.first, keyPair.second) = value;
				}

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

				break;
			}
			case E_SerialObject::FILTER_PLUS:
			{
				lag++;

				if (write)
				{
					std::cout << std::endl << "Lag: " << lag << std::endl;
				}

				KFState kalmanPlus;
				bool pass = getFilterObjectFromFile(type, kalmanPlus, startPos, inputFile);
				if (pass == false)
				{
					return KFState();
				}

				if (smoothedPready == false)
				{
					smoothedPready	= true;
					smoothedKF		= kalmanPlus;

					if (write)
					{
						spitFilterToFile(smoothedKF, E_SerialObject::FILTER_PLUS, outputFile);
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
				auto& F = transistionMatrix;

				kalmanMinus.P(0,0) = 1;			//todo aaron, undo this later?

				MatrixXd Pinv = kalmanMinus.P.inverse();

				Pinv = (Pinv + Pinv.transpose()).eval()	/ 2;

				MatrixXd Ck = kalmanPlus.P * F.transpose() * Pinv;

				smoothedKF.x = ( kalmanPlus.x + Ck * (smoothedKF.x - kalmanMinus.x)						).eval();
				smoothedKF.P = ( kalmanPlus.P + Ck * (smoothedKF.P - kalmanMinus.P) * Ck.transpose()	).eval();

				smoothedKF.kfIndexMap = kalmanPlus.kfIndexMap;

				if (write)
				{
					spitFilterToFile(smoothedKF, E_SerialObject::FILTER_PLUS, outputFile);
				}
				
				break;
			}
		}

		if (startPos == 0)
		{
			break;
		}
	}

	if (lag == kfState.rts_lag)
	{
		//delete the beginning of the history file
		string tempFile	= kfState.rts_forward_filename + "_temp";
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

	if (lag == kfState.rts_lag)
	{
		return smoothedKF;
	}
	else
	{
		return KFState();
	}
}

/** Output filter states from a reversed binary trace file
*/
void RTS_Output(
	KFState&	kfState,			///< State to get filter traces from
	string		clockFilename)		///< Filename to output clocks to once smoothed
{
	std::ofstream ofs(kfState.rts_filename + SMOOTHED_SUFFIX,	std::ofstream::out | std::ofstream::app);

	long int startPos = -1;
	while (1)
	{
		E_SerialObject type = getFilterTypeFromFile(startPos, kfState.rts_filename + BACKWARD_SUFFIX);

		switch (type)
		{
			default:
			{
				std::cout << "UNEXPECTED RTS OUTPUT TYPE";
				return;
			}

			case E_SerialObject::FILTER_PLUS:
			{
				KFState archiveKF;
				bool pass = getFilterObjectFromFile(type, archiveKF, startPos, kfState.rts_filename + BACKWARD_SUFFIX);

				if	( acsConfig.output_clocks
					&&clockFilename.empty() == false)
				{
					double ep[6];
					time2epoch(archiveKF.time, ep);

					outputClockfileHeader(clockFilename, archiveKF, ep);
					outputReceiverClocks (clockFilename, archiveKF, ep);
					outputSatelliteClocks(clockFilename, archiveKF, ep);
				}

				archiveKF.outputStates(ofs);

#				ifdef ENABLE_MONGODB
				if	( acsConfig.output_mongo_states)
				{
					mongoStates(archiveKF, "RTS_");
				}
#				endif

				pppoutstat(ofs, archiveKF, true);
				break;
			}
		}

		if (startPos == 0)
		{
			return;
		}
	}
}
