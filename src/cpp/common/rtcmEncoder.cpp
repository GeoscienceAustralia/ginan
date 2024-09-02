
// #pragma GCC optimize ("O0")

#include <boost/log/trivial.hpp>

#include "rtcmEncoder.hpp"
#include "navigation.hpp"
#include "ephemeris.hpp"
#include "acsConfig.hpp"
#include "constants.hpp"

/** Convert SSR URA to URA_CLASS and URA_VALUE combination, with 3 Msb URA_CLASS and 3Lsb URA_VALUE
*/
int uraToClassValue(double ura)
{
	int uraClassValue = 0;

	for (uraClassValue = 0; uraClassValue < 64; uraClassValue++)
		if (uraSsr[uraClassValue] >= ura)
			break;

	return uraClassValue;
}

void calculateSsrComb(
	GTime 			referenceTime,
	int 			udi,
	SSRMeta& 		ssrMeta,
	int 			masterIod,
	SsrOutMap&		ssrOutMap)
{
	if (ssrOutMap.empty())
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: No suitable Ephemeris data available.";
		return;
	}

	map<E_Sys, std::array<double, 2>> commonClockOffsetsMap;	// 0: bias; 1: bias rate
	map<E_Sys, std::array<GTime,  2>> commonClockEpochsMap;	// 0: 1st straddle pt epoch; 1: 2nd straddle pt epoch

	for (auto& [Sat, ssrOut] : ssrOutMap)
	{
		if (ssrOut.clkInput.vals[0].iode != ssrOut.ephInput.vals[0].iode)
		{
			tracepdeex(3, std::cout, "IODE mismatch between clock and ephemeris for %s\n", Sat.id().c_str());
			continue;
		}

		auto& ssrEph		= ssrOut.ssrEph;
		auto& ssrClk		= ssrOut.ssrClk;
		auto& ssrUra		= ssrOut.ssrUra;
		auto& ssrEphInput	= ssrOut.ephInput;
		auto& ssrClkInput	= ssrOut.clkInput;

		ssrEph.t0 			= referenceTime;
		ssrClk.t0 			= referenceTime;
		ssrUra.t0 			= referenceTime;
		ssrEph.udi			= udi;
		ssrClk.udi			= udi;
		ssrUra.udi			= udi;
		ssrEph.iod			= masterIod;
		ssrClk.iod			= masterIod;
		ssrUra.iod			= masterIod;

		ssrEph.ssrMeta		= ssrMeta;
		ssrClk.ssrMeta		= ssrMeta;
		ssrUra.ssrMeta		= ssrMeta;

		ssrEph.iode			= ssrEphInput.vals[0].iode;
		// ssrEph.iodcrc		= ??

		double		clkCorrections[2];
		Vector3d	posCorrections[2];
		double		uras[2];

		for (int i = 0; i < 2; i++)
		{
			posCorrections[i]	= ssrEphInput.vals[i].brdcPos
								- ssrEphInput.vals[i].precPos;
			clkCorrections[i]	= ssrClkInput.vals[i].brdcClk
								- ssrClkInput.vals[i].precClk;
			uras[i]				= ephVarToUra(ssrEphInput.vals[i].ephVar);
		}

		if (acsConfig.ssrOpts.extrapolate_corrections)	// todo Eugene: check if ura can be interpolated
		{
			Vector3d	diffRAC[2];
			double		diffClock[2];
			double		uraSsr[2];

			for (int dt : {0, 1})
			{
				double		ephRatio	= (ssrEph.t0 + dt - ssrEphInput.vals[0].time).to_double() / (ssrEphInput.vals[1].time - ssrEphInput.vals[0].time).to_double();
				double		clkRatio	= (ssrClk.t0 + dt - ssrClkInput.vals[0].time).to_double() / (ssrClkInput.vals[1].time - ssrClkInput.vals[0].time).to_double();

				Vector3d	posCorrection	= posCorrections[0] 			+ ephRatio	* (posCorrections[1]			- posCorrections[0]);
				double		clkCorrection	= clkCorrections[0]				+ clkRatio	* (clkCorrections[1]			- clkCorrections[0]);
				double		ura				= uras[0] 						+ ephRatio	* (uras[1]						- uras[0]);

				Vector3d	satPosition		= ssrEphInput.vals[0].brdcPos	+ ephRatio	* (ssrEphInput.vals[1].brdcPos	- ssrEphInput.vals[0].brdcPos);
				Vector3d	satVelocity		= ssrEphInput.vals[0].brdcVel	+ ephRatio	* (ssrEphInput.vals[1].brdcVel	- ssrEphInput.vals[0].brdcVel);

				Vector3d	diffRac			= ecef2rac(satPosition, satVelocity) * posCorrection;

				diffRAC		[dt]	= diffRac;
				diffClock	[dt]	= -clkCorrection;
				uraSsr		[dt]	= ura;
			}

			ssrEph. deph	= diffRAC[0];
			ssrEph.ddeph	= diffRAC[1] - diffRAC[0];

			ssrClk.dclk[0]	= diffClock[0];
			ssrClk.dclk[1]	= 0; //diffClock[1] - diffClock[0];
			ssrClk.dclk[2]	= 0;	// set to zero (not used)

			ssrUra.ura		= uraSsr[0];

			tracepdeex (6, std::cout, "\n   RTCM_intp_Clk  %s %s %8.3f", referenceTime.to_string().c_str(), Sat.id().c_str(), ssrClk.dclk[0]);

		}
		else
		{
			ssrEph.deph		= ecef2rac(ssrEphInput.vals[1].brdcPos, ssrEphInput.vals[1].brdcVel) * posCorrections[1];
			ssrClk.dclk[0]	= -clkCorrections[1];
			ssrUra.ura		= uras[1];
			tracepdeex (6, std::cout, "\n   RTCM_last_Clk  %s %s %8.4f", referenceTime.to_string().c_str(), Sat.id().c_str(), ssrClk.dclk[0]);
			tracepdeex (6, std::cout, "\n   RTCM_last_Eph  %s %s %8.4f %8.4f %8.4f", referenceTime.to_string().c_str(), Sat.id().c_str(), ssrEph.deph[0], ssrEph.deph[1], ssrEph.deph[2]);
		}

		//adjust all clock corrections so that they remain within the bounds of the outputs
		E_Sys sys = Sat.sys;
		if (commonClockOffsetsMap[sys][0] == 0)
		{
			commonClockOffsetsMap[sys][0] = ssrClk.dclk[0];
			commonClockOffsetsMap[sys][1] = ssrClk.dclk[1];
			commonClockEpochsMap [sys][0] = ssrClkInput.vals[0].time;
			commonClockEpochsMap [sys][1] = ssrClkInput.vals[1].time;
		}
		if	( ssrClkInput.vals[0].time != commonClockEpochsMap[sys][0]
			||ssrClkInput.vals[1].time != commonClockEpochsMap[sys][1])
		{
			continue; // commonClockOffset is lagging/leading this sat's clock, skip
		}
		ssrClk.dclk[0] -= commonClockOffsetsMap[sys][0];
		ssrClk.dclk[1] -= commonClockOffsetsMap[sys][1];
	}
}


int RtcmEncoder::getUdiIndex(int udi)
{
	for (int i = 0; i < 16; i++) // 16 from updateInterval[16] above
	{
		if (updateInterval[i] == udi)
		{
			return i;
		}
	}

	BOOST_LOG_TRIVIAL(error) << "Error: udi is not valid :" << udi << ").";

	return -1;
}

void RtcmEncoder::encodeWriteMessages(
	std::ostream& outputStream)
{
	if (outputStream)
	{
		outputStream.write((const char*) &data[0], data.size());
		data.clear();
	}
}

bool RtcmEncoder::encodeWriteMessageToBuffer(
	vector<uint8_t>& buffer)
{
	int i = 0;
	int messLength = buffer.size();

	if (buffer.empty())
	{
		return false;
	}

	if (messLength > 1023)
	{
		BOOST_LOG_TRIVIAL(error) << "Error: message length exceeds the limit.";
		return false;
	}

// 	unsigned char nbuf[messLength+6];
	vector<uint8_t> newbuffer(messLength + 6);
	unsigned char* nbuf	= newbuffer.data();
	unsigned char* buf	= buffer.data();

	i = setbituInc(nbuf, i,  8, RTCM_PREAMBLE);
	i = setbituInc(nbuf, i,  6, 0);
	i = setbituInc(nbuf, i, 10, messLength);

	memcpy(nbuf + 3, buf, sizeof(uint8_t)* messLength);
	i = i + messLength * 8;

	const unsigned char* bCrcBuf = (const unsigned char *)nbuf;
	unsigned int crcCalc = crc24q(bCrcBuf, sizeof(char) * (messLength + 3));

	unsigned char* bCrcCalc = (unsigned char*)&crcCalc;
	unsigned int b1 = 0;
	unsigned int b2 = 0;
	unsigned int b3 = 0;
	setbituInc((unsigned char *)&b1, 0, 8, *(bCrcCalc + 0));
	setbituInc((unsigned char *)&b2, 0, 8, *(bCrcCalc + 1));
	setbituInc((unsigned char *)&b3, 0, 8, *(bCrcCalc + 2));
	i = setbituInc(nbuf, i, 8, b3);
	i = setbituInc(nbuf, i, 8, b2);
	i = setbituInc(nbuf, i, 8, b1);

	data.insert(data.end(), &nbuf[0], &nbuf[messLength + 6]);

	return true;
}


vector<uint8_t> RtcmEncoder::encodeTimeStampRTCM()
{
	// Custom message code, for crcsi maximum length 4096 bits or 512 bytes.
	unsigned int messCode = +RtcmMessageType::CUSTOM;
	unsigned int messType = +E_RTCMSubmessage::TIMESTAMP;

	GTime now = timeGet();

	int i = 0;
	int byteLen = 11;
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();
	unsigned int reserved = 0;
	i = setbituInc(buf, i, 12,	messCode);
	i = setbituInc(buf, i, 8,	messType);
	i = setbituInc(buf, i, 4,	reserved);

	long int milliseconds = now.bigTime * 1000;

	unsigned int chunk;
	chunk = milliseconds;	i = setbituInc(buf, i, 32,	chunk);		milliseconds >>= 32;
	chunk = milliseconds;	i = setbituInc(buf, i, 32,	chunk);

	traceTimestamp(now);

	return buffer;
}


/** encode SSR header information
*/
int RtcmEncoder::encodeSsrHeader(
	unsigned char*	buf,					///< byte data
	E_Sys			sys,					///< system to encode
	RtcmMessageType	messCode,				///< RTCM message code to encode ephemeris of
	SSRMeta&		ssrMeta,				///< SSR metadata
	int				iod,					///< IOD SSR
	int				dispBiasConistInd,		///< Dispersive Bias Consistency Indicator (for phase bias only)
	int				MWConistInd)			///< MW Consistency Indicator (for phase bias only)
{
	string	messCodeStr	= messCode._to_string();
	string	messTypeStr	= messCodeStr.substr(8);

	int ne = 0;
	int ns = 0;
	switch (sys)
	{
		case E_Sys::GPS:	ne = 20;	ns = 6;		break;
		case E_Sys::GLO:	ne = 17;	ns = 6;		break;
		case E_Sys::GAL:	ne = 20;	ns = 6;		break;
		case E_Sys::QZS:	ne = 20;	ns = 4;		break;
		case E_Sys::BDS:	ne = 20;	ns = 6;		break;
		case E_Sys::SBS:	ne = 20;	ns = 6;		break;
		default:
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system: " << sys._to_string() << " in " << __FUNCTION__;
			return 0;
	}

	int i = 0;
	i = setbituInc(buf, i, 12, messCode);
	i = setbituInc(buf, i, ne, ssrMeta.epochTime1s);
	i = setbituInc(buf, i,  4, ssrMeta.updateIntIndex);
	i = setbituInc(buf, i,  1, ssrMeta.multipleMessage);

	if	( messTypeStr == "ORB_CORR"
		||messTypeStr == "COMB_CORR")
	{
		i = setbituInc(buf, i,  1, ssrMeta.referenceDatum);
	}

	i = setbituInc(buf, i,  4, iod);
	i = setbituInc(buf, i, 16, ssrMeta.provider);
	i = setbituInc(buf, i,  4, ssrMeta.solution);

	if  ( messTypeStr == "PHASE_BIAS")
	{
		i = setbituInc(buf, i,  1, dispBiasConistInd);
		i = setbituInc(buf, i,  1, MWConistInd);
	}

	i = setbituInc(buf, i, ns, ssrMeta.numSats);

	return i;
}

/** encode orbit/clock messages
*/
vector<uint8_t> RtcmEncoder::encodeSsrOrbClk(
	SsrOutMap&		ssrOutMap,				///< orbits/clocks to encode
	RtcmMessageType	messCode)				///< RTCM message code to encode ephemeris of
{
	string	messCodeStr	= messCode._to_string();
	string	messTypeStr	= messCodeStr.substr(8);

	int numSats = ssrOutMap.size();
	if (numSats == 0)
	{
		return vector<uint8_t>();
	}

	auto& [Sat, ssrOut]		= *ssrOutMap.begin();
	auto& ssrEph			= ssrOut.ssrEph;
	auto& ssrMeta			= ssrEph.ssrMeta;
	ssrMeta.numSats			= numSats;

	int np = 0;
	int ni = 0;
	int nj = 0;
	switch (Sat.sys)
	{
		case E_Sys::GPS:	np = 6;		ni =  8;	nj =  0;	break;
		case E_Sys::GLO:	np = 5;		ni =  8;	nj =  0;	break;
		case E_Sys::GAL:	np = 6;		ni = 10;	nj =  0;	break;
		case E_Sys::QZS:	np = 4;		ni =  8;	nj =  0;	break;
		case E_Sys::BDS:	np = 6;		ni =  8;	nj = 10;	break;
		case E_Sys::SBS:	np = 6;		ni =  9;	nj =  0;	break;
		default:
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system: " << Sat.sysName() << " in " << __FUNCTION__;
			return vector<uint8_t>();
	}

	int bitLen = 0;
	switch (messCode)
	{
		case RtcmMessageType::GPS_SSR_ORB_CORR:			bitLen = 68 + numSats * 135;	break;
		case RtcmMessageType::GPS_SSR_CLK_CORR:			bitLen = 67 + numSats *  76;	break;
		case RtcmMessageType::GPS_SSR_COMB_CORR:		bitLen = 68 + numSats * 205;	break;
		case RtcmMessageType::GPS_SSR_HR_CLK_CORR:		bitLen = 67 + numSats *  28;	break;
		case RtcmMessageType::GLO_SSR_ORB_CORR:			bitLen = 65 + numSats * 134;	break;
		case RtcmMessageType::GLO_SSR_CLK_CORR:			bitLen = 64 + numSats *  75;	break;
		case RtcmMessageType::GLO_SSR_COMB_CORR:		bitLen = 65 + numSats * 204;	break;
		case RtcmMessageType::GLO_SSR_HR_CLK_CORR:		bitLen = 64 + numSats *  27;	break;
		case RtcmMessageType::GAL_SSR_ORB_CORR:			bitLen = 68 + numSats * 137;	break;
		case RtcmMessageType::GAL_SSR_CLK_CORR:			bitLen = 67 + numSats *  76;	break;
		case RtcmMessageType::GAL_SSR_COMB_CORR:		bitLen = 68 + numSats * 207;	break;
		case RtcmMessageType::GAL_SSR_HR_CLK_CORR:		bitLen = 67 + numSats *  28;	break;
		case RtcmMessageType::QZS_SSR_ORB_CORR:			bitLen = 66 + numSats * 133;	break;
		case RtcmMessageType::QZS_SSR_CLK_CORR:			bitLen = 65 + numSats *  74;	break;
		case RtcmMessageType::QZS_SSR_COMB_CORR:		bitLen = 66 + numSats * 203;	break;
		case RtcmMessageType::QZS_SSR_HR_CLK_CORR:		bitLen = 65 + numSats *  26;	break;
		case RtcmMessageType::BDS_SSR_ORB_CORR:			bitLen = 68 + numSats * 145;	break;
		case RtcmMessageType::BDS_SSR_CLK_CORR:			bitLen = 67 + numSats *  76;	break;
		case RtcmMessageType::BDS_SSR_COMB_CORR:		bitLen = 68 + numSats * 215;	break;
		case RtcmMessageType::BDS_SSR_HR_CLK_CORR:		bitLen = 67 + numSats *  28;	break;
		case RtcmMessageType::SBS_SSR_ORB_CORR:			bitLen = 68 + numSats * 160;	break;
		case RtcmMessageType::SBS_SSR_CLK_CORR:			bitLen = 67 + numSats *  76;	break;
		case RtcmMessageType::SBS_SSR_COMB_CORR:		bitLen = 68 + numSats * 230;	break;
		case RtcmMessageType::SBS_SSR_HR_CLK_CORR:		bitLen = 67 + numSats *  28;	break;
		default:	return vector<uint8_t>();
	}

	int byteLen = ceil(bitLen / 8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();

	int i = 0;
	// Write the header information.
	i = encodeSsrHeader(buf, Sat.sys, messCode, ssrMeta, ssrEph.iod);

	for (auto& [Sat, ssrOut] : ssrOutMap)
	{
		auto& ssrEph = ssrOut.ssrEph;
		auto& ssrClk = ssrOut.ssrClk;

		SSRHRClk ssrHRClk;
		ssrHRClk.ssrMeta	= ssrClk.ssrMeta;
		ssrHRClk.t0			= ssrClk.t0;
		ssrHRClk.udi		= ssrClk.udi;
		ssrHRClk.iod		= ssrClk.iod;

		//convert doubles to scaled integers
		int deph	[3];
		int ddeph	[3];
		int dclk	[3];
		deph[0]		= (int)round(ssrEph.deph[0]		/ 0.1e-3);
		deph[1]		= (int)round(ssrEph.deph[1]		/ 0.4e-3);
		deph[2]		= (int)round(ssrEph.deph[2]		/ 0.4e-3);
		ddeph[0]	= (int)round(ssrEph.ddeph[0]	/ 0.001e-3);
		ddeph[1]	= (int)round(ssrEph.ddeph[1]	/ 0.004e-3);
		ddeph[2]	= (int)round(ssrEph.ddeph[2]	/ 0.004e-3);
		dclk[0]		= (int)round(ssrClk.dclk[0]		/ 0.1e-3);
		dclk[1]		= (int)round(ssrClk.dclk[1]		/ 0.001e-3);
		dclk[2]		= (int)round(ssrClk.dclk[2]		/ 0.00002e-3);

		i = setbituInc(buf, i, np, Sat.prn);

		if	( messTypeStr == "ORB_CORR"
			||messTypeStr == "COMB_CORR")
		{
			i = setbituInc(buf, i, nj, ssrEph.iodcrc);
			i = setbituInc(buf, i, ni, ssrEph.iode);

			i = setbitsInc(buf, i, 22, deph[0]);
			i = setbitsInc(buf, i, 20, deph[1]);
			i = setbitsInc(buf, i, 20, deph[2]);
			i = setbitsInc(buf, i, 21, ddeph[0]);
			i = setbitsInc(buf, i, 19, ddeph[1]);
			i = setbitsInc(buf, i, 19, ddeph[2]);

			lastRegSsrEphMap[Sat] = ssrEph;

			traceSsrEph(messCode, Sat, ssrEph);
		}

		if	( messTypeStr == "CLK_CORR"
			||messTypeStr == "COMB_CORR")
		{
			try
			{
				auto& lastSsrEph = lastRegSsrEphMap.at(Sat);

				if (ssrEph.iode != lastSsrEph.iode)
				{
					return vector<uint8_t>();
				}
			}
			catch (...)
			{
				return vector<uint8_t>();
			}

			i = setbitsInc(buf, i, 22, dclk[0]);
			i = setbitsInc(buf, i, 21, dclk[1]);
			i = setbitsInc(buf, i, 27, dclk[2]);

			lastRegSsrClkMap[Sat] = ssrClk;

			traceSsrClk(messCode, Sat, ssrClk);
		}

		if	( messTypeStr == "HR_CLK_CORR")
		{
			try
			{
				auto& lastSsrEph = lastRegSsrEphMap.at(Sat);

				if (ssrEph.iode != lastSsrEph.iode)
				{
					return vector<uint8_t>();
				}
			}
			catch (...)
			{
				return vector<uint8_t>();
			}

			try
			{
				auto& lastRegSsrClk = lastRegSsrClkMap.at(Sat);

				GTime currentTime = ssrHRClk.t0;
				double tClk = (currentTime - lastRegSsrClk.t0).to_double();

				double dclkReg	= lastRegSsrClk.dclk[0]
								+ lastRegSsrClk.dclk[1] * tClk
								+ lastRegSsrClk.dclk[2] * tClk * tClk;

				ssrHRClk.hrclk	= ssrClk.dclk[0] - dclkReg;
			}
			catch (...)
			{
				return vector<uint8_t>();
			}

			i = setbitsInc(buf, i, 22, ssrHRClk.hrclk	/ 0.1e-3);

			traceSsrHRClk(messCode, Sat, ssrHRClk);
		}
	}

	int bitl = byteLen * 8 - i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Orbit/Clock.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << "\n";
	}
	i = setbituInc(buf, i, bitl, 0);

	return buffer;
}

/** encode phase bias messages
*/
vector<uint8_t> RtcmEncoder::encodeSsrPhase(
	SsrPBMap&		ssrPBMap,				///< phase biases to encode
	RtcmMessageType	messCode)				///< RTCM message code to encode ephemeris of
{
	int numSats = ssrPBMap.size();
	if (numSats == 0)
	{
		return vector<uint8_t>();
	}

	int totalNumBias = 0;
	for (auto& [Sat, ssrPhasBias] : ssrPBMap)
	{
		totalNumBias += ssrPhasBias.obsCodeBiasMap.size();
	}
	if (totalNumBias == 0)
	{
		return vector<uint8_t>();
	}

	auto& [Sat, ssrPhasBias]	= *ssrPBMap.begin();
	auto& ssrMeta				= ssrPhasBias.ssrMeta;
	ssrMeta.numSats				= numSats;

	int np = 0;
	switch (Sat.sys)
	{
		case E_Sys::GPS:	np = 6;		break;
		case E_Sys::GLO:	np = 5;		break;
		case E_Sys::GAL:	np = 6;		break;
		case E_Sys::QZS:	np = 4;		break;
		case E_Sys::BDS:	np = 6;		break;
		case E_Sys::SBS:	np = 6;		break;
		default:
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system: " << Sat.sysName() << " in " << __FUNCTION__;
			return vector<uint8_t>();
	}

	int bitLen = 0;
	switch (messCode)
	{
		case RtcmMessageType::GPS_SSR_PHASE_BIAS:	bitLen = 69 + numSats * 28 + totalNumBias * 32;		break;
		case RtcmMessageType::GLO_SSR_PHASE_BIAS:	bitLen = 66 + numSats * 27 + totalNumBias * 32;		break;
		case RtcmMessageType::GAL_SSR_PHASE_BIAS:	bitLen = 69 + numSats * 28 + totalNumBias * 32;		break;
		case RtcmMessageType::QZS_SSR_PHASE_BIAS:	bitLen = 67 + numSats * 26 + totalNumBias * 32;		break;
		case RtcmMessageType::BDS_SSR_PHASE_BIAS:	bitLen = 69 + numSats * 28 + totalNumBias * 32;		break;
		case RtcmMessageType::SBS_SSR_PHASE_BIAS:	bitLen = 69 + numSats * 28 + totalNumBias * 32;		break;
		default:	return vector<uint8_t>();
	}

	int byteLen = ceil(bitLen / 8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();

	int i = 0;
	// Write the header information.
	int dispBiasConistInd	= ssrPhasBias.ssrPhase.dispBiasConistInd;
	int MWConistInd			= ssrPhasBias.ssrPhase.MWConistInd;
	i = encodeSsrHeader(buf, Sat.sys, messCode, ssrMeta, ssrPhasBias.iod, dispBiasConistInd, MWConistInd);

	for (auto& [Sat, ssrPhasBias] : ssrPBMap)
	{
		ssrPhasBias.udi		= updateInterval[ssrMeta.updateIntIndex];	// for rtcmTrace (debugging)

		SSRPhase ssrPhase = ssrPhasBias.ssrPhase;

		unsigned int nbias	= ssrPhasBias.obsCodeBiasMap.size();
		int yawAngle		= (int)round(ssrPhase.yawAngle	*  256	/ SC2RAD);
		int yawRate			= (int)round(ssrPhase.yawRate	* 8192	/ SC2RAD);

		i = setbituInc(buf, i, np, Sat.prn);
		i = setbituInc(buf, i,  5, nbias);
		i = setbituInc(buf, i,  9, yawAngle);
		i = setbitsInc(buf, i,  8, yawRate);

		for (auto& [obsCode, entry] : ssrPhasBias.obsCodeBiasMap)
		{
			SSRPhaseCh ssrPhaseCh = ssrPhasBias.ssrPhaseChs[obsCode];

			//BOOST_LOG_TRIVIAL(debug) << "Phase, obsCode : " << obsCode << "\n";
			//BOOST_LOG_TRIVIAL(debug) << "E_sys         : " << sys << "\n";
			//BOOST_LOG_TRIVIAL(debug) << "mCodes_gps.size() : " << mCodes_gps.size() << "\n";
			//print_map( mCodes_gps.left, " E_ObsCode <--> RTCM ", BOOST_LOG_TRIVIAL(debug) );

			int rtcmCode = 0;
			if		(Sat.sys == +E_Sys::GPS)	{	rtcmCode = mCodes_gps.left.at(obsCode);	}	//todo aaron, crash heaven, needs else, try
			else if (Sat.sys == +E_Sys::GLO)	{	rtcmCode = mCodes_glo.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::GAL)	{	rtcmCode = mCodes_gal.left.at(obsCode); }
			else if (Sat.sys == +E_Sys::QZS)	{	rtcmCode = mCodes_qzs.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::BDS)	{	rtcmCode = mCodes_bds.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::SBS)	{	rtcmCode = mCodes_sbs.left.at(obsCode);	}

			//BOOST_LOG_TRIVIAL(debug) << "rtcmCode      : " << rtcmCode << "\n";

			int bias = (int)round(entry.bias / 0.0001);

			i = setbituInc(buf, i,  5, rtcmCode);
			i = setbituInc(buf, i,  1, ssrPhaseCh.signalIntInd);
			i = setbituInc(buf, i,  2, ssrPhaseCh.signalWLIntInd);
			i = setbituInc(buf, i,  4, ssrPhaseCh.signalDisconCnt);
			i = setbitsInc(buf, i, 20, bias);

			traceSsrPhasBias(messCode, Sat, obsCode, ssrPhasBias);
		}
	}

	int bitl = byteLen * 8 - i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Phase.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << "\n";
	}

	i = setbituInc(buf, i, bitl, 0);

	return buffer;
}

/** encode code bias messages
*/
vector<uint8_t> RtcmEncoder::encodeSsrCode(
	SsrCBMap&		ssrCBMap,				///< code biases to encode
	RtcmMessageType	messCode)				///< RTCM message code to encode ephemeris of
{
	int numSats = ssrCBMap.size();
	if (numSats == 0)
	{
		return vector<uint8_t>();
	}

	int totalNumBias = 0;
	for (auto& [Sat, ssrCodeBias] : ssrCBMap)
	{
		totalNumBias += ssrCodeBias.obsCodeBiasMap.size();
	}
	if (totalNumBias == 0)
	{
		return vector<uint8_t>();
	}

	auto& [Sat, ssrCodeBias]	= *ssrCBMap.begin();
	auto& ssrMeta				= ssrCodeBias.ssrMeta;
	ssrMeta.numSats				= numSats;

	int np = 0;
	switch (Sat.sys)
	{
		case E_Sys::GPS:	np = 6;		break;
		case E_Sys::GLO:	np = 5;		break;
		case E_Sys::GAL:	np = 6;		break;
		case E_Sys::QZS:	np = 4;		break;
		case E_Sys::BDS:	np = 6;		break;
		case E_Sys::SBS:	np = 6;		break;
		default:
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system: " << Sat.sysName() << " in " << __FUNCTION__;
			return vector<uint8_t>();
	}

	int bitLen = 0;
	switch (messCode)
	{
		case RtcmMessageType::GPS_SSR_CODE_BIAS:	bitLen = 67 + numSats * 11 + totalNumBias * 19;		break;
		case RtcmMessageType::GLO_SSR_CODE_BIAS:	bitLen = 64 + numSats * 10 + totalNumBias * 19;		break;
		case RtcmMessageType::GAL_SSR_CODE_BIAS:	bitLen = 67 + numSats * 11 + totalNumBias * 19;		break;
		case RtcmMessageType::QZS_SSR_CODE_BIAS:	bitLen = 65 + numSats *  9 + totalNumBias * 19;		break;
		case RtcmMessageType::BDS_SSR_CODE_BIAS:	bitLen = 67 + numSats * 11 + totalNumBias * 19;		break;
		case RtcmMessageType::SBS_SSR_CODE_BIAS:	bitLen = 67 + numSats * 11 + totalNumBias * 19;		break;
		default:	return vector<uint8_t>();
	}

	int byteLen = ceil(bitLen / 8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();

	int i = 0;
	// Write the header information.
	i = encodeSsrHeader(buf, Sat.sys, messCode, ssrMeta, ssrCodeBias.iod);

	for (auto& [Sat, ssrCodeBias] : ssrCBMap)
	{
		ssrCodeBias.udi		= updateInterval[ssrMeta.updateIntIndex];	// for rtcmTrace (debugging)

		unsigned int nbias	= ssrCodeBias.obsCodeBiasMap.size();

		i = setbituInc(buf, i, np, Sat.prn);
		i = setbituInc(buf, i,  5, nbias);

		for (auto& [obsCode, entry] : ssrCodeBias.obsCodeBiasMap)
		{
			int rtcmCode = 0;
			if		(Sat.sys == +E_Sys::GPS)	{	rtcmCode = mCodes_gps.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::GLO)	{	rtcmCode = mCodes_glo.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::GAL)	{	rtcmCode = mCodes_gal.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::QZS)	{	rtcmCode = mCodes_qzs.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::BDS)	{	rtcmCode = mCodes_bds.left.at(obsCode);	}
			else if (Sat.sys == +E_Sys::SBS)	{	rtcmCode = mCodes_sbs.left.at(obsCode);	}

			int bias = (int)round(entry.bias / 0.01);

			i = setbituInc(buf, i,  5, rtcmCode);
			i = setbitsInc(buf, i, 14, bias);

			traceSsrCodeBias(messCode, Sat, obsCode, ssrCodeBias);
		}
	}

	int bitl = byteLen * 8 - i;
	if (bitl > 7)
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR Code.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << "\n";
	}
	i = setbituInc(buf, i, bitl, 0);

	return buffer;
}

/** encode URA messages
*/
vector<uint8_t> RtcmEncoder::encodeSsrUra(
	SsrOutMap&		ssrOutMap,				///< URAs to encode
	RtcmMessageType	messCode)				///< RTCM message code to encode ephemeris of
{
	int numSats = ssrOutMap.size();
	if (numSats == 0)
	{
		return vector<uint8_t>();
	}

	auto& [Sat, ssrOut]		= *ssrOutMap.begin();
	auto& ssrUra			= ssrOut.ssrUra;
	auto& ssrMeta			= ssrUra.ssrMeta;
	ssrMeta.numSats			= numSats;

	int np = 0;
	switch (Sat.sys)
	{
		case E_Sys::GPS:	np = 6;		break;
		case E_Sys::GLO:	np = 5;		break;
		case E_Sys::GAL:	np = 6;		break;
		case E_Sys::QZS:	np = 4;		break;
		case E_Sys::BDS:	np = 6;		break;
		case E_Sys::SBS:	np = 6;		break;
		default:
			BOOST_LOG_TRIVIAL(error) << "Error: unrecognised system: " << Sat.sysName() << " in " << __FUNCTION__;
			return vector<uint8_t>();
	}

	int bitLen = 0;
	switch (messCode)
	{
		case RtcmMessageType::GPS_SSR_URA:		bitLen = 67 + numSats * 12;		break;
		case RtcmMessageType::GLO_SSR_URA:		bitLen = 64 + numSats * 11;		break;
		case RtcmMessageType::GAL_SSR_URA:		bitLen = 67 + numSats * 12;		break;
		case RtcmMessageType::QZS_SSR_URA:		bitLen = 65 + numSats * 10;		break;
		case RtcmMessageType::BDS_SSR_URA:		bitLen = 67 + numSats * 12;		break;
		case RtcmMessageType::SBS_SSR_URA:		bitLen = 67 + numSats * 12;		break;
		default:	return vector<uint8_t>();
	}

	int byteLen = ceil(bitLen / 8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();

	int i = 0;
	// Write the header information.
	i = encodeSsrHeader(buf, Sat.sys, messCode, ssrMeta, ssrUra.iod);

	for (auto& [Sat, ssrOut] : ssrOutMap)
	{
		auto& ssrUra = ssrOut.ssrUra;

		ssrUra.udi		= updateInterval[ssrMeta.updateIntIndex];	// for rtcmTrace (debugging)

		int uraClassValue = uraToClassValue(ssrUra.ura);

		i = setbituInc(buf, i, np, Sat.prn);
		i = setbituInc(buf, i,  6, uraClassValue);

		traceSsrUra(messCode, Sat, ssrUra);
	}

	int bitl = byteLen * 8 - i;
	if (bitl > 7 )
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding SSR URA.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << "\n";
	}
	i = setbituInc(buf, i, bitl, 0);

	return buffer;
}

/** encode GPS/GAL/BDS/QZS ephemeris messages
*/
vector<uint8_t> RtcmEncoder::encodeEphemeris(
		Eph&			eph,					///< ephemeris to encode
		RtcmMessageType	messCode)				///< RTCM message code to encode ephemeris of
{
	int bitLen = 0;
	switch (messCode)
	{
		case RtcmMessageType:: GPS_EPHEMERIS:		{ bitLen	= 488;	break; }
		case RtcmMessageType:: BDS_EPHEMERIS:		{ bitLen	= 511;	break; }
		case RtcmMessageType:: QZS_EPHEMERIS:		{ bitLen	= 485;	break; }
		case RtcmMessageType:: GAL_FNAV_EPHEMERIS:	{ bitLen	= 496;	break; }
		case RtcmMessageType:: GAL_INAV_EPHEMERIS:	{ bitLen	= 504;	break; }
		default:	return vector<uint8_t>();
	}

	int byteLen = ceil(bitLen / 8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();

	int i = 0;
	i = setbituInc(buf, i, 12, messCode);

	auto Sat	= eph.Sat;
	auto type	= eph.type;
	if (Sat.sys == +E_Sys::GPS)
	{
		int idot			= (int)			round(eph.idot	/P2_43/SC2RAD);
		int tocs			= (int)			round(eph.tocs	/16.0 );
		int f2				= (int)			round(eph.f2	/P2_55);
		int f1				= (int)			round(eph.f1	/P2_43);
		int f0				= (int)			round(eph.f0	/P2_31);
		int crs				= (int)			round(eph.crs	/P2_5 );
		int deln			= (int)			round(eph.deln	/P2_43/SC2RAD);
		int M0				= (int)			round(eph.M0	/P2_31/SC2RAD);
		int cuc				= (int)			round(eph.cuc	/P2_29);
		unsigned int e		= (unsigned int)round(eph.e		/P2_33);
		int cus				= (int)			round(eph.cus	/P2_29);
		unsigned int sqrtA	= (unsigned int)round(eph.sqrtA	/P2_19);
		int toes			= (int)			round(eph.toes	/16.0 );
		int cic				= (int)			round(eph.cic	/P2_29);
		int OMG0			= (int)			round(eph.OMG0	/P2_31/SC2RAD);
		int cis				= (int)			round(eph.cis	/P2_29);
		int i0				= (int)			round(eph.i0	/P2_31/SC2RAD);
		int crc				= (int)			round(eph.crc	/P2_5 );
		int omg				= (int)			round(eph.omg	/P2_31/SC2RAD);
		int OMGd			= (int)			round(eph.OMGd	/P2_43/SC2RAD);
		int tgd				= (int)			round(eph.tgd[0]/P2_31);

		i = setbituInc(buf, i,  6, Sat.prn);
		i = setbituInc(buf, i, 10, eph.weekRollOver);
		i = setbituInc(buf, i,  4, eph.sva);
		i = setbituInc(buf, i,  2, eph.code);
		i = setbitsInc(buf, i, 14, idot);
		i = setbituInc(buf, i,  8, eph.iode);
		i = setbituInc(buf, i, 16, tocs);
		i = setbitsInc(buf, i,  8, f2);
		i = setbitsInc(buf, i, 16, f1);
		i = setbitsInc(buf, i, 22, f0);
		i = setbituInc(buf, i, 10, eph.iodc);
		i = setbitsInc(buf, i, 16, crs);
		i = setbitsInc(buf, i, 16, deln);
		i = setbitsInc(buf, i, 32, M0);
		i = setbitsInc(buf, i, 16, cuc);
		i = setbituInc(buf, i, 32, e);
		i = setbitsInc(buf, i, 16, cus);
		i = setbituInc(buf, i, 32, sqrtA);
		i = setbituInc(buf, i, 16, toes);
		i = setbitsInc(buf, i, 16, cic);
		i = setbitsInc(buf, i, 32, OMG0);
		i = setbitsInc(buf, i, 16, cis);
		i = setbitsInc(buf, i, 32, i0);
		i = setbitsInc(buf, i, 16, crc);
		i = setbitsInc(buf, i, 32, omg);
		i = setbitsInc(buf, i, 24, OMGd);
		i = setbitsInc(buf, i,  8, tgd);
		i = setbituInc(buf, i,  6, eph.svh);
		i = setbituInc(buf, i,  1, eph.flag);
		i = setbituInc(buf, i,  1, eph.fitFlag);
	}
	else if (Sat.sys == +E_Sys::BDS)
	{
		int idot			= (int)			round(eph.idot	/P2_43/SC2RAD);
		int tocs			= (int)			round(eph.tocs	/8.0  );
		int f2				= (int)			round(eph.f2	/P2_66);
		int f1				= (int)			round(eph.f1	/P2_50);
		int f0				= (int)			round(eph.f0	/P2_33);
		int crs				= (int)			round(eph.crs	/P2_6 );
		int deln			= (int)			round(eph.deln	/P2_43/SC2RAD);
		int M0				= (int)			round(eph.M0	/P2_31/SC2RAD);
		int cuc				= (int)			round(eph.cuc	/P2_31);
		unsigned int e		= (unsigned int)round(eph.e		/P2_33);
		int cus				= (int)			round(eph.cus	/P2_31);
		unsigned int sqrtA	= (unsigned int)round(eph.sqrtA	/P2_19);
		int toes			= (int)			round(eph.toes	/8.0  );
		int cic				= (int)			round(eph.cic	/P2_31);
		int OMG0			= (int)			round(eph.OMG0	/P2_31/SC2RAD);
		int cis				= (int)			round(eph.cis	/P2_31);
		int i0				= (int)			round(eph.i0	/P2_31/SC2RAD);
		int crc				= (int)			round(eph.crc	/P2_6 );
		int omg				= (int)			round(eph.omg	/P2_31/SC2RAD);
		int OMGd			= (int)			round(eph.OMGd	/P2_43/SC2RAD);
		int tgd1			= (int)			round(eph.tgd[0]/1E-10);
		int tgd2			= (int)			round(eph.tgd[1]/1E-10);

		i = setbituInc(buf, i,  6, Sat.prn);
		i = setbituInc(buf, i, 13, eph.weekRollOver);
		i = setbituInc(buf, i,  4, eph.sva);
		i = setbitsInc(buf, i, 14, idot);
		i = setbituInc(buf, i,  5, eph.aode);
		i = setbituInc(buf, i, 17, tocs);
		i = setbitsInc(buf, i, 11, f2);
		i = setbitsInc(buf, i, 22, f1);
		i = setbitsInc(buf, i, 24, f0);
		i = setbituInc(buf, i,  5, eph.aodc);
		i = setbitsInc(buf, i, 18, crs);
		i = setbitsInc(buf, i, 16, deln);
		i = setbitsInc(buf, i, 32, M0);
		i = setbitsInc(buf, i, 18, cuc);
		i = setbituInc(buf, i, 32, e);
		i = setbitsInc(buf, i, 18, cus);
		i = setbituInc(buf, i, 32, sqrtA);
		i = setbituInc(buf, i, 17, toes);
		i = setbitsInc(buf, i, 18, cic);
		i = setbitsInc(buf, i, 32, OMG0);
		i = setbitsInc(buf, i, 18, cis);
		i = setbitsInc(buf, i, 32, i0);
		i = setbitsInc(buf, i, 18, crc);
		i = setbitsInc(buf, i, 32, omg);
		i = setbitsInc(buf, i, 24, OMGd);
		i = setbitsInc(buf, i, 10, tgd1);
		i = setbitsInc(buf, i, 10, tgd2);
		i = setbituInc(buf, i,  1, eph.svh);
	}
	else if (Sat.sys == +E_Sys::QZS)
	{
		int tocs			= (int)			round(eph.tocs	/16.0 );
		int f2				= (int)			round(eph.f2	/P2_55);
		int f1				= (int)			round(eph.f1	/P2_43);
		int f0				= (int)			round(eph.f0	/P2_31);
		int crs				= (int)			round(eph.crs	/P2_5 );
		int deln			= (int)			round(eph.deln	/P2_43/SC2RAD);
		int M0				= (int)			round(eph.M0	/P2_31/SC2RAD);
		int cuc				= (int)			round(eph.cuc	/P2_29);
		unsigned int e		= (unsigned int)round(eph.e		/P2_33);
		int cus				= (int)			round(eph.cus	/P2_29);
		unsigned int sqrtA	= (unsigned int)round(eph.sqrtA	/P2_19);
		int toes			= (int)			round(eph.toes	/16.0 );
		int cic				= (int)			round(eph.cic	/P2_29);
		int OMG0			= (int)			round(eph.OMG0	/P2_31/SC2RAD);
		int cis				= (int)			round(eph.cis	/P2_29);
		int i0				= (int)			round(eph.i0	/P2_31/SC2RAD);
		int crc				= (int)			round(eph.crc	/P2_5 );
		int omg				= (int)			round(eph.omg	/P2_31/SC2RAD);
		int OMGd			= (int)			round(eph.OMGd	/P2_43/SC2RAD);
		int idot			= (int)			round(eph.idot	/P2_43/SC2RAD);
		int tgd				= (int)			round(eph.tgd[0]/P2_31);

		i = setbituInc(buf, i,  4, Sat.prn);
		i = setbituInc(buf, i, 16, tocs);
		i = setbitsInc(buf, i,  8, f2);
		i = setbitsInc(buf, i, 16, f1);
		i = setbitsInc(buf, i, 22, f0);
		i = setbituInc(buf, i,  8, eph.iode);
		i = setbitsInc(buf, i, 16, crs);
		i = setbitsInc(buf, i, 16, deln);
		i = setbitsInc(buf, i, 32, M0);
		i = setbitsInc(buf, i, 16, cuc);
		i = setbituInc(buf, i, 32, e);
		i = setbitsInc(buf, i, 16, cus);
		i = setbituInc(buf, i, 32, sqrtA);
		i = setbituInc(buf, i, 16, toes);
		i = setbitsInc(buf, i, 16, cic);
		i = setbitsInc(buf, i, 32, OMG0);
		i = setbitsInc(buf, i, 16, cis);
		i = setbitsInc(buf, i, 32, i0);
		i = setbitsInc(buf, i, 16, crc);
		i = setbitsInc(buf, i, 32, omg);
		i = setbitsInc(buf, i, 24, OMGd);
		i = setbitsInc(buf, i, 14, idot);
		i = setbituInc(buf, i,  2, eph.code);
		i = setbituInc(buf, i, 10, eph.weekRollOver);
		i = setbituInc(buf, i,  4, eph.sva);
		i = setbituInc(buf, i,  6, eph.svh);
		i = setbitsInc(buf, i,  8, tgd);
		i = setbituInc(buf, i, 10, eph.iodc);
		i = setbituInc(buf, i,  1, eph.fitFlag);
	}
	else if (Sat.sys == +E_Sys::GAL)
	{
		int idot			= (int)			round(eph.idot	/P2_43/SC2RAD);
		int tocs			= (int)			round(eph.tocs	/60.0 );
		int f2				= (int)			round(eph.f2	/P2_59);
		int f1				= (int)			round(eph.f1	/P2_46);
		int f0				= (int)			round(eph.f0	/P2_34);
		int crs				= (int)			round(eph.crs	/P2_5 );
		int deln			= (int)			round(eph.deln	/P2_43/SC2RAD);
		int M0				= (int)			round(eph.M0	/P2_31/SC2RAD);
		int cuc				= (int)			round(eph.cuc	/P2_29);
		unsigned int e		= (unsigned int)round(eph.e		/P2_33);
		int cus				= (int)			round(eph.cus	/P2_29);
		unsigned int sqrtA	= (unsigned int)round(eph.sqrtA	/P2_19);
		int toes			= (int)			round(eph.toes	/60.0 );
		int cic				= (int)			round(eph.cic	/P2_29);
		int OMG0			= (int)			round(eph.OMG0	/P2_31/SC2RAD);
		int cis				= (int)			round(eph.cis	/P2_29);
		int i0				= (int)			round(eph.i0	/P2_31/SC2RAD);
		int crc				= (int)			round(eph.crc	/P2_5 );
		int omg				= (int)			round(eph.omg	/P2_31/SC2RAD);
		int OMGd			= (int)			round(eph.OMGd	/P2_43/SC2RAD);
		int bgd1			= (int)			round(eph.tgd[0]/P2_32);
		int bgd2			= (int)			round(eph.tgd[1]/P2_32);

		i = setbituInc(buf, i,  6, Sat.prn);
		i = setbituInc(buf, i, 12, eph.weekRollOver);
		i = setbituInc(buf, i, 10, eph.iode);
		i = setbituInc(buf, i,  8, eph.sva);
		i = setbitsInc(buf, i, 14, idot);
		i = setbituInc(buf, i, 14, tocs);
		i = setbitsInc(buf, i,  6, f2);
		i = setbitsInc(buf, i, 21, f1);
		i = setbitsInc(buf, i, 31, f0);
		i = setbitsInc(buf, i, 16, crs);
		i = setbitsInc(buf, i, 16, deln);
		i = setbitsInc(buf, i, 32, M0);
		i = setbitsInc(buf, i, 16, cuc);
		i = setbituInc(buf, i, 32, e);
		i = setbitsInc(buf, i, 16, cus);
		i = setbituInc(buf, i, 32, sqrtA);
		i = setbituInc(buf, i, 14, toes);
		i = setbitsInc(buf, i, 16, cic);
		i = setbitsInc(buf, i, 32, OMG0);
		i = setbitsInc(buf, i, 16, cis);
		i = setbitsInc(buf, i, 32, i0);
		i = setbitsInc(buf, i, 16, crc);
		i = setbitsInc(buf, i, 32, omg);
		i = setbitsInc(buf, i, 24, OMGd);
		i = setbitsInc(buf, i, 10, bgd1);

		if		(type == +E_NavMsgType::FNAV)
		{
			i = setbituInc(buf, i,  2, eph.e5a_hs);
			i = setbituInc(buf, i,  1, eph.e5a_dvs);
			i = setbituInc(buf, i,  7, 0); /* reserved */
		}
		else if (type == +E_NavMsgType::INAV)
		{
			i = setbitsInc(buf, i, 10, bgd2);
			i = setbituInc(buf, i,  2, eph.e5b_hs);
			i = setbituInc(buf, i,  1, eph.e5b_dvs);
			i = setbituInc(buf, i,  2, eph.e1_hs);
			i = setbituInc(buf, i,  1, eph.e1_dvs);
		}
	}

	int bitl = byteLen * 8 - i;
	if (bitl > 7)
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding ephmeris.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << "\n";
	}
	i = setbituInc(buf, i, bitl, 0);

	if (acsConfig.output_encoded_rtcm_json)
		traceBrdcEph(messCode, eph);

	return buffer;
}

/** encode GLO ephemeris messages
*/
vector<uint8_t> RtcmEncoder::encodeEphemeris(
		Geph&			geph,					///< ephemeris to encode
		RtcmMessageType	messCode)				///< RTCM message code to encode ephemeris of
{
	int bitLen = 360;

	int byteLen = ceil(bitLen / 8.0);
	vector<uint8_t> buffer(byteLen);
	unsigned char* buf = buffer.data();

	int i = 0;
	i = setbituInc(buf, i, 12, messCode);

	auto Sat = geph.Sat;
	{
		int vel[3], pos[3], acc[3];
		for (int j=0; j<3; j++)
		{
			vel[j]	= (int)round(geph.vel[j]/P2_20/1E3);
			pos[j]	= (int)round(geph.pos[j]/P2_11/1E3);
			acc[j]	= (int)round(geph.acc[j]/P2_30/1E3);
		}

		int gammaN	= (int)round(geph.gammaN	/ P2_40);
		int taun	= (int)round(geph.taun		/ P2_30);
		int dtaun	= (int)round(geph.dtaun		/ P2_30);

		i	= setbituInc(buf, i,  6, Sat.prn);
		i	= setbituInc(buf, i,  5, geph.frq+7);
		i	= setbituInc(buf, i,  4, 0);				// almanac health, P1
		i	= setbituInc(buf, i,  5, geph.tk_hour);
		i	= setbituInc(buf, i,  6, geph.tk_min);
		i	= setbituInc(buf, i,  1, geph.tk_sec);
		i	= setbituInc(buf, i,  1, geph.svh);
		i	= setbituInc(buf, i,  1, 0);				// P2
		i	= setbituInc(buf, i,  7, geph.tb);
		i	= setbitgInc(buf, i, 24, vel[0]);
		i	= setbitgInc(buf, i, 27, pos[0]);
		i	= setbitgInc(buf, i,  5, acc[0]);
		i	= setbitgInc(buf, i, 24, vel[1]);
		i	= setbitgInc(buf, i, 27, pos[1]);
		i	= setbitgInc(buf, i,  5, acc[1]);
		i	= setbitgInc(buf, i, 24, vel[2]);
		i	= setbitgInc(buf, i, 27, pos[2]);
		i	= setbitgInc(buf, i,  5, acc[2]);
		i	= setbituInc(buf, i,  1, 0);				// P3
		i	= setbitgInc(buf, i, 11, gammaN);
		i	= setbituInc(buf, i,  3, 0);				// P, ln
		i	= setbitgInc(buf, i, 22, taun);
		i	= setbitgInc(buf, i,  5, dtaun);
		i	= setbituInc(buf, i,  5, geph.age);
		i	= setbituInc(buf, i,  5, 0);				// P4, FT
		i	= setbituInc(buf, i, 11, geph.NT);			// GLONASS-M only
		i	= setbituInc(buf, i,  2, geph.glonassM);	// M (if GLONASS-M data feilds valid)
		i	= setbituInc(buf, i,  1, geph.moreData);	// availability of additional data
		i	= setbituInc(buf, i, 11, 0);				// NA
		i	= setbitgInc(buf, i, 32, 0);				// tauc
		i	= setbituInc(buf, i,  5, geph.N4);			// additional data and GLONASS-M only
		i	= setbitgInc(buf, i, 22, 0);				// tauGPS
		i	= setbituInc(buf, i,  1, 0);				// ln
		i	= setbituInc(buf, i,  7, 0);				// reserved
	}

	int bitl = byteLen * 8 - i;
	if (bitl > 7)
	{
		BOOST_LOG_TRIVIAL(error) << "Error encoding ephmeris.\n";
		BOOST_LOG_TRIVIAL(error) << "Error: bitl : " << bitl << ", i : " << i << ", byteLen : " << byteLen << "\n";
	}
	i = setbituInc(buf, i, bitl, 0);

	if (acsConfig.output_encoded_rtcm_json)
		traceBrdcEph(messCode, geph);

	return buffer;
}


/** set unsigned bits to byte data
*/
void setbitu(
	unsigned char*	buff,			///< byte data
	int 			pos,			///< bit position from start of data (bits)
	int				len,			///< bit length (bits) (len<=32)
	unsigned int	value)			///< value to set
{
	unsigned int mask=1u<<(len-1);

	if	( len <= 0
		||len >  32)
	{
		return;
	}

	unsigned long int invalid = (1ul<<len);

	if (value >= invalid)
	{
		BOOST_LOG_TRIVIAL(warning)
		<< "Warning: " << __FUNCTION__ << " has data outside range\n";
	}

	for (int i = pos; i < pos+len; i++, mask >>= 1)
	{
		if (value&mask)	buff[i/8] |=  (1u<<(7-i%8));
		else			buff[i/8] &= ~(1u<<(7-i%8));
	}
}

/** set signed bits to byte data
*/
void setbits(
	unsigned char*	buff,	///< byte data
	int				pos,	///< bit position from start of data (bits)
	int				len,	///< bit length (bits) (len<=32)
	int				value)	///< value to set
{
	unsigned int mask = 1u<<(len-1);

	if	( len <= 0
		||len >  32)
	{
		return;
	}

	long int invalid = (1ul<<(len-1));

	if	( +value >= invalid
		||-value >= invalid)
	{
		BOOST_LOG_TRIVIAL(warning) << "Warning: " << __FUNCTION__ << " has data outside range, setting invalid\n";
		value = -invalid;
	}

	for (int i = pos; i < pos+len; i++, mask >>= 1)
	{
		if (value&mask)	buff[i/8] |=  (1u<<(7-i%8));
		else			buff[i/8] &= ~(1u<<(7-i%8));
	}
}

/** increasingly set unsigned bits to byte data
*/
int setbituInc(
	unsigned char*	buff,	///< byte data
	int				pos,	///< bit position from start of data (bits)
	int				len,	///< bit length (bits) (len<=32)
	unsigned int	value)	///< value to set
{
	setbitu(buff, pos, len, value);
	return pos + len;
}

/** increasingly set signed bits to byte data
*/
int setbitsInc(
	unsigned char*	buff,	///< byte data
	int				pos,	///< bit position from start of data (bits)
	int				len,	///< bit length (bits) (len<=32)
	int				value)	///< value to set
{
	setbits(buff, pos, len, value);
	return pos + len;
}

/** set sign-magnitude bits applied in GLO nav messages
*/
void setbitg(
	unsigned char*	buff,	///< byte data
	int				pos,	///< bit position from start of data (bits)
	int				len,	///< bit length (bits) (len<=32)
	int				value)	///< value to set
{
    setbitu(buff, pos, 1, value<0?1:0);
    setbitu(buff, pos+1, len-1, value<0?-value:value);
}

/** increasingly set sign-magnitude bits applied in GLO nav messages
*/
int setbitgInc(
	unsigned char*	buff,	///< byte data
	int				pos,	///< bit position from start of data (bits)
	int				len,	///< bit length (bits) (len<=32)
	int				value)	///< value to set
{
	setbitg(buff, pos, len, value);
	return pos + len;
}
