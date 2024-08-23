
#include "receiver.hpp"
#include "sinex.hpp"
#include "tides.hpp"

SinexSiteId				dummySiteid;
SinexReceiver			dummyReceiver;
SinexAntenna			dummyAntenna;
SinexSiteEcc			dummySiteEcc;

SinexSatIdentity		dummySinexSatIdentity;
SinexSatEcc				dummySinexSatEcc;




ReceiverMap						receiverMap;

void initialiseStation(
	string		id,
	Receiver&	rec)
{
	if (rec.id.empty() == false)
	{
		//already initialised
		return;
	}

	BOOST_LOG_TRIVIAL(info)
	<< "Initialising station " << id;

	rec.id = id;

	auto loadBlq = [&](vector<string> files, E_LoadingType type)
	{
		bool found = false;
		for (auto& blqfile : acsConfig.ocean_tide_loading_blq_files)
		{
			found |= readBlq(blqfile, rec, E_LoadingType::OCEAN);
		}

		if (found == false)
		{
			BOOST_LOG_TRIVIAL(warning)
			<< "Warning: No " << type._to_string() << " BLQ for " << id;
		}
	};

	loadBlq(acsConfig.ocean_tide_loading_blq_files, E_LoadingType::OCEAN);
	loadBlq(acsConfig.atmos_tide_loading_blq_files, E_LoadingType::ATMOSPHERIC);
}
