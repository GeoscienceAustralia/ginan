
#pragma once

#include "streamParser.hpp"
#include "acsConfig.hpp"
#include "receiver.hpp"
#include "enums.h"



struct ObsLister
{
	list<ObsList>	obsListList;
};

struct ObsStream : StreamParser
{
	E_ObsWaitCode	obsWaitCode = E_ObsWaitCode::OK;

	bool	isPseudoRec;

	ObsStream(
		unique_ptr<Stream>	stream_ptr,
		unique_ptr<Parser>	parser_ptr,
		bool				isPseudoRec = false)
	:	StreamParser(std::move(stream_ptr), std::move(parser_ptr)), isPseudoRec{isPseudoRec}
	{

	}

	ObsList getObs()
	{
		try
		{
			auto& obsLister = dynamic_cast<ObsLister&>(parser);

			if (obsLister.obsListList.size() < 2)
			{
				parse();
			}

			if (obsLister.obsListList.empty())
			{
				return ObsList();
			}

			ObsList& obsList = obsLister.obsListList.front();

			for (auto& obs					: only<GObs>(obsList))
			for (auto& [ftype, sigsList]	: obs.sigsLists)
			{
				E_Sys sys = obs.Sat.sys;

				if (sys == +E_Sys::GPS)
				{
					double dirty_C1W_phase = 0;
					for (auto& sig : sigsList)
					{
						if	( sig.code == +E_ObsCode::L1C)
							dirty_C1W_phase = sig.L;

						if	(  sig.code	== +E_ObsCode::L1W
							&& sig.P	== 0)
						{
							sig.L = 0;
						}
					}

					for (auto& sig : sigsList)
					if	(  sig.code	== +E_ObsCode::L1W
						&& sig.L	== 0
						&& sig.P    != 0)
					{
						sig.L = dirty_C1W_phase;
						break;
					}
				}

				sigsList.remove_if([sys](Sig& a)
					{
						return std::find(acsConfig.code_priorities[sys].begin(), acsConfig.code_priorities[sys].end(), a.code) == acsConfig.code_priorities[sys].end();
					});

				sigsList.sort([sys](Sig& a, Sig& b)
					{
						auto iterA = std::find(acsConfig.code_priorities[sys].begin(), acsConfig.code_priorities[sys].end(), a.code);
						auto iterB = std::find(acsConfig.code_priorities[sys].begin(), acsConfig.code_priorities[sys].end(), b.code);

						if (a.L == 0)		return false;
						if (b.L == 0)		return true;
						if (a.P == 0)		return false;
						if (b.P == 0)		return true;
						if (iterA < iterB)	return true;
						else				return false;
					});

				if (sigsList.empty())
				{
					continue;
				}

				Sig firstOfType = sigsList.front();

				//use first of type as representative if its in the priority list
				auto iter = std::find(acsConfig.code_priorities[sys].begin(), acsConfig.code_priorities[sys].end(), firstOfType.code);
				if (iter != acsConfig.code_priorities[sys].end())
				{
					obs.sigs[ftype] = Sig(firstOfType);
				}
			}

			return obsList;
		}
		catch(...){}

		return ObsList();
	}

	/** Return a list of observations from the stream, with a specified timestamp.
	* This function may be overridden by objects that use this interface
	*/
	ObsList getObs(
		GTime	time,			///< Timestamp to get observations for
		double	delta = 0.5)	///< Acceptable tolerance around requested time
	{
		ObsList bigObsList;
		bool foundGoodObs = false;
		while (1)
		{
			ObsList obsList = getObs();

			if		(time == GTime::noTime())				{	foundGoodObs = true;						eatObs();	bigObsList += obsList;	break;	}
			else if	(obsList.empty())						{	obsWaitCode = E_ObsWaitCode::NO_DATA_WAIT;				bigObsList += obsList;	break;	}
			else if	(obsList.front()->time	< time - delta)	{	obsWaitCode = E_ObsWaitCode::EARLY_DATA;	eatObs();	bigObsList += obsList;	break;	}
			else if	(obsList.front()->time	> time + delta)	{	obsWaitCode = E_ObsWaitCode::NO_DATA_EVER;										break;	}
			else											{	foundGoodObs = true;						eatObs();	bigObsList += obsList;			}
		}

		if		(foundGoodObs)									obsWaitCode = E_ObsWaitCode::OK;
		else if	(obsWaitCode == +E_ObsWaitCode::NO_DATA_EVER)	return ObsList();
		return bigObsList;
	}

	/** Remove some observations from memory
	*/
	void eatObs()
	{
		try
		{
			auto& obsLister = dynamic_cast<ObsLister&>(parser);

			if (obsLister.obsListList.size() > 0)
			{
				obsLister.obsListList.pop_front();
			}
		}
		catch(...){}
	}

	bool hasObs()
	{
		try
		{
			auto& obsLister = dynamic_cast<ObsLister&>(parser);

			if (obsLister.obsListList.empty())
			{
				return false;
			}

			return true;
		}
		catch(...)
		{
			return false;
		}
	}
};
