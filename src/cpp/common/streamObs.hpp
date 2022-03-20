
#ifndef __ACSOBSSTREAM_HPP__
#define __ACSOBSSTREAM_HPP__

#include "station.hpp"
#include "enums.h"

//interfaces

/** Interface for streams that supply observations
*/
struct ObsStream
{
	RinexStation	rnxStation = {};
	list<ObsList>	obsListList;	
	string			sourceString;
	E_ObsWaitCode	obsWaitCode = E_ObsWaitCode::OK;

	/** Return a list of observations from the stream.
	* This function may be overridden by objects that use this interface
	*/
	virtual ObsList getObs();

	/** Return a list of observations from the stream, with a specified timestamp.
	* This function may be overridden by objects that use this interface
	*/
	ObsList getObs(
		GTime	time,			///< Timestamp to get observations for
		double	delta = 0.5)	///< Acceptable tolerance around requested time
	{
		while (1)
		{
			ObsList obsList = getObs();

			if (obsList.size() == 0)
			{
				obsWaitCode = E_ObsWaitCode::NO_DATA_WAIT;
				return obsList;
			}

			if (time == GTime::noTime())
			{
				obsWaitCode = E_ObsWaitCode::OK;
				return obsList;
			}

			if	(obsList.front().time < time - delta)
			{
				eatObs();
			}
			else if	(obsList.front().time > time + delta)
			{
				obsWaitCode = E_ObsWaitCode::NO_DATA_EVER;
				return ObsList();
			}
			else
			{
				obsWaitCode = E_ObsWaitCode::OK;
				return obsList;
			}
		}
	}


	/** Check to see if this stream has run out of data
	*/
	virtual bool isDead()
	{
		return false;
	}

	/** Remove some observations from memory
	*/
	void eatObs()
	{
		if (obsListList.size() > 0)
		{
			obsListList.pop_front();
		}
	}
};

/** Interface for streams that supply observations
*/
struct PseudoObsStream
{
	list<PseudoObsList>	obsListList;	
	string				sourceString;

	/** Return a list of observations from the stream.
	* This function may be overridden by objects that use this interface
	*/
	virtual PseudoObsList getObs();

	/** Return a list of observations from the stream, with a specified timestamp.
	* This function may be overridden by objects that use this interface
	*/
	PseudoObsList getObs(
		GTime	time,			///< Timestamp to get observations for
		double	delta = 0.5)	///< Acceptable tolerance around requested time
	{
		while (1)
		{
			PseudoObsList pseudoObsList = getObs();

			if (pseudoObsList.size() == 0)
			{
				return pseudoObsList;
			}

			if (time == GTime::noTime())
			{
				return pseudoObsList;
			}

			if		(pseudoObsList.front().time < time - delta)
			{
				eatObs();
			}
			else if	(pseudoObsList.front().time > time + delta)
			{
				return PseudoObsList();
			}
			else
			{
				return pseudoObsList;
			}
		}
	}


	/** Check to see if this stream has run out of data
	*/
	virtual bool isDead()
	{
		return false;
	}

	/** Remove some observations from memory
	*/
	void eatObs()
	{
		if (obsListList.size() > 0)
		{
			obsListList.pop_front();
		}
	}
};

#endif
