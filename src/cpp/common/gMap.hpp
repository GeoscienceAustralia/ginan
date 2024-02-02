
#pragma once

#include <map>

using std::map;

template<
typename KEY,
typename VALUE,
typename COMPARE = std::less<KEY>>
struct GMap : map<KEY, VALUE, COMPARE>
{
	map<KEY, VALUE, COMPARE>::iterator cache = map<KEY, VALUE, COMPARE>::end();

	map<KEY, VALUE, COMPARE>::iterator lower_bound(
		const KEY& key)
	{
		if (true)
		{
			if (cache == map<KEY, VALUE, COMPARE>::end())
			{
				//uninitialised

				// std::cout << std::endl << "uninited";
				cache = map<KEY, VALUE, COMPARE>::lower_bound(key);
				return cache;
			}

			//iterate backward a few times till we're below
			bool found = false;
			for (int i = 0; i < 5; i++)
			{
				if	( cache == map<KEY, VALUE, COMPARE>::begin()
					||COMPARE{}(cache->first, key))
				{
					found = true;
					break;
				}

				// std::cout << std::endl << "iterating left from " << cache->first;

				cache--;
			}

			if (found == false)
			{
				//too slow, screw it, ignore the cache
				// std::cout << std::endl << "screw it 1";
				cache = map<KEY, VALUE, COMPARE>::lower_bound(key);
				return cache;
			}

			//iterate forward a few times until we're above or equal
			for (int i = 0; i < 5; i++)
			{
				if	( cache							== map<KEY, VALUE, COMPARE>::end()
					||COMPARE{}(cache->first,key)	== false)
				{
					return cache;
				}

				// std::cout << std::endl << "iterating right from " << cache->first;

				cache++;
			}
		}

		//too slow, screw it, ignore the cache
		// std::cout << std::endl << "screw it 2";
		cache = map<KEY, VALUE, COMPARE>::lower_bound(key);
		return cache;
	}

	size_t								erase(const KEY& key)							{	cache = map<KEY, VALUE, COMPARE>::end();	return	map<KEY, VALUE, COMPARE>::erase(key);	}
	map<KEY, VALUE, COMPARE>::iterator	erase(map<KEY, VALUE, COMPARE>::iterator& it)	{	cache = map<KEY, VALUE, COMPARE>::end();	return	map<KEY, VALUE, COMPARE>::erase(it);	}
	void								clear()											{	cache = map<KEY, VALUE, COMPARE>::end();			map<KEY, VALUE, COMPARE>::clear();		}
};