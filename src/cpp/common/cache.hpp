
#pragma once

#include <functional>

template<typename TYPE>
struct Cache : TYPE
{
	std::function<TYPE()> lambda;
	
	TYPE output;
	
	bool initialised = false;
	
	Cache(){}
	
	Cache<TYPE>& uninit()
	{
		initialised = false;
		
		return *this;
	}
	
	const TYPE& useCache(
		std::function<TYPE()> lambda)
	{
		if (initialised)
			return output;
		
		this->lambda = lambda;
		
		output = lambda();
		
		initialised = true;
		
		return output;
	}
};
