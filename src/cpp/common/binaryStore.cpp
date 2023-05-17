

// #pragma GCC optimize ("O0")

#include "binaryStore.hpp"
#include "coordinates.hpp"
#include "acsConfig.hpp"
#include "algebra.hpp"


map<string, map<KFKey, map<E_Component, vector<GeneralDataEntry>>>> generalDataMap;
// map<string, map<KFKey, AlgebraData<GeneralDataEntry>>>	generalDataMap;
// map<string, map<KFKey, AlgebraData<MeasDataEntry>>>		measDataMap;
// map<string, map<KFKey, AlgebraData<StateDataEntry>>>	stateDataMap;



// map<string, Plotter> plotMap;

void storeResiduals(
	GTime				time,
	vector<KFKey>&		obsKeys,
	VectorXd&			prefits,
	VectorXd&			postfits,
	MatrixXd&			variance,
	string				suffix,
	int					beg,
	int					num)
{
// 	for (int i = beg; i < beg + num; i++)
// 	{
// 		auto& obsKey = obsKeys[i];
// 		
// 		auto& measData = measDataMap[suffix][obsKey];			//todo aaron, gonna need mutexes probably
// 
// 		
// 		auto it = measData.timeIndexMap.find(time);
// 		if (it == measData.timeIndexMap.end())
// 		{
// 			measData.timeIndexMap[time] = measData.entries.size();
// 			measData.entries.push_back({});
// 		}
// 		
// 		auto& measDataEntry = measData.entries[measData.timeIndexMap[time]];
// 		measDataEntry.time		= time;
// 		measDataEntry.prefit	= prefits	[i];
// 		measDataEntry.postfit	= postfits	[i];
// 		measDataEntry.variance	= variance	(i,i);
// 	}
	
	for (int i = beg; i < beg + num; i++)
	{
		auto& obsKey = obsKeys[i];
		
		generalDataMap[suffix][obsKey][E_Component::PREFIT]		.push_back({time, prefits	(i)});
		generalDataMap[suffix][obsKey][E_Component::POSTFIT]	.push_back({time, postfits	(i)});
		generalDataMap[suffix][obsKey][E_Component::VARIANCE]	.push_back({time, variance	(i,i)});	
		
		
				//todo aaron, gonna need mutexes probably
	}
}

void storeResiduals(
	GTime		time,
	VectorXd&	rtsPostfits,
	string		suffix)
{
	
}

void storeStates(
	KFState&	kfState,
	string		suffix)
{
	if (acsConfig.store_binary_states == false)
	{
		return;
	}
	
	auto time = kfState.time;

	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{		
		if (kfKey.type == KF::ONE)
		{
			continue;
		}
		generalDataMap[suffix][kfKey][E_Component::X]	.push_back({time, kfState.x	(index)			});
		generalDataMap[suffix][kfKey][E_Component::P]	.push_back({time, kfState.P	(index,index)	});
		generalDataMap[suffix][kfKey][E_Component::DX]	.push_back({time, kfState.dx(index)			});	
		
	
		
				//todo aaron, gonna need mutexes probably
	}
	
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{		
		if	( kfKey.type	!= KF::REC_POS
			||kfKey.num 	!= 0)
		{
			continue;
		}
		
		Vector3d recPos;
		
		for (int i = 0; i < 3; i++)
		{
			auto key = kfKey;
			key.num = i;
			
			kfState.getKFValue(key, recPos(i));
		}
		
		VectorPos pos = ecef2pos(recPos);
		
		for (int i = 0; i < 3; i++)
		{
			auto key = kfKey;
			key.num = i;
			generalDataMap[suffix][key][E_Component::LLH].push_back({time, pos[i] * R2D});	
		}
	}
	/*
	
	for (auto& [kfKey, index] : kfState.kfIndexMap)
	{		
		
		auto& stateData = stateDataMap[""][kfKey];			//todo aaron, gonna need mutexes probably

		auto it = stateData.timeIndexMap.find(time);
		if (it == stateData.timeIndexMap.end())
		{
			stateData.timeIndexMap[time] = stateData.entries.size();
			stateData.entries.push_back({});
		}
		
		auto& stateDataEntry = stateData.entries[stateData.timeIndexMap[time]];
		if		(suffix == " apriori")
		{
			stateDataEntry.apriori	= kfState.x [index];
			stateDataEntry.time		= kfState.time;
		}
		else if	(suffix == " rts")
		{
			stateDataEntry.rts		= kfState.x [index];
			stateDataEntry.time		= kfState.time;
		}
		else
		{
			stateDataEntry.time		= kfState.time;
			stateDataEntry.x		= kfState.x	[index];
			stateDataEntry.P		= kfState.P	(index,index);
			stateDataEntry.dx		= kfState.dx(index);
		}
	}
*/
}

// 		PlotData plotData;
// 		plotData.time = kfState.time;
// 		plotData.prefit = kfState.x[index];
// 		
// 		plotMap[str + suffix].data.push_back(plotData);
		
