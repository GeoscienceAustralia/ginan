
#pragma once

#include <sstream>
#include <string>
#include <vector>
#include <mutex>
#include <map>

#include "gTime.hpp"
#include "trace.hpp"
#include "enums.h"

using std::ostringstream;
using std::lock_guard;
using std::vector;
using std::string;
using std::mutex;
using std::map;

struct InteractivePage
{
	vector<string>	lines;
	int				currentLine = 0;
	bool			followEnd	= true;
};

struct InteractiveMode
{
	GTime	startTime;
	GTime	stopTime;
	double	duration	= 0;
	int		active		= 0;
	string	modeName;
};

struct InteractiveTerminal : ostringstream
{
	static mutex										dataMutex;
	static mutex										displayMutex;
	static string										activePage;
	static string										epoch;
	static string										duration;
	static E_InteractMode								interactMode;
	static E_InteractiveMode							activeMode;
	static bool											enabled;
	static map<string,				InteractivePage>	pages;
	static map<E_InteractiveMode,	InteractiveMode>	modes;


	static void enable();
	static void drawMenus();
	static void drawWindow();
	static void keyboardHandler();

	static void clearPage(
		string pageName)
	{
		if (enabled == false)
			return;

		boost::replace_all(pageName, "\t", "/");

		lock_guard<mutex> guard(dataMutex);

		pages[pageName].lines.clear();
	}

	static void clearModes(
		string epochStr		= "",
		string durationStr	= "")
	{
		setMode(E_InteractiveMode::Syncing);

		if (enabled == false)
			return;

		epoch		= epochStr;
		duration	= durationStr;

		for (auto& [modeName, mode] : modes)
		{
			mode.active = 0;
		}
	}

	static void setMode(
		E_InteractiveMode		modeName);

	static void addString(
				string	pageName,
		const	string&	str,
				bool	updateWindow = true);


	string	name;
	Trace&	trace;
	bool	doClear;

	/// Creates a RAII object that acts like an ostringstream and spits results out to another trace when it goes out of scope
	/// Also clears and updates an interactive terminal page
	InteractiveTerminal(
		const	string&	name,
				Trace&	trace,
				bool	doClear = true)
	:	name	{name},
		trace	{trace},
		doClear	{doClear}
	{

	}

	~InteractiveTerminal()
	{
		trace << str();

		if (doClear)
		{
			clearPage(name);
		}

		addString(name, str());
	}
};

struct InteractiveTerminalDestructor
{
	~InteractiveTerminalDestructor();
};

extern InteractiveTerminalDestructor interactiveTerminaldestructor;
