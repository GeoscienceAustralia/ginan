
// #pragma GCC optimize ("O0")

#include "interactiveTerminal.hpp"

#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string.hpp>
#include <thread>
#include <unistd.h>
#include <iostream>
#include <cstdlib>
#include <ncurses.h>

WINDOW* window;
WINDOW* menu;
WINDOW* bar;

int activeLevel;
vector<string> activeSplit;


mutex										InteractiveTerminal::dataMutex;
mutex										InteractiveTerminal::displayMutex;
string										InteractiveTerminal::activePage;
string										InteractiveTerminal::epoch;
string										InteractiveTerminal::duration;
E_InteractMode								InteractiveTerminal::interactMode	= E_InteractMode::Page;
E_InteractiveMode							InteractiveTerminal::activeMode		= E_InteractiveMode::Syncing;
bool										InteractiveTerminal::enabled		= false;
map<string,				InteractivePage>	InteractiveTerminal::pages;
map<E_InteractiveMode,	InteractiveMode>	InteractiveTerminal::modes;

InteractiveTerminalDestructor interactiveTerminaldestructor;

InteractiveTerminalDestructor::~InteractiveTerminalDestructor()
{
	if (InteractiveTerminal::enabled)
	{
		werase(window);
		werase(bar);
		werase(menu);
		erase();
		endwin();

		for (auto& line : InteractiveTerminal::pages["Messages/All"].lines)
		{
			std::cout << line << "\n";
		}
	}
}

void InteractiveTerminal::keyboardHandler()
{
	while (1)
	{
		int ch = getch();

		if (activePage.empty())
		{
			continue;
		}

		lock_guard<mutex> guard(dataMutex);

		auto& page = pages[activePage];

		if		(ch == '1')	interactMode = E_InteractMode::Page;
		else if	(ch == '2')	interactMode = E_InteractMode::Scroll;
		else if (interactMode == +E_InteractMode::Page)
		{
			switch (ch)
			{
				case KEY_UP:		{	activeLevel--;	break;		}
				case KEY_DOWN:		{	activeLevel++;	break;		}
				case KEY_RIGHT:
				case KEY_LEFT:
				{
					auto it = pages.find(activePage);

					while (1)
					{
						//keep trying inc/decrement pages until the base prefix at the correct level changes

						if (ch == KEY_LEFT)
						{
							if (it == pages.begin())
								break;

							it--;
						}
						if (ch == KEY_RIGHT)
						{
							it++;

							if (it == pages.end())
							{
								it--;
								break;
							}
						}

						auto& [testPage, page] = *it;

						auto nthIt = boost::find_nth(activePage, "/", activeLevel);

						int baseChars = std::distance(activePage.begin(), nthIt.begin());

						bool changed = (activePage.substr(0, baseChars) != testPage.substr(0, baseChars));

						if (changed)
						{
							break;
						}
					}

					//found something with a different base, check if the base's base is common or not to determine if we can get to it from here
					auto& [testPage, page] = *it;

					bool changed = false;

					if (activeLevel > 0)
					{
						auto nthIt = boost::find_nth(activePage, "/", activeLevel-1);

						int baseChars = std::distance(activePage.begin(), nthIt.begin());

						changed = (activePage.substr(0, baseChars) != testPage.substr(0, baseChars));
					}

					if (changed == false)
					{
						activePage	= testPage;

						boost::algorithm::split(activeSplit, activePage, boost::is_any_of("\t/"), boost::token_compress_on);
					}

					break;
				}
			}
		}
		else if (interactMode == +E_InteractMode::Scroll)
		{
			switch (ch)
			{
				case KEY_UP:		{	page.currentLine -= 3;						page.followEnd = false;	break;		}
				case KEY_DOWN:		{	page.currentLine += 3;												break;		}
				case KEY_PPAGE:		{	page.currentLine -= 20;						page.followEnd = false;	break;		}
				case KEY_NPAGE:		{	page.currentLine += 20;												break;		}
				case KEY_HOME:		{	page.currentLine =  0;						page.followEnd = false;	break;		}
				case KEY_END:		{	page.currentLine =  page.lines.size() + 1;							break;		}
				default:
				{
					break;
				}
			}
		}

		if (activeLevel < 0)							{	activeLevel			= 0;							}
		if (activeLevel >= activeSplit.size())			{	activeLevel			= activeSplit	.size() - 1;	}
		if (page.currentLine < 0)						{	page.currentLine	= 0;													}
		if (page.currentLine > page.lines.size())		{	page.currentLine	= page.lines	.size() - 1;	page.followEnd = true;	}

		drawWindow();
		drawMenus();
	}
}

void InteractiveTerminal::enable()
{
	ConsoleLog::useInteractive = true;

	for (E_InteractiveMode mode : E_InteractiveMode::_values())
	{
		modes[mode].modeName = mode._to_string();
	}

	initscr();

	if (has_colors())
		start_color();

	init_color(COLOR_YELLOW, 1000, 165, 0);

    init_pair(1, COLOR_WHITE,	COLOR_RED);
    init_pair(2, COLOR_YELLOW,	COLOR_BLACK);

	erase();
	refresh();
    cbreak();
	noecho();
	keypad(stdscr, true);

	menu	= newwin(6,				COLS, 0,			0);
	window	= newwin(LINES - 5 - 6,	COLS, 6,			0);
	bar		= newwin(5,				COLS, LINES - 5,	0);

	std::thread(keyboardHandler).detach();

	wrefresh(window);
	wrefresh(menu);
	wrefresh(bar);
	wclear	(window);
	wclear	(menu);
	wclear	(bar);

	enabled = true;

	drawWindow();
	drawMenus();
}


void InteractiveTerminal::drawMenus()
{
	lock_guard<mutex> guard(displayMutex);

	boost::algorithm::split(activeSplit, activePage, boost::is_any_of("/"), boost::token_compress_on);

	werase	(menu);
	box		(menu, 0, 0);

	if (activePage.empty() == false)
	for (int level = 0; level < activeSplit.size(); level++)
	{
		wmove(menu, level, 1);

		//get activeSplit

		//level 0 - print the 0 split of everything but only once (ignore repeated prefixes)

		//level 1 - print the 1 split of everything that matches active.split0, but only once

		//level 2 - print the 2 split of everything that matches active.split1 and active split2 but only once
		string lastPrinted;

		for (auto& [pageName, page] : pages)
		{
			vector<string> testSplit;

			boost::algorithm::split(testSplit, pageName, boost::is_any_of("/"), boost::token_compress_on);

			if (level >= testSplit.size())
			{
				//too deep already, couldnt possibly be anything to print
				continue;
			}

			bool isVisible = true;

			//check all levels
			for (int i = 0; i < level; i++)
			{
				if (activeSplit[i] != testSplit[i])
				{
					//no longer visible at this level
					isVisible = false;
					break;
				}
			}

			if (isVisible == false)
			{
				//difference in lower prefixes
				continue;
			}

			string subPage = testSplit[level];

			if (subPage == lastPrinted)
			{
				//just printed this for the last page which had same prefixes
				continue;
			}

			if (subPage == activeSplit[level])
			{
				//this is (half) selected
				wattron	(menu, A_BOLD);
				wattron	(menu, A_UNDERLINE);

				if (level == activeLevel)
					wattron	(menu, A_STANDOUT);
			}

			string label = subPage;
			boost::trim(label);

			wprintw(menu, " %s ", label.c_str());

			wattroff(menu, A_STANDOUT);
			wattroff(menu, A_UNDERLINE);
			wattroff(menu, A_BOLD);

			lastPrinted = subPage;
		}
	}

	werase	(bar);
	box		(bar, 0, 0);

	wmove	(bar, 0, 1);
	wprintw	(bar, " Status ");

	wmove	(bar, 0, COLS - 30);

	for (int i = 1; i < E_InteractMode::_size(); i++)
	{
		auto mode = E_InteractMode::_values()[i];
		if (interactMode == +mode)	{	wattron	(bar, A_STANDOUT);	 wattron	(bar, A_BOLD);	}
		else						{	wattroff(bar, A_STANDOUT);	 wattroff	(bar, A_BOLD);	}

		wprintw(bar, " (%d) %s ", i, mode._to_string());

									{	wattroff(bar, A_STANDOUT);	 wattroff	(bar, A_BOLD);	}
	}

	wmove	(bar, 1, 1);
	wprintw	(bar, "%35s    ",	epoch.c_str());
	wprintw	(bar, "%s",			duration.c_str());

	wmove	(bar, 2,1);
	for (auto& [modeName, mode] : modes)
	{
		if (mode.active == 1)	wattron	(bar, A_STANDOUT);
		if (mode.active >= 1)	wattron	(bar, A_BOLD);

		int col1 = getcurx(bar);
		wprintw(bar, " %3s ", mode.modeName.c_str());
		int col2 = getcurx(bar);

								wattroff(bar, A_BOLD);
								wattroff(bar, A_STANDOUT);

		wmove(bar, 3, col1);
		wprintw(bar, " %0.2fs", mode.duration);
		wmove(bar, 2, col2);
	}

	wrefresh(menu);
	wrefresh(bar);
}

void InteractiveTerminal::drawWindow()
{
	lock_guard<mutex> guard(displayMutex);
	wclear(window);

	int offset = LINES - 7 - 6;

	if (activePage.empty() == false)
	{
		auto& page = pages[activePage];

		for (int i = page.currentLine; i > 0 && offset > 1; i--, offset--)
		{
			if (i >= page.lines.size())
			{
				continue;
			}

			string line = page.lines[i];

			for (int i = 0; i < line.length(); i++)
			{
				if (line[i] == '\t')
				{
					int spaces = 4 - i % 4;
					line.replace(i, 1, string(spaces, ' '));
				}
			}

			line = line.substr(0, COLS - 2);

			if (has_colors())
			{
				if (line.find("Warning")	!= string::npos)	{	wattron(window, COLOR_PAIR(2));		wattron	(window, A_BOLD);	}
				if (line.find("Error")		!= string::npos)	{	wattron(window, COLOR_PAIR(1));		wattron	(window, A_BOLD);	}
			}

			mvwprintw(window, offset, 1, "%s", line.c_str());

			if (has_colors())
			{
																	wattroff(window, COLOR_PAIR(2));	wattroff(window, A_BOLD);
																	wattroff(window, COLOR_PAIR(1));	wattroff(window, A_BOLD);
			}
		}
	}


	box(window, 0, 0);

	wrefresh(window);
}

void InteractiveTerminal::addString(
			string	pageName,
	const	string&	str,
			bool	updateWindow)
{
	if (enabled == false)
		return;

	boost::replace_all(pageName, "\t", "/");

	vector<string> split;

	boost::algorithm::split(split, str, boost::is_any_of("\n"), boost::token_compress_on);

	if (split.size() > 1)
	{
		for (auto& line : split)
		{
			addString(pageName, line, false);
		}

		if (activePage == pageName)
		{
			drawWindow();
		}
		return;
	}

	lock_guard<mutex> guard(dataMutex);

	auto& page = pages[pageName];

	if (activePage.empty())
	{
		activePage = pageName;
	}

	if (page.followEnd)
	{
		page.currentLine = page.lines.size();
	}

	page.lines.push_back(str);

	if	( updateWindow
		&&activePage == pageName)
	{
		drawWindow();
	}
}


void InteractiveTerminal::setMode(
	E_InteractiveMode		modeName)
{
	if (enabled == false)
		return;

	if (activeMode == modeName)
		return;

	modes[activeMode].stopTime	= timeGet();

	if (modes[activeMode].startTime != GTime::noTime())
	{
		modes[activeMode].duration = (modes[activeMode].stopTime - modes[activeMode].startTime).to_double();
	}

	activeMode = modeName;
	modes[activeMode].startTime	= timeGet();

	for (auto& [testMode, mode] : modes)
	{
		if	( testMode < modeName
			&&mode.active)
		{
			mode.active++;
		}

		if (testMode == modeName)
		{
			mode.active = 1;
		}
	}

	drawMenus();
}
