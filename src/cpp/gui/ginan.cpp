	
// #pragma GCC optimize ("O0")

#include <boost/algorithm/string.hpp>


#include "sciplot/sciplot.hpp"
#include "binaryStore.hpp"
#include "acsConfig.hpp"
#include "ginan.hpp"
#include "anode.hpp"
#include "api.hpp"

#include <fstream>
#include <chrono>
#include <thread>
#include <list>
#include <any>


using namespace std::literals::chrono_literals;

using std::this_thread::sleep_for;
using boost::algorithm::to_lower;
using std::ofstream;
using std::list;
using std::any;

int Beasty();

	map<string, vector<GeneralDataEntry>*>	anyPtrMap;
	
	


void callback0();
void callback1();
void callback2();


double click = 0;

int main(
	int		argc, 
	char**	argv)
{
	configure(argc, argv);
	Beasty();
	callback0();
// 	oncePerEpochCallbacks.push_back(callback0);
	oncePerEpochCallbacks.push_back(callback1);
	
	callback2();
	std::cout << "The Ginan server is running at http://localhost:8081" << std::endl;
	
	
	while (true)
	{
		while (click == false)
		{
			callback1();
			sleep_for(1000ms);
		}
		click = false;
		
		ginan(argc, argv);
	}
	
// 	/ Create a vector with values from 0 to 100 divived into 2000 uniform intervals for the z-axis
// 	Vec z = sciplot::linspace(0.0, 100.0, 2000);

	map<string, map<int, map<int, sciplot::Plot2D>>> pageFigurePlotMap;
	

	auto makePlot = [&pageFigurePlotMap](
		auto&	entries,
		auto	functor,
		string	page		= "",
		int		figureCol	= 0, 
		int		figureRow	= 0,
		string	id			= "",
		int		dash		= 1,
		bool	legend		= true) -> sciplot::Plot2D&
	{
		vector<GTime>	x;
		vector<double>	y;
		
		for (auto& entry : entries)
		{
			x.push_back(entry.time);
			y.push_back(functor(entry));
		}
		
		auto& plot = pageFigurePlotMap[page][figureRow][figureCol];

		plot.xrange("*","*");
		plot.yrange("*","*");
	
		// Clear all borders and set the visible ones
		plot.border()
			.clear()
			.bottomLeftFront()
			.bottomRightFront()
			.leftVertical();
		
		// Change its palette
		plot.palette("dark2");

		plot.timed = true;
// 		if (legend == false)
// 			plot.drawHistogram(y)
// 				.label(id);
// 		else
			plot.drawCurve(x, y)
				.label(id)
				.dashType(dash);//.lineColor("orange");
		
		
		plot.fontName("Palatino");
		plot.fontSize(12);
		
		plot.ytics()
			.insideGraph()
			.show(true);
// 			.logscale();
		
		plot.grid()
			.lineWidth(2)
			.show();
		
			
		if (legend)
			plot.legend()
				.atOutsideRight()
				.fontSize(12)
				.opaque()
				.enhanced(false)
	// 			.displayHorizontal()
				.displayExpandWidthBy(2);
		else
			plot.legend()
				.hide();
				
		return plot;
	};
	
	auto makeHist = [&pageFigurePlotMap](
		auto&	entries,
		auto	functor,
		string	page		= "", 
		int		figureCol	= 0,
		int		figureRow	= 0,
		string	id			= "",
		int		dash		= 1,
		bool	legend		= true) -> sciplot::Plot2D&
	{
		vector<double>	x;
		vector<double>	y;
	
		map<double, int>	boundsCount;
		
		for (double i = -dash;	i < dash;	i += dash/100.0)	
			boundsCount[i];
		
		
		for (auto& entry : entries)
		{
			double value = functor(entry);
			auto it = boundsCount.lower_bound(value);
			if (it == boundsCount.end())
			{
				continue;
			}
			
			auto& [val, bin] = *it;
			bin++;
		}
		
		for (auto& [bin, val] : boundsCount)
		{
			x.push_back(bin);
			y.push_back(val);
		}
		
		auto& plot = pageFigurePlotMap[page][figureRow][figureCol];

		plot.xrange(0.0,200.0);
		plot.yrange("*","*");
	
		// Clear all borders and set the visible ones
		plot.border()
			.clear()
			.bottomLeftFront()
			.bottomRightFront()
			.leftVertical();
		
		// Change its palette
		plot.palette("dark2");

		plot.drawHistogram(y)
			.lineWidth(10)
			.label(id);
		
		plot.fontName("Palatino");
		plot.fontSize(1);
		
// 		plot.ylabel()
// 			.show(false);
// 		plot.xlabel()
// 			.show(false);
		plot.ytics()
			.show(false);
		plot.xtics()
			.show(false);
			
// 		plot.ytics()
// 			.logscale();
		
// 		plot.grid()
// 			.lineWidth(2)
// 			.show();
		
			
		if (legend)
			plot.legend()
				.atOutsideRight()
				.fontSize(12)
				.opaque()
				.enhanced(false)
	// 			.displayHorizontal()
				.displayExpandWidthBy(2);
		else
			plot.legend()
				.hide();
				
		return plot;
	};
	
// 	if (0)
// 	for (auto& [suffix, sds]	: stateDataMap)
// 	for (auto& [kfKey,	sd]		: sds)
// 	{
// 		auto& sd0 = sd.entries.back();
// 		
// 		switch (kfKey.type)
// 		{
// 			case KF::REC_POS:	{															makePlot(sd.entries, [&](auto& e)->double{return e.x	- e.apriori;		}, "1 - Position Deltas",							0, kfKey.num, kfKey.str);			}	break;	
// 			case KF::REC_CLOCK:	{	double off = sd0.x		- sd0.apriori;					makePlot(sd.entries, [&](auto& e)->double{return e.x	- e.apriori - off;	}, "3 - Receiver Clock Deltas minus final bias",	0, kfKey.num, kfKey.str);			}			
// 								{	double off = sd0.rts	- sd0.apriori;	if (sd0.rts)	makePlot(sd.entries, [&](auto& e)->double{return e.rts	- e.apriori - off;	}, "3 - Receiver Clock Deltas minus final bias",	0, kfKey.num, kfKey.str + "*", 2);	}	break;	
// 			case KF::TROP:		{															makePlot(sd.entries, [&](auto& e)->double{return e.x;						}, "2 - Tropospheres",								0, kfKey.num, kfKey.str);			}			
// 								{											if (sd0.rts)	makePlot(sd.entries, [&](auto& e)->double{return e.rts;						}, "2 - Tropospheres",								0, kfKey.num, kfKey.str + "*", 2);	}	break;	
// 			default:
// 			{
// 				continue;
// 			}
// 		}
// 	}
// 	
// 	for (auto& [suffix, mds]	: measDataMap)
// 	for (auto& [kfKey,	md]		: mds)
// 	{
// 		switch (kfKey.type)
// 		{
// 			case KF::CODE_MEAS:	
// 								{	makeHist(md.entries, [](auto& e)->double{return e.prefit;	}, "5 - Prefit Residuals by Sat",		0, kfKey.num + kfKey.Sat*100, 						kfKey.str, 3, false);	}
// 								{	makeHist(md.entries, [](auto& e)->double{return e.prefit;	}, "5 - Prefit Residuals by Rec",		0, kfKey.num + std::hash<string>{}(kfKey.str),		kfKey.str, 3, false);	} 
// 								{	makeHist(md.entries, [](auto& e)->double{return e.postfit;	}, "5 - Postfit Residuals by Sat",		0, kfKey.num + kfKey.Sat*100, 						kfKey.str, 3, false);	}
// 								{	makeHist(md.entries, [](auto& e)->double{return e.postfit;	}, "5 - Postfit Residuals by Rec",		0, kfKey.num + std::hash<string>{}(kfKey.str),		kfKey.str, 3, false);	} 
// 								{	makePlot(md.entries, [](auto& e)->double{return e.prefit;	}, "5 - Code Residuals",				0, kfKey.num, 										kfKey.str, 1, false).yrange(-10.0,+10.0);	}
// 								{	makePlot(md.entries, [](auto& e)->double{return e.postfit;	}, "5 - Code Residuals",				0, kfKey.num + 100,									kfKey.str, 1, false).yrange(-10.0,+10.0);	}
// 									break;
// 								
// 			case KF::PHAS_MEAS:	{	makeHist(md.entries, [](auto& e)->double{return e.prefit;	}, "5 - Prefit Residuals by Sat",		1, kfKey.num + kfKey.Sat*100, 						kfKey.str, 1, false);	}
// 								{	makeHist(md.entries, [](auto& e)->double{return e.prefit;	}, "5 - Prefit Residuals by Rec",		1, kfKey.num + std::hash<string>{}(kfKey.str), 		kfKey.str, 1, false);	}
// 								{	makeHist(md.entries, [](auto& e)->double{return e.postfit;	}, "5 - Postfit Residuals by Sat",		1, kfKey.num + kfKey.Sat*100, 						kfKey.str, 1, false);	}
// 								{	makeHist(md.entries, [](auto& e)->double{return e.postfit;	}, "5 - Postfit Residuals by Rec",		1, kfKey.num + std::hash<string>{}(kfKey.str),	 	kfKey.str, 1, false);	} 
// 								{	makePlot(md.entries, [](auto& e)->double{return e.prefit;	}, "4 - Phase Residuals",				0, kfKey.num,										kfKey.str, 1, false).yrange(-1.0,+1.0);	}
// 								{	makePlot(md.entries, [](auto& e)->double{return e.postfit;	}, "4 - Phase Residuals",				0, kfKey.num + 100,									kfKey.str, 1, false);	}
// 									break;
// 			default:
// 			{
// 				continue;
// 			}
// 		}
// 	}
	
	return 1;
	
	
	vector<vector<sciplot::Figure>> figures;
	
	for (auto& [token, tokenplot] : pageFigurePlotMap)
	{
		vector<vector<sciplot::PlotVariant>> plotVector;
		for (auto& [rowid, plots] : tokenplot)
		{
			// Create figure to hold plot
			vector<sciplot::PlotVariant> vec;
			for (auto& [colid, plot] : plots)
			{
				double rows = tokenplot.size();
				double cols = plots.size();
				
				double row = plotVector.size();
				double col = vec.size();
				
				plot.gnuplot(string("set tmargin at screen ") + std::to_string((row + 0)		* 1/rows));
				plot.gnuplot(string("set bmargin at screen ") + std::to_string((row + 1)		* 1/rows));
				plot.gnuplot(string("set lmargin at screen ") + std::to_string((col + 0.022)	* 1/cols));
				plot.gnuplot(string("set rmargin at screen ") + std::to_string((col + 1)		* 1/cols));
				
				vec.push_back(plot);
				
			}
			plotVector.push_back(vec);
		}
		sciplot::Figure fig = plotVector;
		fig.title(token);
// 			.titleShiftAlongY(10);
		
// 		fig.layout().marginsRelative(10,10,10,10);
		figures.push_back({fig});
	}
	
	{
		// Create canvas to hold figure
		sciplot::Canvas canvas = figures;

    	canvas.size(1000, 500);
	
		// Show the plot in a pop-up window
// 		canvas.show();

		// This disables the deletion of the created gnuplot script and data file.
		canvas.autoclean(false);

		// Save the plot to a PDF file
		canvas.save(acsConfig.config_description + "_report.pdf");
	}
	
	string command = R"(curl --trace-ascii -F "payload_json='{'username': 'Clark Kent', 'content': 'Test two'}'" -F 'file1=@ex11_report.pdf' "https://discord.com/api/webhooks/1044032882672422942/s7MFwrbikBEIdjAGdKeX3cjBBtIgKhMqkdixnfHuiTo_5sJyy4Rx0-Ycv9hcccMfeYYR")";
	
	return std::system(command.c_str()) == 0;
}
