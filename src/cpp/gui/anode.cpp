
// #pragma GCC optimize ("O0")


#include <boost/log/sinks/basic_sink_backend.hpp>

#include <boost/log/sinks/sync_frontend.hpp>

#include <boost/algorithm/string.hpp>
#include "binaryStore.hpp"
#include "constants.hpp"
#include "anode.hpp"

#include <condition_variable>
#include <iostream>
#include <fstream>
#include <sstream>

using std::shared_ptr;
using std::ofstream;

extern 	map<string, vector<GeneralDataEntry>*>	anyPtrMap;
map<string, bool> subscribedMap;
// map<string, bool> requestedMap;
list<tuple<string,string>> stringList;



int ANode::nextGuid = 0;
map<string, shared_ptr<ANode>>	ANode::anodeIdMap;

map<string, string>	stringMap;

	ostream& operator<<(
		ostream&	out,
		ANode&		anode)
	{
		return anode.shift(out);
	}

	ANode& ANode::operator << (string content)
	{
		if (childless)
		{
			this->content += content;
		}
		else
		{
			addChild<Content>() << content;
		}
		return *this;
	}

list<string> outQueue;
list<string> msgQueue;
mutex outMutex;

mutex subscribedMapMutex;

bool sendFlag = true;

extern double click;
namespace sinks = boost::log::sinks;



struct GooeySinkBackend : public sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>
{
	// The function consumes the log records that come from the frontend
	void consume(
		boost::log::record_view																	const&	rec,
		sinks::basic_formatted_sink_backend<char, sinks::synchronized_feeding>::string_type		const&	log_string)
	{
		int sev;
		if (auto severity = rec[boost::log::trivial::severity])
		{
			// Convert the enum to unsigned int
			sev = static_cast< unsigned int >(*severity);
		}
		else
		{
			// Either "Severity" attribute not found, or it has value type
			// other than boost::log::trivial::severity_level. Use a default level.
			sev = 0;
		}

		bsoncxx::builder::basic::document doc = {};
		doc.append(kvp("send",				"CONS"				));
		doc.append(kvp("level",				sev					));
		doc.append(kvp("content",			log_string.c_str()	));
		{
			lock_guard<mutex> guard(outMutex);

			msgQueue.push_back(bsoncxx::to_json(doc));
		}
	}
};

void callback0()
{
	static bool once = true;

	if (once == false)
	{
		return;
	}

	// Construct the sink
	using GooeyLogSink = sinks::synchronous_sink<GooeySinkBackend>;

	boost::shared_ptr<GooeyLogSink> gooeyLogSink = boost::make_shared<GooeyLogSink>();

	// Register the sink in the logging core
	boost::log::core::get()->add_sink(gooeyLogSink);

	once = false;
}

string nonNumericStack(
	const string&	stack,
	string&			cut,
	bool			colon = true);

#include "acsConfig.hpp"

void callback2()
{
	auto config = make_shared<Div>("configTab_");
	ANode::anodeIdMap[config->id] = config;
// 	config->parent = "body";
// 	config->Class("pageTab");
	config->addChild<H1>("configHeader") << "Configuration";

	auto it = acsConfig.yamlDefaults.begin();
	while (it != acsConfig.yamlDefaults.end())
	{
		auto& [stack, defaultVals] = *it;
		it++;

		auto& gotValue	= defaultVals.foundValue;
		auto& comment	= defaultVals.comment;

		//split the name into tokens so that ordering numerals can be removed
		size_t pos_start = 0;
		size_t pos_end;

		string token;
		string flatStack = "";
		//find each part of the stack for this entry and make a list of them
		while ((pos_end = stack.find(":", pos_start)) != string::npos)
		{
			token = stack.substr(pos_start, pos_end - pos_start);
			pos_start = pos_end + 1;
			string dummy;
			token = nonNumericStack(token, dummy);
			flatStack += token;
		}

// 		if (token == "outputs:")
// 		{
// 			break;
// 		}
		string root = flatStack.substr(0, flatStack.size() - token.size());

		string parentString = "configTab_";
		if (root.empty() == false)
		{
			parentString = root + "_";
		}

		auto& parent = ANode::anodeIdMap[parentString];
		auto& element	= parent->addChild<Div>()
							.Class("element");


		auto& modified = element.addChild<Input>(flatStack)
			.attr("type", "checkbox");

		if (defaultVals.found)
		{
			modified.attr("checked", "true");
		}

		auto& ident = element.addChild<Div>()
			.Class("ident");

		bool nextIsChild = false;
		if (it != acsConfig.yamlDefaults.end())
		{
			auto& [nextStack, dummy] = *it;
			if (nextStack.substr(0, stack.size()) == stack)
			{
				nextIsChild = true;
			}
		}
		if (nextIsChild)	ident.Class("bold");

		ident << token;

		if (nextIsChild)	ident << " ⯆";


		if (comment.empty() == false)
		{
			ident.addChild<Span>()
				.Class("tooltiptext") << "# " + comment;
		}

		if (nextIsChild)
			element.addChild<Div>(flatStack + "_")
				.Class("contents");

// 		if (firstChild)
// 		{
// 			//finalise the child section
// 			html << std::endl << --	htmlIndentor		<< "</div>";
// 		}
// 		else
		{
			//this has no children, output the default value of this parameter instead - according to its commented parameter type

			for (auto once : {1})
			{
				//booleans
				if (comment.find("(bool)") != string::npos)
				{
					auto& boolSelect = element.addChild<Select>()
									.attr("class", "value");

					{auto& a = boolSelect.addChild<Option>().attr("value", "true")	<< "true";		if (gotValue == "1")	a.attr("selected",	"true");}
					{auto& a = boolSelect.addChild<Option>().attr("value", "false")	<< "false";		if (gotValue == "0")	a.attr("selected",	"true");}
					{auto& a = boolSelect.addChild<Option>().attr("value", "1")		<< "true";								a.attr("hidden",	"true");}
					{auto& a = boolSelect.addChild<Option>().attr("value", "0")		<< "false";								a.attr("hidden",	"true");}

					break;
				}

				auto begin	= comment.find('{');
				auto end	= comment.find('}', begin);

				//enums
				if	( begin != string::npos
					&&end	!= string::npos)
				{
					string enums = comment.substr(begin + 1, end - begin - 1);
					size_t pos_start = 0;
					size_t pos_end;

					auto& boolSelect = element.addChild<Select>()
									.attr("class", "value");


					//find each part of the stack for this entry and make a list of them
					while ((pos_end = enums.find(',', pos_start)) != string::npos)
					{
						string token = enums.substr(pos_start, pos_end - pos_start);
						pos_start = pos_end + 1;

						auto& a = boolSelect.addChild<Option>().attr("value", token)	<< token;
						if (boost::algorithm::to_lower_copy(gotValue) == token)	a.attr("selected", "true");
					}
					//get last one
					string token = enums.substr(pos_start);

					auto& a = boolSelect.addChild<Option>().attr("value", token)	<< token;
					if (boost::algorithm::to_lower_copy(gotValue) == token)	a.attr("selected", "true");

					break;
				}

				//general parameters
				{
					element.addChild<Input>()
						.attr("type", "text")
						.attr("class", "value")
						.attr("value", gotValue);
				}
			}
		}
	}

	{
		ofstream out("config.html");

// 		std::cout << *script << std::endl;
		out << *config;
	}

// 	config->mapify(stringMap);
// 	config->stringify(stringList);
}



void callback1()
{
	if (outQueue.empty() == false)
	{
		return;
	}
	stringList.clear();	//todo aaron, wait for done before doing this function

// 	std::cout << "callback"<<std::endl;
	if (sendFlag == false)
	{
		return;
	}
	sendFlag = false;

	{
		//prevent sending same data over and over if the last set hasnt been sent
		lock_guard<mutex> guard(outMutex);
		outQueue.clear();
	}

	lock_guard<mutex> lock(subscribedMapMutex);

	for (auto& [subscription, val] : subscribedMap)
	{
		if (val == false)
		{
			continue;
		}

		bsoncxx::builder::basic::document doc = {};
		doc.append(kvp("send",				"DATA"			));
		doc.append(kvp("name",				subscription	));
		doc.append(kvp("type",				"scatter"		));
		string macsAreDumb = subscription;
		doc.append(kvp("x", [&](sub_array subArr)
		{
			for (auto& [time, val] : *(anyPtrMap[macsAreDumb]))	subArr.append(time.to_string());
		}));
		doc.append(kvp("y", [&](sub_array subArr)
		{
			for (auto& [time, val] : *(anyPtrMap[macsAreDumb]))	subArr.append(val);
		}));

		{
			lock_guard<mutex> guard(outMutex);

			outQueue.push_back(bsoncxx::to_json(doc));
		}
	}

	for (auto& [thing, thang] : generalDataMap)
	for (auto& [thong, theng] : thang)
	for (auto& [thung, thyng] : theng)
	{
		if	( thung		!= +E_Component::LLH
			||thong.num	!= 0)
		{
			continue;
		}



		string id = "," + thing + "," + thong.commaString() + "," + thung._to_string() + ",";

		to_lower(id);

		bsoncxx::builder::basic::document doc = {};
		doc.append(kvp("send",				"POS"		));
		doc.append(kvp("name",				thong.str + thing	));


		auto a = thing;
		auto b = thong;
		auto c = thung;

		doc.append(kvp("coords", [&](sub_array subArr)
		{
			for (int i = 1; i >= 0; i--)
			{
				auto kfKey = b;
				kfKey.num = i;
				double val = generalDataMap[a][kfKey][c].back().data;
				subArr.append(val);
			}
		}));

		{
			lock_guard<mutex> guard(outMutex);

			outQueue.push_back(bsoncxx::to_json(doc));
		}
	}




// 	outStream
// 		boost::beast::ostream(sendBuffer) << "{ \"name\": \"" << subscription << "\", \"x\": [" << ssx.str() << "], \"y\": [" << ssy.str() << "], \"type\": \"scatter\" }";
// 		std::cout << "sending " << sendBuffer.size() << " bytes" << std::endl;
//
// 		ws_.async_write(sendBuffer.data(), beast::bind_front_handler(&websocketsession::on_write, shared_from_this()));


	auto html = make_shared<Html>("html");
	auto& bb = html->addChild<Body>("body");
	auto& header = bb.addChild<Div>("headerDiv");
	{
		header.addChild<Img>()
			.attr("src", "GinanLogo60.png");

		header.addChild<H3>() << "Ginan";


		header.addChild<A>().attr("href", "javascript:openTab('wizardTab'	);") << "Wizard";
		header.addChild<A>().attr("href", "javascript:openTab('consoleTab'	);") << "Console";
		header.addChild<A>().attr("href", "javascript:openTab('configTab'	);") << "Config";
		header.addChild<A>().attr("href", "javascript:openTab('mapTab'		);") << "Map";
		header.addChild<A>().attr("href", "javascript:openTab('plotsTab'	);") << "Plots";

	}

	auto& wizardTab		= bb.addChild<Div>("wizardTab")	.Class("pageTab");
	auto& consoleTab	= bb.addChild<Div>("consoleTab").Class("pageTab");
	auto& configTab		= bb.addChild<Div>("configTab")	.Class("pageTab");
	auto& mapTab		= bb.addChild<Div>("mapTab")	.Class("pageTab");
	auto& plotsTab		= bb.addChild<Div>("plotsTab")	.Class("pageTab");

	{
		wizardTab.addChild<H1>() << "Wizard";
		auto& button = wizardTab.addChild<Label>() << "Go";
		button.addChild<Input>()
			.attr("type", "checkbox")
			.attach(click);
	}

	{
		auto& consoleLinks = consoleTab.addChild<Div>();
		consoleLinks.addChild<A>().attr("href", "javascript:toggleConsoleSev(4);").style("background-color:pink;		border: var(--sev4border);"	) << "Errors";
		consoleLinks.addChild<A>().attr("href", "javascript:toggleConsoleSev(3);").style("background-color:gold;		border: var(--sev3border);"	) << "Warnings";
		consoleLinks.addChild<A>().attr("href", "javascript:toggleConsoleSev(2);").style("background-color:whitesmoke;	border: var(--sev2border);"	) << "Info";
		consoleLinks.addChild<A>().attr("href", "javascript:toggleConsoleSev(1);").style("background-color:lightblue;	border: var(--sev1border);"	) << "Debug";

		consoleTab.addChild<Div>("Console");
	}

	{
	}



	{
		auto& map = mapTab.addChild<Div>("map");

// 	<div class="map-overlay top">
// 		<div class="map-overlay-inner">
// 			<input id="slider" type="range" min="0" max="1000" step="1" value="1000">
// 			<input id="scaler" type="range" min="0" max="20" step="0.001" value="20">
// 		</div>
// 		<div class="map-overlay-inner">
// 			<div id="legend" class="legend">
// 			<div class="bar"></div>
// 			<div>Magnitude (m)</div>
// 			</div>
// 		</div>
// 	</div>

	}


	{

		auto& scroller = plotsTab.addChild<Div>("scroller")
			.style("overflow: auto;")
			.style("max-height: 100vh;");

		plotsTab.addChild<Div>	("plot")
			.attr("ondrop", "drop(event)")
			.attr("ondragover", "allowDrop(event)");


		{
			scroller.addChild<Input>("filter").attr("type", "text");
			scroller.addChild<Div>	("filters");

			auto& bothPlots = scroller.addChild<Div>("bothPlots");

			auto& availableDiv = bothPlots.addChild<Div>();
			auto& removableDiv = bothPlots.addChild<Div>();

			removableDiv.addChild<Div> ("removeAll")
				.attr("onclick", "removeAll()") << "Remove all";

			removableDiv.addChild<Ul>	("plotted");


			availableDiv.addChild<Div> ("addAll")
				.attr("onclick", "addAll()") << "Add all";

			auto& list = availableDiv.addChild<Ul>("plots");
			for (auto& [thing, thang] : generalDataMap)
			for (auto& [thong, theng] : thang)
			for (auto& [thung, thyng] : theng)
			{
				string id = "," + thing + "," + thong.commaString() + "," + thung._to_string() + ",";

				to_lower(id);

				list.addChild<Li>(id)
					.attr("draggable", "true")
					.attr("ondragstart", "drag(event)")
					<< id;

				anyPtrMap[id] = &thyng;
			}

			scroller.addChild<Script>()
				.onAppend("") << R"(

	// 			console.log("test");
				document.getElementById("filter").addEventListener("input", function()
				{
					var filterString = document.getElementById("filter").value;

					const tokens = filterString.split(" ");
	// 				console.log(filterString);


					for (child of document.getElementById("plots").children)
					{
						child.style.display = "inherit";
					}

					for (token of tokens)
					for (child of document.getElementById("plots").children)
					{
						if (token == "")
							continue;

						if (token[0] == "-")
						{
							if (child.innerHTML.match(token.substring(1)))		child.style.display = "none";
						}
						else
						{
							if (child.innerHTML.match(token))	;
							else												child.style.display = "none";
						}
					}
					getFilters();
				});
			)";

		}
	}

	auto style = make_shared<Content>();

	*style << R"(
		:root
		{
			--sev4vis: block;
			--sev3vis: block;
			--sev2vis: block;
			--sev1vis: block;
			--sev4border: 2px solid;
			--sev3border: 2px solid;
			--sev2border: 2px solid;
			--sev1border: 2px solid;
		}
		#filters span
		{
			background-color: #ffcb89;
			margin: 5px;
			line-height: 1.5;
			padding: 2px;
		}

		#body
		{
			display: flex;
			flex-direction: column;
			margin:0;
			font-family: "Roboto", sans-serif;
			height:100vh;
			overflow:hidden;
		}

		#content
		{
			display: grid;
// 			grid-template-columns: 1fr 3fr;
		}

		.sev4
		{
			display: var(--sev4vis);
			background-color:#f00;
			color:white;
		}
		.sev3
		{
			display: var(--sev3vis);
			background-color:#ffe9b5;
		}
		.sev2
		{
			display: var(--sev2vis);
		}
		.sev1
		{
			display: var(--sev1vis);
			background-color:blue;
		}
		#console
		{
			padding:1em;
			margin:1em;
			font-size:small;
			overflow:scroll;
			background-color:#eee;
			font-family: mono;
			color:black;
			max-height:80vh;
		}

		#scroller
		{
			height:100vh;
			overflow:scroll;
		}

		#headerDiv
		{
			background-color: #f8f9fa !important;
			color: rgba(0,0,0,0.6);
			margin-top:0.5em;
			margin-bottom:0.5em;
			max-width:1200px;
			margin-left:auto;
			margin-right:auto;
			text-align:center;
		}

		.pageTab
		{
			display:none;
			flex-direction:column;
			flex:1 1 auto;
		}

		#wizardTab
		{
			display:flex;
		}
		#Console
		{
			flex-grow:1;
		}

		.bold
		{
			font-weight:bold;
		}

#consoleTab a
{
	display:inline-block;
	padding: 0.5em;
	margin: 0.5em;

}
		#plotsTab
		{
			flex-direction:row;
		}
		#configTab
		{
			overflow:scroll;
			flex-direction:column;
		}

		*
		{
			box-sizing: border-box;
		}

		#bothPlots
		{
			display:flex;
			flex-direction:row;
		}

#headerDiv > *
{
	vertical-align:middle;
	margin:1em;
}
		a, a:hover, a:visited, a:active
		{
  color: inherit;
  text-decoration: none;
 }
		#headerDiv H3
		{
			display:inline-block;
		}

		#map
		{
			width: 100%;
			height:100%;
		}

		#details {
			position: fixed;
			height: 200px;
			background: transparent;
			color: cadetblue;
			padding-left: 20px;
			top: 50px;
		}

		#details .title {
			color: slategray;
			font-size: small;
		}

		#locate {
			position: fixed;
			left: 20px;
			top: 10px;
		}

		.map-overlay {
			font: 12px/20px 'Helvetica Neue', Arial, Helvetica, sans-serif;
			position: absolute;
			width: 100%;
			bottom: 0;
			left: 0;
			padding: 10px;
			box-sizing: border-box;
			background-color: transparent;
		}

		.map-overlay .map-overlay-inner {
			background-color: transparent;
			box-shadow: 0 1px 2px rgba(0, 0, 0, 0.2);
			border-radius: 3px;
			padding: 10px;
			margin-bottom: 10px;
		}

		.map-overlay h2 {
			line-height: 24px;
			display: block;
			margin: 0 0 10px;
		}

		.map-overlay .legend .bar {
			height: 10px;
			width: 100%;
			background: linear-gradient(to right, #fca107, #7f3121);
		}

		.map-overlay input {
			background-color: transparent;
			display: inline-block;
			width: 100%;
			position: relative;
			margin: 0;
			cursor: ew-resize;
		}

		#configTab input
		{
		// display: inline-block;
		font-family: monospace;
		}

		body
		{
		font-family: monospace;
		}

.ident {
  position: relative;
  display: inline-block;
  border-bottom: 1px dotted black;
}

.ident .tooltiptext {
  visibility: hidden;
  width: 50vw;
  background-color: teal;
  color: #fff;
  text-align: left;
  border-radius: 6px;
  padding: 5px;
  position: absolute;
  z-index: 1;
  top: -5px;
  left: 110%;
}

.ident .tooltiptext::after {
  content: "";
  position: absolute;
  top: 50%;
  right: 100%;
  margin-top: -5px;
  border-width: 5px;
  border-style: solid;
  border-color: transparent teal transparent transparent;
}
.ident:hover .tooltiptext {
  visibility: visible;
}


#configTab input[type=text]
{
  border: none;
  width: 70vw;
}
.contents
{
  margin-left: 2em;
}


.value
{
  display: inline-block;
    -webkit-user-modify: read-write;
    -moz-user-modify: read-write;
    user-modify: read-write;
}

#configTab input:not(:checked) ~ div
  {
    color: grey;
  }
#configTab input:not(:checked) ~ input
  {
    pointer-events:none;
    color: grey;
  }
#configTab input:not(:checked) ~ .contents
  {
    display: none;
  }

#configTab input:not(:checked) + div::before
  {
    content:  "#";
  }

		)";

	html->addChild<Meta>()
		.attr("name", "viewport")
		.attr("content", "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no");

	html->addChild<Link>()
		.attr("rel", "stylesheet")
		.attr("href", "https://api.mapbox.com/mapbox-gl-js/v0.42.1/mapbox-gl.css");
	html->addChild<Link>()
		.attr("rel", "stylesheet")
		.attr("href", "pea.css");

	html->addChild<Script>()	.attr("src", "https://api.mapbox.com/mapbox-gl-js/v0.42.1/mapbox-gl.js");
	html->addChild<Script>()	.attr("src", "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.2.2/mapbox-gl-draw.js");
	html->addChild<Script>()	.attr("src", "https://unpkg.com/mqtt/dist/mqtt.min.js");
	html->addChild<Script>()	.attr("src", "https://cdn.plot.ly/plotly-2.16.1.min.js");
	html->addChild<Script>()	.attr("src", "pea.js");
	html->addChild<Script>()	.attr("src", "map.js");

	auto script = make_shared<Content>();

	*script << R"script(

	var getFilters = function()
	{
		var filters = new Set();
		for (child of document.getElementById("plots").children)
		for (token of child.id.split(","))
		{
			if (token == "")
			{
				continue;
			}

			if (child.style.display != "none")
				filters.add("," + token + ",");
		}

		document.getElementById("filters").replaceChildren();
		for (filter of Array.from(filters).sort())
		{
			if (filter == "")
				continue;

			var button = document.createElement("span");
			button.textContent = " " + filter + " ";

			button.addEventListener("click", function(ev)
			{
				console.log(ev);
				var filterText = document.getElementById("filter");
				filterText.value += ev.target.textContent;
				filterText.dispatchEvent(new CustomEvent("input", {}));
			});

			document.getElementById("filters").appendChild(button);
		}
	}

	getFilters();
	// get the references of the page elements.
	var listMsgs		= document.getElementById('msgs');
	var socketStatus	= document.getElementById('status');

	console.log("some script");

	load("config.html", document.getElementById("configTab"));

	var f = function(data)
	{
		if (data.send == "CONS")
		{
			var p = document.createElement("p");

			p.appendChild(document.createTextNode(data.content));
			p.classList.add("sev" + data.level);

			document.getElementById("Console").appendChild(p);

			return;
		}

		if (data.send == "DATA")
		{
			var average = data.y.reduce((a, b) => (a + b)) / data.y.length;
// 			data.y = data.y.map(y => y - average);

			document.getElementById("plot").data = document.getElementById("plot").data || [];

			// add a single trace to an existing graphDiv

			var found = null;

			for (var [index, trace] of document.getElementById("plot").data.entries())
			{
				if (trace.name == data.name)
				{
					found = index;
				}
			}



			if (found)
			{
				console.log("updating");
				var update = {'x': [data.x], 'y': [data.y]};
				Plotly.update("plot", update, {}, [found]);
			}
			else
			{
				console.log("new");
				Plotly.addTraces("plot", data);
			}


			Array.from(document.getElementsByClassName("legendtext")).forEach(function(el)
			{
				if (el.getAttribute("draggable") == null)
				{
					el.setAttribute("draggable", "true");
					var name = el.textContent;
					console.log(name);

					el.addEventListener("dragstart", function(ev)
					{
						ev.dataTransfer.setData("text", name);
					});
				}
			})
		}
	};

	socketCallbacks.push(f);


	function allowDrop(ev)
	{
		ev.preventDefault();
	}

	function drag(ev)
	{
		ev.dataTransfer.setData("text", ev.target.id);
	}

	function drop(ev)
	{
		ev.preventDefault();
		var data = ev.dataTransfer.getData("text");
		document.getElementById("plotted").appendChild(document.getElementById(data));
		socket.send("+" + data);
		console.log(data);
	}

	function openTab(name)
	{
		Array.from(document.getElementsByClassName("pageTab")).forEach(function(el)
		{
			el.style.display = "none";
		});
		document.getElementById(name).style.display = "flex";
		map.resize();
		Plotly.Plots.resize("plot");
	}

	function remove(ev)
	{

	}

	function removeAll()
	{
		var i = 0;

		while (i < document.getElementById("plotted").childNodes.length)
		{
			var child = document.getElementById("plotted").childNodes[i];
			document.getElementById("plots").appendChild(child);
			socket.send("-" + child.id);
			for (const [index, val] of document.getElementById("plot").data.entries())
			{
				if (child.id == val.name)
				{
					Plotly.deleteTraces("plot", index);
				}
			}
		}
	}

	function addAll()
	{
		var ev = {};
		ev.preventDefault = function(){};

		var i = 0;

		while (i < document.getElementById("plots").childNodes.length)
		{
			var child = document.getElementById("plots").childNodes[i];

			if (child.style.display != "none")
			{
				ev.dataTransfer = {};
				ev.dataTransfer.getData = function(){ return child.id;};
				drop(ev);
				continue;
			}
			i++;
		}
	}

	function toggleConsoleSev(level)
	{
		var display = window.getComputedStyle(document.documentElement).getPropertyValue("--sev" + level + "vis");
		if (display == "block")
		{
			document.documentElement.style.setProperty("--sev" + level + "vis", "none");
			document.documentElement.style.setProperty("--sev" + level + "border", "none");
		}
		else
		{
			document.documentElement.style.setProperty("--sev" + level + "vis", "block");
			document.documentElement.style.setProperty("--sev" + level + "border", "2px solid");
		}
	}

	var initFunc = function()
	{
		if (typeof Plotly == 'undefined')
		{
			setTimeout(initFunc, 10);
			return;
		}
		if (typeof mapboxgl == 'undefined')
		{
			setTimeout(initFunc, 10);
			return;
		}
		if (typeof mapInit == 'undefined')
		{
			setTimeout(initFunc, 10);
			return;
		}

		Plotly.newPlot('plot');

		mapInit();
	}
	initFunc();

	)script";




	auto gScript = make_shared<Content>();

	*gScript << R"script(

	var socketCallbacks = [];
	var socket;

	function onChangeNum(e)
	{
		var val = this.value;

		if (val == "on")	val = 1;
		if (val == "off")	val = 0;

		var thing = "%" + this.id + "%" + val;
		socket.send(thing);
	}

	window.onload = function init()
	{

		socket = new WebSocket('ws://localhost:8082');

// 		socket.open();
		socket.onopen = function(event)
		{
			console.log("open");
// 			socketStatus.innerHTML = 'Connected to: ' + event.currentTarget.URL;
// 			socketStatus.className = 'open';

// 			socket.send("test");
			setInterval(function() {socket.send("")}, 1000);
		};

		socket.onerror = function(error)
		{
			console.log('WebSocket error: ' + error);
		};

		socket.onclose = function(event)
		{
// 			socketStatus.innerHTML = 'Disconnected from the WebSocket.';
// 			socketStatus.className = 'closed';
		};

		socket.onmessage = function(event)
		{
			var msg = event.data;

// 			console.log(msg);

			var data = JSON.parse(msg);

			if (data.avail)
			{
// 				console.log("DOM " + data);

				if (document.getElementById(data.avail))
				{
// 					console.log("already have " + data.avail);
					return;
				}

				socket.send("?" + data.avail);
				return;
			}

			if (data.send == "DOM")
			{
// 				console.log("DOM " + data);


				if (document.getElementById(data.id))
					return;

				if (data.parent == null)
					return;

				var parent = document.getElementById(data.parent);
// 				if (parent == null)
// 					return;


				var newElement;
				if (data.type == null)
				{
					data.type = "span";
				}
				newElement = document.createElement(data.type);
				newElement.id = data.id;
				if (data.content)
					newElement.appendChild(document.createTextNode(data.content));
				parent.appendChild(newElement);

				if (data.attr)
				for (const [attr, val] of Object.entries(data.attr))
				{
					document.getElementById(data.id).setAttribute(attr, val);
				}

				if (data.numeric)
				{
					document.getElementById(data.id).addEventListener("change", onChangeNum);
				}

				if (data.style)
				{
					document.getElementById(data.id).style.cssText = data.style;
				}

				if (data.class)
				{
					const classes = data.class.split(" ");

					for (Class of classes)
					{
						if (Class == "")
							continue;
						document.getElementById(data.id).classList.add(Class);
					}
				}

// 				console.log("added have " + data.id + " to " + data.parent);
				return;
			}

			for (callback of socketCallbacks)
			{
				callback(data);
			}
		}
	}

	function load(url, element)
	{
		fetch(url)
			.then(response => response.text())
			.then(html =>
			{
				// console.log(html);
				element.innerHTML = html;
			})
	}
		)script";

	auto gTemplate = make_shared<Html>("html");

	gTemplate->addChild<Script>()
		.attr("src", "app.js");

	gTemplate->addChild<Body>("body");

	{
		ofstream out("index.html");

// 		std::cout << *html << std::endl;
		out << *gTemplate;
	}
	{
		ofstream out("app.js");

// 		std::cout << *script << std::endl;
		out << *gScript;
	}
	{
		ofstream out("pea.js");

// 		std::cout << *script << std::endl;
		out << *script;
	}
	{
		ofstream out("pea.css");

// 		std::cout << *script << std::endl;
		out << *style;
	}

	html->mapify(stringMap);
	html->stringify(stringList);


	for (auto& [str, content] : stringList)
	{
// 		std::cout << str << std::endl;
// 		if (requestedMap[str])
// 		{
// 			requestedMap[str] = false;
//
// 			lock_guard<mutex> guard(outMutex);
//
// 			outQueue.push_back(content);
// 		}
// 		else
		{
			bsoncxx::builder::basic::document doc = {};
			doc.append(kvp("avail",	 str));

			{

				lock_guard<mutex> guard(outMutex);

				outQueue.push_back(bsoncxx::to_json(doc));
			}
		}
	}

}
