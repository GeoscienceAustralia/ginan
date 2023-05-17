
#pragma once

#include <fstream>
#include <string>
#include <memory>
#include <list>
#include <map>
#include <any>

#include <bsoncxx/builder/basic/document.hpp>
#include <bsoncxx/json.hpp>

using bsoncxx::builder::basic::sub_document;
using bsoncxx::builder::basic::sub_array;
using bsoncxx::builder::basic::kvp;

using std::make_shared;
using std::shared_ptr;
using std::ostream;
using std::string;
using std::list;
using std::map;


// namespace Anode
// {
	struct ANode;
	struct Content;
	
	ostream& operator<<(
		ostream&	out, 
		ANode&		anode);
	
	struct ANode : list<shared_ptr<ANode>>
	{
		static int nextGuid;
		static map<string, shared_ptr<ANode>>	anodeIdMap;
		
		string id;
		string parent;
		string appF;
		string content;
		
		string styleString = "";
		string classString = "";
		
		map<string, string>	stringAttributes;
		map<string, int>	intAttributes;
		map<string, double>	doubleAttributes;
		
		ANode& onAppend(string evaluate)		{	appF = evaluate;				return *this;	}
		ANode& attr(string at,	string	val)	{	stringAttributes[at] = val;		return *this;	}
		ANode& attr(string at,	double	val)	{	doubleAttributes[at] = val;		return *this;	}
		
		ANode& style(string entry)				{	styleString += entry + " ";		return *this;	}
		ANode& Class(string entry)				{	classString += entry + " ";		return *this;	}
		ANode& attach(double& entry)			{	numeric = &entry;				return *this;	}
		ANode& attach(string& entry)			{	textual = &entry;				return *this;	}
	
		string	type;
		bool	close		= false;
		bool	childless	= false;
		double*	numeric		= nullptr;
		string*	textual		= nullptr;
		
		ANode(string id = "")
		{
			if (id.empty())
				this->id = type + std::to_string(nextGuid++);
			else
				this->id = id;
		}
		
		template<typename TYPE>
		ANode& addChild(string id = "")
		{
			if (id.empty())
			{
				id = this->id + "_" + std::to_string(this->size());
			}
	
			auto child = make_shared<TYPE>(id);
			push_back(child);
			anodeIdMap[id] = child;

			back()->parent = this->id;
			
			
			return *back();
		}
		
		virtual ostream& shift(ostream& out)
		{
			if (type.empty() == false)
			{
				out 
				<< std::endl
				<< "<" 
				<< type
				<< " id='"
				<< id
				<< "'";
			
				for (auto& [at, val] : stringAttributes)	{	out << " " << at << "='"	<< val << "'";	} 
				for (auto& [at, val] : doubleAttributes)	{	out << " " << at << "="		<< val;			} 
				
				if (styleString.empty() == false)			{	out << " style=\"" << styleString << "\" ";	}
				if (classString.empty() == false)			{	out << " class=\"" << classString << "\" ";	}
				
				if (close == false)
				{
					out 
					<< "/";
				}
				
				out
				<< ">";
			}
			
			if (content.empty() == false)			{	out << content;	}
			out << std::flush;
			
			for (auto& child_ptr : *this)
			{
				out << *child_ptr;
			}
			
			if (type.empty() == false)
			if (close)
			{
				out
				<< std::endl
				<< "</"
				<< type
				<< ">";
			}
			
			out
			<< std::endl;
			
			return out;
		}
	
		virtual operator string()
		{
			bsoncxx::builder::basic::document doc = {};
												doc.append(kvp("send",				"DOM"		));
			if (id			.empty() == false)	doc.append(kvp("id",				id			));
			if (type		.empty() == false)	doc.append(kvp("type",				type		));
			if (parent		.empty() == false)	doc.append(kvp("parent",			parent		));
			if (appF		.empty() == false)	doc.append(kvp("onAppend",			appF		));
			if (content		.empty() == false)	doc.append(kvp("content",			content		));
			if (styleString	.empty() == false)	doc.append(kvp("style",				styleString	));
			if (classString	.empty() == false)	doc.append(kvp("class",				classString	));
			if (numeric)						doc.append(kvp("numeric",			true		));
			if (textual)						doc.append(kvp("textual",			true		));
			
			if	( stringAttributes.size()
				||intAttributes.size())
			doc.append(kvp("attr", [&](sub_document subDoc)
			{
				for (auto& [attr, val] : stringAttributes)		subDoc.append(kvp(attr, val));
				for (auto& [attr, val] : intAttributes)			subDoc.append(kvp(attr, val));
			}));
			
			return bsoncxx::to_json(doc);
		}
		
		void stringify(
			list<tuple<string,string>>& stringList)
		{
// 			std::cout << (string)*this << std::endl;
			stringList.push_back({id,*this});
			for (auto& child_ptr : *this)
			{
				child_ptr->stringify(stringList);
			}
		}
		
		void mapify(
			map<string, string>& stringMap)
		{
			stringMap[id] = *this;
			for (auto& child_ptr : *this)
			{
				child_ptr->mapify(stringMap);
			}
		}
		
		virtual ANode& operator << (string content);
	};
	
	struct Html		: ANode	{	Html	(string id=""):ANode(id){	type = "html";		close = true;	}	};
	struct Body		: ANode	{	Body	(string id=""):ANode(id){	type = "body";		close = true;	}	};
	struct Style	: ANode	{	Style	(string id=""):ANode(id){	type = "style";		close = true;	childless = true;}	};
	struct Div		: ANode	{	Div		(string id=""):ANode(id){	type = "div";		close = true;	}	};
	struct Span		: ANode	{	Span	(string id=""):ANode(id){	type = "span";		close = true;	childless = true;}	};
	struct Br		: ANode	{	Br		(string id=""):ANode(id){	type = "br";						}	};
	struct Img		: ANode	{	Img		(string id=""):ANode(id){	type = "img";						}	};
	struct Canvas	: ANode {	Canvas	(string id=""):ANode(id){	type = "canvas";	close = true;	}	};
	struct H1		: ANode {	H1		(string id=""):ANode(id){	type = "h1";		close = true;	}	};
	struct A		: ANode {	A		(string id=""):ANode(id){	type = "a";			close = true;	}	};
	struct Meta		: ANode {	Meta	(string id=""):ANode(id){	type = "meta";						}	};
	struct Link		: ANode {	Link	(string id=""):ANode(id){	type = "link";						}	};
	struct H2		: ANode {	H2		(string id=""):ANode(id){	type = "h2";		close = true;	}	};
	struct H3		: ANode {	H3		(string id=""):ANode(id){	type = "h3";		close = true;	}	};
	struct H4		: ANode {	H4		(string id=""):ANode(id){	type = "h4";		close = true;	}	};
	struct H5		: ANode {	H5		(string id=""):ANode(id){	type = "h5";		close = true;	}	};
	struct Script	: ANode {	Script	(string id=""):ANode(id){	type = "script";	close = true;	childless = true;}	};
	struct Ul		: ANode {	Ul		(string id=""):ANode(id){	type = "ul";		close = true;	}	};
	struct Li		: ANode {	Li		(string id=""):ANode(id){	type = "li";		close = true;	}	};
	struct Label	: ANode {	Label	(string id=""):ANode(id){	type = "label";		close = true;	}	};
	struct Select	: ANode {	Select	(string id=""):ANode(id){	type = "select";	close = true;	}	};
	struct Option	: ANode {	Option	(string id=""):ANode(id){	type = "option";	close = true;	childless = true;}	};
	struct Input	: ANode {	Input	(string id=""):ANode(id){	type = "input";						}	};
	struct Button	: ANode {	Button	(string id=""):ANode(id){	type = "button";					}	};
	
	struct Content : ANode
	{
		
		Content(
			string id	= "")
		:	ANode(id)
		{
			childless = true;
		}
		
		ostream& shift(ostream& out) override
		{
			return out << content;
		}
	};
	
	ostream& operator<<(
		ostream&	out, 
		ANode&		anode);
// };
