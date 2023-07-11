 

// The MIT License (MIT)
//
// Copyright (c) 2016 Stack Exchange
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// ===============================================
"use strict";

var StackExchangemathjaxEditing;
var ready = false; // true after initial typeset is complete
var pending = false; // true when MathJax has been requested
var preview = null; // the preview container
var inline = "$"; // the inline math delimiter
var blocks, start, end, last, braces; // used in searching for math
var math; // stores math until markdone is done

//
//  The pattern for math delimiters and special symbols
//    needed for searching for math in the page.
//
var SPLIT = /(\$\$?|\\(?:begin|end)\{[a-z]*\*?\}|\\[\\{}$]|[{}]|(?:\n\s*)+|@@\d+@@)/i;

//
//  The math is in blocks i through j, so 
//    collect it into one block and clear the others.
//  Replace &, <, and > by named entities.
//  For IE, put <br> at the ends of comments since IE removes \n.
//  Clear the current math positions and store the index of the
//    math, then push the math string onto the storage array.
//
function processMath(i, j, preProcess) {
    var block = blocks.slice(i, j + 1).join("").replace(/&/g, "&amp;") // use HTML entity for &
    .replace(/</g, "&lt;") // use HTML entity for <
    .replace(/>/g, "&gt;") // use HTML entity for >
    ;
    //if (HUB.Browser.isMSIE) {
    //    block = block.replace(/(%[^\n]*)\n/g, "$1<br/>\n")
    //}
    while (j > i) {
        blocks[j] = "";
        j--;
    }
    blocks[i] = "@@" + math.length + "@@";
    if (preProcess)
        block = preProcess(block);
    math.push(block);
    start = end = last = null;
}


var capturingStringSplit;
if ("aba".split(/(b)/).length === 3) {
    capturingStringSplit = function (str, regex) { return str.split(regex) }
}
else { // IE8
    capturingStringSplit = function (str, regex) {
        var result = [], match;
        if (!regex.global) {
            var source = regex.toString(),
                flags = "";
            source = source.replace(/^\/(.*)\/([im]*)$/, function (wholematch, re, fl) { flags = fl; return re; });
            regex = new RegExp(source, flags + "g");
        }
        regex.lastIndex = 0;
        var lastPos = 0;
        while (match = regex.exec(str))
        {
            result.push(str.substring(lastPos, match.index));
            result.push.apply(result, match.slice(1));
            lastPos = match.index + match[0].length;
        }
        result.push(str.substring(lastPos));
        return result;
    }
}


//
//  Break up the text into its component parts and search
//    through them for math delimiters, braces, linebreaks, etc.
//  Math delimiters must match and braces must balance.
//  Don't allow math to pass through a double linebreak
//    (which will be a paragraph).
//
function removeMath(text) {
    start = end = last = null; // for tracking math delimiters
    math = []; // stores math strings for latter
    
    // Except for extreme edge cases, this should catch precisely those pieces of the markdown
    // source that will later be turned into code spans. While MathJax will not TeXify code spans,
    // we still have to consider them at this point; the following issue has happened several times:
    //
    //     `$foo` and `$bar` are varibales.  -->  <code>$foo ` and `$bar</code> are variables.

    var hasCodeSpans = /`/.test(text),
        deTilde;
    if (hasCodeSpans) {
        text = text.replace(/~/g, "~T").replace(/(^|[^\\`])(`+)(?!`)([^\n]*?[^`\n])\2(?!`)/gm, function (wholematch) {
            return wholematch.replace(/\$/g, "~D");
        });
        deTilde = function (text) { return text.replace(/~([TD])/g, function (wholematch, character) { return { T: "~", D: "$" }[character]; }) };
    } else {
        deTilde = function (text) { return text; };
    }
    
    
    blocks = capturingStringSplit(text.replace(/\r\n?/g, "\n"), SPLIT);
    
    for (var i = 1, m = blocks.length; i < m; i += 2) {
        var block = blocks[i];
        if (block.charAt(0) === "@") {
            //
            //  Things that look like our math markers will get
            //  stored and then retrieved along with the math.
            //
            blocks[i] = "@@" + math.length + "@@";
            math.push(block);
        }
        else if (start) {
            //
            //  If we are in math, look for the end delimiter,
            //    but don't go past double line breaks, and
            //    and balance braces within the math.
            //
            if (block === end) {
                if (braces) {
                    last = i
                }
                else {
                    processMath(start, i, deTilde)
                }
            }
            else if (block.match(/\n.*\n/)) {
                if (last) {
                    i = last;
                    processMath(start, i, deTilde)
                }
                start = end = last = null;
                braces = 0;
            }
            else if (block === "{") {
                braces++
            }
            else if (block === "}" && braces) {
                braces--
            }
        }
        else {
            //
            //  Look for math start delimiters and when
            //    found, set up the end delimiter.
            //
            if (block === inline || block === "$$") {
                start = i;
                end = block;
                braces = 0;
            }
            else if (block.substr(1, 5) === "begin") {
                start = i;
                end = "\\end" + block.substr(6);
                braces = 0;
            }
        }
    }
    if (last) {
        processMath(start, last, deTilde)
    }
    return deTilde(blocks.join(""));
}

//
//  Put back the math strings that were saved,
//    and clear the math array (no need to keep it around).
//  
function replaceMath(text) {
    text = text.replace(/@@(\d+)@@/g, function (match, n) {
        return math[n]
    });
    math = null;
    return text;
}

$.urlParam = function(name)
{
	var results = new RegExp('[\?&]' + name + '=([^&#]*)').exec(window.location.href);
	if (results==null) 
    {
		return null;
	}
	return decodeURI(results[1]) || 0;
}

function addIncludes(editedStuff)
{
    const regexp = /^(\\include) "(.+)"/gm;
    const matches = editedStuff.matchAll(regexp);
    
    var i = 0;
    var includes = [];

    for (const match of matches) 
    {
        $.get(match[2], function(includeData, statusTxt)
        {
          if (statusTxt == "success")
          {
            includes[i] = includeData;
            editedStuff = editedStuff.replace(match[0], "```\n" + includeData + "\n```");
          }
          if (statusTxt == "error")
          {
            alert("Error:");
          }
        });
    }


    return editedStuff;
}

function addMarkdown(chapterdata)
{
                var editedStuff = removeMath(chapterdata, '$');

                var moreEditedStuff = addIncludes(editedStuff);

                var newEditedStuff = marked.parse(moreEditedStuff);

                newEditedStuff = replaceMath(newEditedStuff);

                document.getElementById('maths').innerHTML += newEditedStuff;
}

$(document).ready(function()
{
  var page = "home.index";
  var chapters;
	if ($.urlParam('p'))
		page = $.urlParam('p');
  jQuery.ajaxSetup({async:false});
  $.get(page, function(data, statusTxt)
  {
    if(statusTxt == "success")
    {

      if (page.search(/\.index$/) >= 0)
      {
          chapters = data.split(/\r?\n/);

          chapters.forEach(function(chapter)
          {
            //alert("getting " + chapter);

            $.get(chapter, function(chapterdata, statusTxt)
            {
              if (statusTxt == "success")
              {
                addMarkdown(chapterdata);
              }
              if (statusTxt == "error")
              {
                alert("Error:");
              }
            });
          });

          if(statusTxt == "error")
          {
            alert("Error:");
          }
      }
      else if (page.search(/\.html$/) >= 0)
      {
        console.log("got " + page);
        document.getElementById('maths').innerHTML += data;
      }
      else
      {
        console.log("got " + page);
        addMarkdown(data);
      }
    }
  });

  MathJax.typeset();


if ($.urlParam('c')=="on")
{
  Toc.init({
    $nav: $("#myNavbar")
    // ...
  });
}
else
{
    $("#myNavbar").addClass("col-lg-2");
    $("#myNavbar").removeClass("col-lg-3");
    $("#myNavbar").removeClass("col-lg-4");
    $("#myNavbar").css('visibility', 'hidden');
}

});
