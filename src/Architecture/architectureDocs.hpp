
#pragma once


typedef void Architecture;
typedef void Library;
typedef void Output;
typedef void Input;
typedef void FileType;

#define DOCS_REFERENCE(ref)		\
Architecture ref();				\
if (doDocs) ref();


extern bool doDocs;
