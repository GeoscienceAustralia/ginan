
#pragma once


typedef void ParallelArchitecture;
typedef void Architecture;
typedef void FileType;
typedef void Database;
typedef void Library;
typedef void Output;
typedef void Input;

#define DOCS_REFERENCE(ref)		\
Architecture ref();				\
if (doDocs) ref();


extern bool doDocs;
