# Coding Standards for C++

## Code style
Decades of experience has shown that codebases that are built with concise, clean code have fewer issues and are easier to maintain. If submitting a pull request for a patch to the software, please ensure your code meets the following standards.

Overall we are aiming to

* Write for clarity
* Write for clarity
* Write what you mean, not what is implied
* Write things once
* Use short, descriptive variable names
* Use aliases to reduce clutter.

### Inconcise code - Not recommended

	//check first letter of satellite type against something

	if (obs.Sat.id().c_str()[0]) == 'G') 
	    doSomething(); 
	else if (obs.Sat.id().c_str()[0]) == 'R')
	    doSomething();
	else if (obs.Sat.id().c_str()[0]) == 'E')
	    doSomething();
	else if (obs.Sat.id().c_str()[0]) == 'I')
	    doSomething();
    

### Clear Code - Good

	char& sysChar = obs.Sat.id().c_str()[0];

	switch (sysChar)
	{
	    case 'G':   doSomething();   break;
	    case 'R':   doSomething();   break;
	    case 'E':   doSomething();   break;
	    case 'I':   doSomething();   break;
	}


## Spacing, Indentation, and layout

* Use tabs, with tab spacing set to 4.	
* Use space or tabs before and after any ` + - * / = < > == != % ` etc..
* Use space, tab or new line after any `, ;`
* Use a new line after if statements.
* Use tabs to keep things tidy - If the same function is called multiple times with different parameters, the parameters should line up.

### Scattered Parameters - Bad

	trySetFromYaml(mongo_metadata,output_files,{"mongo_metadata" });
	trySetFromYaml(mongo_output_measurements,output_files,{"mongo_output_measurements" });
	trySetFromYaml(mongo_states,output_files,{"mongo_states" });

### Aligned Parameters - Good

	trySetFromYaml(mongo_metadata,             output_files, {"mongo_metadata"              });
	trySetFromYaml(mongo_output_measurements,  output_files, {"mongo_output_measurements"	});
	trySetFromYaml(mongo_states,               output_files, {"mongo_states"		        });

## Statements

One statement per line 

- `*`unless you have a very good reason

### Multiple Statements per Line - Bad

	z[k]=ROUND(zb[k]); y=zb[k]-z[k]; step[k]=SGN(y);

### Single Statement per Line - Good

	z[k]    = ROUND(zb[k]);
	y       = zb[k]-z[k]; 
	step[k] = SGN(y);

### Example of a good reason:

* Multiple statements per line sometimes shows repetitive code more clearly, but put some spaces so the separation is clear.

#### Normal

	switch (sysChar)
	{
	    case ' ':
	    case 'G': 
	        *sys = E_Sys::GPS; 
	        *tsys = TSYS_GPS; 
	        break;
	    case 'R': 
	        *sys = E_Sys::GLO;  
	        *tsys = TSYS_UTC; 
	        break;
	    case 'E': 
	        *sys = E_Sys::GAL;  
	        *tsys = TSYS_GAL; 
	        break;
	    //...continues
	}

#### Ok

	if      (sys == SYS_GLO)    fact = EFACT_GLO;
	else if (sys == SYS_CMP)    fact = EFACT_CMP;
	else if (sys == SYS_GAL)    fact = EFACT_GAL;
	else if (sys == SYS_SBS)    fact = EFACT_SBS;
	else                        fact = EFACT_GPS;

#### Ok

	switch (sysChar)
	{
	    case ' ':
	    case 'G':   *sys = E_Sys::GPS;      *tsys = TSYS_GPS;    break;
	    case 'R':   *sys = E_Sys::GLO;      *tsys = TSYS_UTC;    break;
	    case 'E':   *sys = E_Sys::GAL;      *tsys = TSYS_GAL;    break;
	    case 'S':   *sys = E_Sys::SBS;      *tsys = TSYS_GPS;    break;
	    case 'J':   *sys = E_Sys::QZS;      *tsys = TSYS_QZS;    break;
	//...continues
	}

## Braces

New line for braces.

	if (pass)
	{
	    doSomething();
	}

## Comments

* Prefer `//` for comments within functions
* Use `/* */` only for temporary removal of blocks of code.
* Use `/** */` and `///<` for automatic documentation

## Conditional checks

* Put `&&` and `||` at the beginning of lines when using multiple conditionals.
* Always use curly braces when using multiple conditionals.

```
if  ( ( testA  >  10)
    &&( testB  == false
      ||testC  == false))
{
    //do something
}
```

## Return Values

* Use variables to name return values rather than using functions directly

### Bad

	if (doSomeParsing(someObject))
	{
	    //code contingent on parsing success? failure?
	}

### Good
	bool fail = doSomeParsing(someObject);
	if (fail)
	{
	    //This code is clearly a response to a failure
	}

## Variable declaration

* Declare variables as late as possible - at point of first use.
* One declaration per line.
* Declare loop counters in loops where possible.
* Always initialise variables at declaration.

```
int  type  = 0;
bool found = false;         //these have to be declared early so they can be used after the for loop

for (int i = 0; i < 10; i++)
{
    bool pass = someTestFunction();    //this pass variable isnt declared until it's used - good
    if (pass)
    {
        type  = typeMap[i];
        found = true;
        break;
    }
}

if (found)
{
    //...
}    
```

## Function parameters

* One per line.
* Add doxygen compatible documentation after parameters in the cpp file.
* Prefer references rather than pointers unless unavoidable.

```
void function(
        bool        runTests,           ///< Run unit test while processing
        MyStruct&   myStruct,           ///< Structure to modify
        OtherStr*   otherStr = nullptr) ///< Optional structure object to populate (cant use reference because its optional)
{
    //...
}
```

## Naming and Structure

* Limit use of abbreviations
* For structs or classes, use `CamelCase` with capital start
* For member variables, use `camelCase` with lowercase start
* For config parameters, use `lowercase_with_underscores`
* Use suffixes (`_ptr`, `_arr`, `Map`, `List` etc.) to describe the type of container for complex types.
* Be sure to provide default values for member variables.
* Use heirarchical objects where applicable.

```
struct SubStruct
{
    int    type = 0;
    double val  = 0;
};

struct MyStruct
{
    bool          memberVariable = false;
    double        precision      = 0.1;

    double                     offset_arr[10]  = {};
    OtherStruct*               refStruct_ptr   = nullptr;

    map<string, double>        offsetMap; 
    list<map<string, double>>  variationMapList;
    map<int, SubStruct>        subStructMap;
};

//...

MyStruct myStruct = {};

if (acsConfig.some_parameter)
{
    //..
}
```

## Undesirable Code

* Do not use 'magic numbers', which require knowledge of other code fragments for comprehension. If a comment is required for explaining what a value means, the code should be rewritten with enums or defined constants. 
* Do not append `.0` to integer valued doubles unless they are required.
* Never use `free()`, `malloc()`, or `new` unless it cannot be avoided.
* Threads create synchronisation issues, they should not be used unless manual synchronisation is never required.

## Testing

* Use `TestStack` objects at top of each function that requires automatic unit testing.
* Use `TestStack` objects with descriptive strings in loops that wrap functions that require automatic unit testing.

```
void function()
{
    TestStack ts(__FUNCTION__);

    //...

    for (auto& obs : obsList)
    {
        TestStack ts(obs.Sat.id());

        //...
    }
}
```

## Documentation

* Use doxygen style documentation for function and struct headers and parameters
* `/** ` for headers.
* `///<` for parameters

```
/** Struct to demonstrate documentation.
* The first line automatically gets parsed as a brief description, but more detailed descriptions are possible too.
*/
struct MyStruct
{
    bool    dummyBool;                  ///< The thing to the left is documented here
};

/** Function to demonstrate documentation
*/
void function(
        bool        runTests,           ///< Run unit test while processing
        MyStruct&   myStruct,           ///< Structure to modify
        OtherStr*   otherStr = nullptr) ///< Optional string to populate
{
   	//...
}
```

## STL Templates

* Prefer maps rather than fixed arrays.
* Prefer range-based loops rather than iterators or `i` loops, unless unavoidable.

### Bad

	double double_arr[10] = {};

	//..(Populate array)

	for (int i = 0; i < 10; i++)    //Magic number 10 - bad.
	{

	}


	map<string, double> doubleMap;

	//..(Populate Map)

	for (auto iter = doubleMap.begin(); iter != doubleMap.end(); iter++)   //long, undescriptive - bad
	{
	   	if (iter->first == someVar)     //'first' is undescriptive - bad
	   	{
	   		//..
	   	}
	}

### Good - Iterating Maps

	map<string, double> offsetMap;

	//..(Populate Map)

	for (auto& [siteName, offset] : doubleMap)	//give readable names to map keys and values
	{
	    if (siteName.empty() == false)
	    {
	    
	    }
	}

### Good - Iterating Lists

	list<Obs> obsList;

	//..(Populate list)

	for (auto& obs : obsList)         //give readable names to list elements
	{
	    doSomethingWithObs(obs);
	}

### Special Case - Deleting from maps/lists

Use iterators when you need to delete from STL containers:

```
for (auto it = someMap.begin(); it != someMap.end();  )
{
    KFKey key = it->first;				//give some alias to the key/value so they're readable

    if (measuredStates[key] == false)
    {
        it = someMap.erase(it);
    }
    else
    {
        ++it;
    }
}
```

## Namespaces

Commonly used std containers may be included with `using`

	#include <string>
	#include <map>
	#include <list>
	#include <unordered_map>

	using std::string;
	using std::map;
	using std::list
	using std::unordered_map;


## Code sequencing

The software is to be kept largely sequential - using threads sparingly to limit the overhead of collision avoidance.
Where possible tasks are completed in parallel using parallelisation libraries to take advantage of all cpu cores in multi-processor systems while still retaining a linear flow through the execution.

Sections of the software that create and modify global objects, such as while reading ephemeris data, will be executed on a single core only. 
This will ensure that collisions are avoided and the debugging of these functions is deterministic.

For sections of the software that have clear delineation between objects, such as per-receiver calculations, these may be completed in parallel, provided they do not attempt to modify or create objects with more global scope. When globally accessible objects need to be created for individual receivers, they should be pre-initialised before the entry to parallel execution section.




