// Copyright Vladimir Prus 2002-2004.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)
#include "navigation.hpp"
#include "snx.hpp"
nav_t nav = {};

#include <boost/program_options.hpp>

using namespace boost;
namespace po = boost::program_options;

#include <iostream>
#include <algorithm>
#include <iterator>
using namespace std;

#include <boost/assign/list_inserter.hpp> // for 'insert()'
using namespace boost::assign; 

// A helper function to simplify the main part.
template<class T>
ostream& operator<<(ostream& os, const vector<T>& v)
{
    copy(v.begin(), v.end(), ostream_iterator<T>(os, " ")); 
    return os;
}

int main(int ac, char* av[])
{
    try {
        int opt;
        int portnum;
        po::options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "produce help message")
            ("verbose,v", po::value<int>()->implicit_value(1),
                  "enable verbosity (optionally specify level)")
            ("input-file,f", po::value< vector<string> >(), "input file")
        ;

        po::positional_options_description p;
        p.add("input-file", -1);
        
        po::variables_map vm;
        po::store(po::command_line_parser(ac, av).
                  options(desc).positional(p).run(), vm);
        po::notify(vm);
    
        if (vm.count("help")) {
            cout << "Usage: options_description [options]\n";
            cout << desc;
            return 0;
        }

        if (vm.count("input-file"))
        {
            cout << "Input files are: " 
                 << vm["input-file"].as< vector<string> >() << "\n";
        }

        if (vm.count("verbose")) {
            cout << "Verbosity enabled.  Level is " << vm["verbose"].as<int>()
                 << "\n";
        } 
            
        if (vm.count("input-file"))
            {
                cout<<vm["input-file"].as< vector<string> >();
                auto test = vm["input-file"].as< vector<string> >();
                // auto test2 = vm["input-file"].as< vector<string> >();
                for (auto it = test.begin(); it != test.end(); it++ )
                {
                    cout <<*it<< endl;
                    //   string filepath ("/data/acs/pea/proc/exs/products/igs19P2062.snx");
                    read_sinex(*it, true);
                    for (auto [index, estimate] : theSinex.map_estimates_primary)
                    {
                        // cout << estimate.sitecode.c_str()<< estimate.refepoch[0] << estimate.type.c_str() << endl;
                        //printf("%s %d:%d:%d %s %f %f\n", 
                        //estimate.sitecode.c_str(),
                        //estimate.refepoch[0],
                        //estimate.refepoch[1],
                        //estimate.refepoch[2],
                        //estimate.type.c_str(),
                        //estimate.estimate,
                        //estimate.stddev
                        //);
                    }
                }
                 
            }      
        }
    catch(std::exception& e)
    {
        cout << e.what() << "\n";
        return 1;
    }    



    return 0;
}

