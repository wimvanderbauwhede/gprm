/*
 *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */

#ifdef TOP_CYCLES
#include "../cycle.h"
#endif

#ifdef TIMINGS
#include <sys/time.h>
#endif



#ifdef WV_BOOST
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>

using namespace boost;
namespace po = boost::program_options;
#endif


#include <iostream>
//#include <algorithm>
//#include <iterator>
#include "../SBA/Types.h"
#include "../SBA/Runtime.h"

using namespace std;
using namespace SBA;

#ifdef WV_SYSTEMC
int sc_main(int ac, char *av[]) {
#else
int main(int ac, char* av[]) {
#endif

//    try {
        unsigned int ncycles=500;
        string tdc_file="NONE";
#ifdef WV_BOOST
        po::options_description desc("Allowed options");
        desc.add_options()
            ("help", "produce help message")
            ("ncycles", po::value<unsigned int>(&ncycles)->default_value(500),
                  "Run for this number of clock cycles")
//            ("include-path,I", po::value< vector<string> >(),
//                  "include path")
            ("double-precision", "Use IEEE 754 double-precision floating point arithmetic")
            ("input-file", po::value< string >(&tdc_file), "Byte-compiled task description file (.tdc)")
        ;

        po::positional_options_description p;
        p.add("input-file", -1); // means

        po::variables_map vm;
        po::store(po::command_line_parser(ac, av).
                  options(desc).positional(p).run(), vm);
        po::notify(vm);

        if (vm.count("help")) {
            cout << "Usage: options_description [options]\n";
            cout << desc;
            return 0;
        }
#else
        string desc="";
//        vector<string> args;// (av, av+ac);
        if(ac>1) {
//            std::cout<<"ac:"<<ac<<"\n";
//        args.push_back(av[1]);
        tdc_file=av[1];//args.at(0);
        if(ac>2) {
            ncycles=atoi(av[2]);
        }
        }
#endif
//        if (vm.count("include-path"))
//        {
//            cout << "Include paths are: "
//                 << vm["include-path"].as< vector<string> >() << "\n";
//        }

//        if (vm.count("input-file"))
//        {
//            cout << "Input files are: "
//                 << vm["input-file"].as< vector<string> >() << "\n";
//        }

          if (tdc_file=="NONE") {
          	            cout << "Usage: tdc_file [ncycles] (default:"<<ncycles<<")\n";
            cout << desc;
            exit(-1);
          }

//          	  cout << "Input file is : " << tdc_file <<"\n";
	  string data_file=tdc_file;
#ifdef WV_BOOST
	  replace_last(data_file, ".tdc", ".data");
#else
	  unsigned int base_str_len=data_file.size()-4;
	data_file.resize(base_str_len);
	data_file+=".data";
#endif
//	  cout << "Data file is : " << data_file <<"\n";
//        cout << "Number of cycles is " << ncycles << "\n";

// Now for the real stuff
		StringPair sp;
		sp.taskfile=tdc_file;//"../../Tasks/task_pre_alu.tdc";
#if DATA==1
		sp.datafile=data_file;//"../../Tasks/task_pre_alu.data";
#endif
		TaskDescList tds;
		tds.push_back(sp);

	    Runtime gannet(tdc_file,ncycles);
#ifdef TOP_CYCLES
     ticks t0=getticks();
#endif
	    gannet.run();
#ifdef TOP_CYCLES
     ticks t1=getticks();
cout << "TOP CYCLES: "<<  elapsed(t1,t0) <<"\n";
#endif

//    }
/*
    catch(exception& e)
    {
        cout << e.what() << "\n";
        return 1;
    }
*/
    return 0;
}
