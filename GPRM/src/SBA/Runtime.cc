// SBA/Runtime.rb
//
// :title: Gannet Service-based SoC project - SBA Runtime class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//
////==============================================================================
////
//// Gannet Service-based SoC project - SBA Runtime class
////
////==============================================================================

#include "Runtime.h"
#include "Timings.h"
#include "Schedule.h"

double custome_time=0.0;


#define RESET_COLOR "\e[m"
#define MAKE_PURPLE "\e[35m"
#define MAKE_GREEN "\e[32m"
#define MAKE_RED "\e[31m"

using namespace std;
using namespace SBA;

void Runtime::init(std::string td_, uint ncycles_) {
#if VERBOSE==1
	std::cout << "Runtime::init("<<td_<<", "<<ncycles_<<")\n";
#endif
	ncycles=ncycles_;
	if (task_description==td_) {
		reuse=true;
		sba.reuse=true;
	} else {
		task_description=td_;
		sba.task_description=td_;
	}
}


void Runtime::setNCycles(uint ncycles_) {
	ncycles=ncycles_;
}

void Runtime::args(const vector< void* >& argv) {
    copy( argv.begin(),argv.end(),back_insert_iterator< vector< void* > >(sba.args) );
}


 void* Runtime::run() {
 #if VERBOSE==1
 		cout << "=========================================================================="<<endl;
 		cout << "Running ..."<<endl;
 #endif // VERBOSE
 #ifdef TIMINGS
         double start = wsecond();
 #endif
         //printf("\n Start time of the application: %f \n", start);
         uint t=0;
         while (t<ncycles){
             t=t+1;
 #if VERBOSE==1
             cout << " *** STEP " <<t<< " ***"<<endl;
 #endif // VERBOSE
             sba.run();
             if (sba.finished == 1){
                 return sba.result;
             }
         }
         cout << "Warning: GPRM ran for " <<ncycles<< " cycles without completion."<<endl;
         cout << "         Try increasing the number of cycles on the command line."<<endl;
 #ifdef TIMINGS
		double end = wsecond();
		//printf("\n End time of the application: %f \n", end);
		double elapsed = end - start;
		FILE * pFile;
		pFile=fopen("gprm.txt","a");
		if (pFile==NULL) {
			printf("\nUnable to open gprm.txt");
		}
		fprintf(pFile, "%f\t", end-custome_time);
		fclose (pFile);
		cout << MAKE_RED "Function Computation Run time:\t" << end-custome_time << RESET_COLOR "\n";
		cout << MAKE_RED "Run time:\t" << elapsed << "s" RESET_COLOR "\n";
 #endif

         return sba.result;
 	}

void* Runtime::run(const vector< void* >& argv) {
	 args(argv);

	 void* res = run();
	 return res;
}
