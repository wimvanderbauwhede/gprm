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
#include <algorithm>
#include <iterator>

using namespace std;
using namespace SBA;

void Runtime::setNCycles(uint ncycles_) {
	ncycles=ncycles_;
}

void Runtime::args(const vector< void* >& argv) {
    copy( argv.begin(),argv.end(),back_insert_iterator< vector< void* > >(sba.args) );
}

void Runtime::args(void* arg0) {
	sba.args.push_back(arg0);
}

void Runtime::args(void* arg0,void* arg1) {
	sba.args.push_back(arg0);
	sba.args.push_back(arg1);
}

void Runtime::args(void* arg0,void* arg1,void* arg2) {
	sba.args.push_back(arg0);
	sba.args.push_back(arg1);
	sba.args.push_back(arg2);
}

void Runtime::args(void* arg0,void* arg1,void* arg2,void* arg3) {
	sba.args.push_back(arg0);
	sba.args.push_back(arg1);
	sba.args.push_back(arg2);
	sba.args.push_back(arg3);
}
/*
 Word_List& Runtime::run_OFF() {
#if VERBOSE==1
		cout << "=========================================================================="<<endl;
		cout << "Running ..."<<endl;
#endif // VERBOSE
#if USE_THREADS==0
        uint t=0;
        while (t<ncycles){
            t=t+1;
#if VERBOSE==1
            cout << " *** STEP " <<t<< " ***"<<endl;
#endif // VERBOSE
            sba.run();
            if (sba.finished == 1                    ){
                return sba.results;
            }
        }
        cout << "Warning: GannetVM ran for " <<ncycles<< " cycles without completion."<<endl;
        cout << "         Try increasing the number of cycles on the command line."<<endl;
#else //  USE_THREADS==1
        double start = wsecond();
        //printf("\n Start time of the application: %f \n", start);

        sba.run_th();
        double end = wsecond();
        //printf("\n End time of the application: %f \n", end);
        double elapsed = end - start;
        cout << "Run time:\t" << (int)(1000*elapsed) << "ms\n";
#endif // USE_THREADS
        return sba.results;

	}
*/

 void* Runtime::run() {
 #if VERBOSE==1
 		cout << "=========================================================================="<<endl;
 		cout << "Running ..."<<endl;
 #endif // VERBOSE
 #if USE_THREADS==0
         uint t=0;
         while (t<ncycles){
             t=t+1;
 #if VERBOSE==1
             cout << " *** STEP " <<t<< " ***"<<endl;
 #endif // VERBOSE
             sba.run();
             if (sba.finished == 1                    ){
                 return sba.result;
             }
         }
         cout << "Warning: GannetVM ran for " <<ncycles<< " cycles without completion."<<endl;
         cout << "         Try increasing the number of cycles on the command line."<<endl;
 #else //  USE_THREADS==1
         double start = wsecond();
         //printf("\n Start time of the application: %f \n", start);

         sba.run_th();
         double end = wsecond();
         //printf("\n End time of the application: %f \n", end);
         double elapsed = end - start;
         cout << "Run time:\t" << (int)(1000*elapsed) << "ms\n";
 #endif // USE_THREADS
         return sba.result;
 	}

 void* Runtime::run(const vector< void* >& argv) {
	 args(argv);
	 void* res = run();
	 return res;
 }

 void* Runtime::run(void* arg0) {
	 args(arg0);
	 void* res = run();
	 return res;
 }

 void* Runtime::run(void* arg0,void* arg1) {
 	 args(arg0);
 	 args(arg1);
 	void* res = run();
 	return res;
  }

 void* Runtime::run(void* arg0,void* arg1,void* arg2) {
  	 args(arg0);
  	 args(arg1);
  	 args(arg2);
  	void* res = run();
  	return res;
   }
 void* Runtime::run(void* arg0,void* arg1,void* arg2,void* arg3) {
   	 args(arg0);
   	 args(arg1);
   	 args(arg2);
   	 args(arg3);
   	void* res = run();
   	return res;
    }
