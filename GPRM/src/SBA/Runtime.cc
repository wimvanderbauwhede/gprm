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

//B Ashkan
double custome_time=0.0;
//E Ashkan


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
/*
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
*/
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
             if (sba.finished == 1){
                 return sba.result;
             }
         }
         cout << "Warning: GPRM ran for " <<ncycles<< " cycles without completion."<<endl;
         cout << "         Try increasing the number of cycles on the command line."<<endl;
 #else //  USE_THREADS==1
 #ifdef TIMINGS
         double start = wsecond();
 #endif
         //printf("\n Start time of the application: %f \n", start);
 #ifdef MULTIPROGRAMMING
	map_shared(); //it will be unmapped at the end of the Gateway
 #endif
	 	 sba.run_th();
 #ifdef TIMINGS
         double end = wsecond();
         //printf("\n End time of the application: %f \n", end);
         double elapsed = end - start;
				 FILE * pFile;
				 pFile=fopen("gprm_xeonphi_conv.txt","a");
				 if (pFile==NULL) {
					 printf("\nUnable to open gprm_xeonphi_conv.txt");
				 }
				 fprintf(pFile, "%f\t", end-custome_time);
				 fclose (pFile);
         cout << MAKE_RED "Function Computation Run time:\t" << end-custome_time << RESET_COLOR "\n";
         cout << MAKE_RED "Run time:\t" << elapsed << "s" RESET_COLOR "\n";
 #endif
 #endif // USE_THREADS
         return sba.result;
 	}

void* Runtime::run(const vector< void* >& argv) {
	 args(argv);

	 void* res = run();
	 return res;
}
/*
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
*/
