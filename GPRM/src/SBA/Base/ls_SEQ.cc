//#include <Timings.h>
 void Base::CoreServices::ls_SEQ() {
	 //double start = wsecond();
	 //printf("SEQ_Start: %f\n",start);
     // SEQ has only one method and does always the same thing: sequencing computations and returning the last one
 #ifdef VERBOSE
          cout << "SEQ CORE: "<<nargs()<<" args\n";
 #endif // VERBOSE
     for (uint argidx=0;argidx<nargs();argidx++) {
 		if (is_quoted_ref(argidx) ) {
 #ifdef VERBOSE
          cout << "SEQ CORE: Dispatching arg# "<<argidx<<": "<<ppSymbol(core_arg(argidx))<<"\n";
 #endif // VERBOSE
			dispatch_reference(argidx);
			break;
 		} else {
 			if (argidx==nargs()-1){ // last arg, a value, return it
 #ifdef VERBOSE
          cout << "SEQ CORE: Returning "<<ppSymbol(core_arg(argidx))<<"\n";
 #endif // VERBOSE
 				result(core_arg(argidx));
 			}
 		}
     }
 }
