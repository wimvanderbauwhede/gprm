 void Base::CoreServices::ls_ARG() {
     // ARG has only one method and does always the same thing: sequencing computations and returning the last one
 #ifdef VERBOSE
          cout << "ARG CORE\n";
 #endif // VERBOSE
	uint method_code=method();
#ifdef VERBOSE
         cout << "ARG CORE: method code = "<< method_code << "("<<M_CoreServices_ARG_write<<","<< M_CoreServices_ARG_read<<")\n";
#endif // VERBOSE
	Word res=0;
	unsigned int regno = getUInt(core_arg(0));
#ifdef VERBOSE
         cout << "ARG CORE: reg  = "<< regno << "\n";
#endif // VERBOSE

	switch (method_code) {
		case M_CoreServices_ARG_write: {
			Word val = core_arg(1);
			system().args.at(regno)=(void*)val; // &  0x0000FFFFFFFFFFFFUL);
			res=val;
			break;
		}
		case M_CoreServices_ARG_read: {
			void* vres = system().args.at(regno);
			res = (Word)vres;
			break;
		}
		default: {}
	};
 #ifdef VERBOSE
	cout << "ARG CORE: Returning "<<res<<"\n";
 #endif // VERBOSE
	result(res);
 }
