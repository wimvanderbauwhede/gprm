// FIXME: not thread-safe
void Base::CoreServices::ls_Math() {
#ifdef VERBOSE
                 cout << "MATH init() "<<endl;
#endif // VERBOSE

     Word *seed;
     if (init_state(SC_CoreServices_Math)) { // just initialised it, nothing there
    	 seed = new Word;
         *seed=random();
         store_state(SC_CoreServices_Math,(void*)seed);
     } else {
         seed = (Word*)load_state(SC_CoreServices_Math);
     }
#ifdef VERBOSE
                 cout << "MATH CORE CALL"<<endl;
#endif // VERBOSE
     
     Word res_symbol=ZERO;
     switch ( method() ) {
         case M_CoreServices_Math_rand:
             {
                 Int min_val=getInt(arg(0));
                 Int max_val=getInt(arg(1));
                 srandom(*seed);
                 Word rnd_val = random();
                 *seed=rnd_val;
                 Word res_val=min_val + max_val + (rnd_val % max_val);  
                 res_symbol=mkIntSymbol(res_val);
#ifdef VERBOSE
                 cout << "MATH rand() CORE RESULT: " <<res_val<< ""<<endl;
#endif // VERBOSE
                 break;
             }
         default:
             cerr << "Math: Operation " <<method()<< " not supported";
             exit(1);
     };
     result(res_symbol);
        
    } // of ls_Math  


