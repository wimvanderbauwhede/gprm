 void Base::CoreServices::ls_LET() {

     // Init or get state
     SBA::Base::Let* scope;
     if (init_state(SC_CoreServices_LET)) { // just initialised it, nothing there
         scope = new SBA::Base::Let;
         store_state(SC_CoreServices_LET,(void*)scope);
     } else {
         scope = (SBA::Base::Let*)load_state(SC_CoreServices_LET);
     }

     // Do the other work

         switch ( method() ) {
         case M_CoreServices_LET_let:
         {
//       case M_CoreServices_LET_lettc: // WV  not sure if I still need this
         	// If the argument is quoted, dispatch it, otherwise eval it.
         	// If it is the last argument, return it
             for (uint argidx=0;argidx<nargs();argidx++) {
         		if (isQuoted(argidx) && isRef(argidx)) {
         				dispatch_reference(argidx);
         				break;
         		} else {
         			if (argidx==nargs()-1){ // last arg, a value, return it
         				result(arg(argidx));
         				// Now that we have returned the argument, the store goes out of scope,
         				// so we can clean up
         				scope->clear();
         			}
         		}
             }
             break;
         }
         case M_CoreServices_LET_assign:
         {
             Symbol_t var = arg(0);
             Symbol_t val = arg(1);
             scope->assign(var,val);
             result(val); // we return the value, no particular reason
             while (scope->pending_reads(var)) {
                 Subtask read_st =  scope->get_pending_read(var);
#if VERBOSE==1
	 std::cout << "ls_LET() ASSIGN: REPOST READ subtask "<< read_st <<" for var "<<ppSymbol(var)<<"\n";
#endif

                 // now un-suspend it
                 /*
                  What this means is that we create a reference packet
                  The destination & subtask is the same as for the assign subtask
                  but the argument position is more difficult. In fact, the only way is to make sure the
                  original read subtask is still intact.
                  So, create a packet based on the read_st record; put it in the queue; clean up the subtask
                  This is actually not specific to read, it means that we redo the subtask
                  So, can I not simply put this subtask onto the queue?

                  */
                 repost_subtask(read_st); // WV maybe "repost" is a better name?
             }
             break;
         } // assign
         case M_CoreServices_LET_buf:
         {
        	 // buf is like assign but the argument is quoted, so we must dispatch
        	 // buf can be used with stream or read, both can be pending
             Symbol_t var = arg(0);
             if (isQuoted(1) && isRef(1)) {
            	 Subtask buf_st = current_subtask();
#if VERBOSE==1
	 std::cout << "ls_LET() BUF: RECORD & DISPATCH subtask "<< buf_st <<" for var "<<ppSymbol(var)<<"\n";
#endif
      				scope->put_subtask(var,buf_st);
      				dispatch_reference(1);
             } else {

            	 scope->assign(var,arg(1));
                 // A buf subtask should never be cleaned up!
    #if VERBOSE==1
    	 std::cout << "ls_LET() BUF: BLOCK subtask "<< current_subtask() <<"\n";
    #endif
                 block_current_subtask();
             }
             while (scope->pending_reads(var)) {
                 Subtask read_st =  scope->get_pending_read(var);
#if VERBOSE==1
	 std::cout << "ls_LET() BUF: REPOST READ subtask "<< read_st <<" for var "<<ppSymbol(var)<<"\n";
#endif
                 repost_subtask(read_st);
             }
             while (scope->pending_streams(var)) {
                 Subtask stream_st =  scope->get_pending_stream(var);
#if VERBOSE==1
	 std::cout << "ls_LET() BUF: REPOST STREAM subtask "<< stream_st <<" for var "<<ppSymbol(var)<<"\n";
#endif
	 	 	 	 repost_subtask(stream_st);
             }
        	 break;
         } // buf

         case M_CoreServices_LET_read:
         {
        	 Symbol_t var=arg(0);
             if (scope->exists(var)) {
                 Symbol_t val = scope->read(var);
                 result(val);
             } else {
            	 Subtask read_st= current_subtask();
#if VERBOSE==1
	 std::cout << "ls_LET(): DEFER READ subtask "<< read_st <<"\n";
#endif
                 scope->put_pending_read(var,read_st);
                 // now suspend this subtask
                 // This means: we must exit the core without returning a result, so change the core status
                 // with this core status, the subtask now has status STS_processing, I guess, which should be fine.
                 core_status = CS_managed;
             }
             break;
         } // read

         case M_CoreServices_LET_stream:
         {
        	 // stream performs a read, deletes the value, and reposts the corresponding buf subtask
        	 // read current value
        	 Symbol_t var=arg(0);
             if (scope->exists(var)) {
                 Symbol_t val = scope->read(var);
            	 // get the subtask
                 Subtask buf_st =scope->get_subtask(var);
#if VERBOSE==1
	 std::cout << "ls_LET(): STREAM val: "<<ppSymbol(val)<<"\n";
#endif
            	 // delete the var
                 scope->remove(var);

                 // so we must set the subtask status to something that stops the clean-up
#if VERBOSE==1
            std::cout << "ls_LET() STREAM: RESET buf subtask "<< buf_st<<"\n";
#endif
            	 reset_subtask(buf_st);

            	 // repost it
#if VERBOSE==1
	 std::cout << "ls_LET(): STREAM: REPOST buf subtask "<< buf_st <<"\n";
#endif
// problem with reposting here is that it is possible

	 	 	 	 repost_subtask(buf_st);
            	 // return the result
                 result(val);
             } else {
            	 Subtask stream_st= current_subtask();
#if VERBOSE==1
	 std::cout << "ls_LET(): DEFER STREAM subtask "<< stream_st <<"\n";
#endif
                 scope->put_pending_stream(var,stream_st);
                 // now suspend this subtask
                 // This means: we must exit the core without returning a result, so change the core status
                 // with this core status, the subtask now has status STS_processing, I guess, which should be fine.
                 core_status = CS_managed;
             }

        	 break;
         } // stream
#if VERBOSE==1
         default:
            std::cout << "ls_LET(): don't know this method(): "<< method() <<"\n";
#endif
         };
 }


