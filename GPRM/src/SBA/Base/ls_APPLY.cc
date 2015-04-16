//FIXME:
//- index of lambda is one off
//- write code directly to code store!!
// (apply 'lambda-ref arg1 ... argn)
// if quoted ref, unquote
// else pass as is
 void Base::CoreServices::ls_APPLY() {
	SBA::Base::Apply* appl;
	if (init_state(SC_CoreServices_APPLY)) {
		appl = new Apply(MAX_STATIC_SUBTASKS+MAX_DYNAMIC_SUBTASKS*get_snid(),MAX_DYNAMIC_SUBTASKS);
		store_state(SC_CoreServices_APPLY,(void*)appl);
	} else {
		appl=(SBA::Base::Apply*)load_state(SC_CoreServices_APPLY);
	}
	SBA::Base::Apply& code_address_stack=*appl;

	if (nargs() == 1){
		result(arg(0));
		// OK, we got a result, so recycle the code address!
		// code_address_stack.push(getSubtask(symbol(0)));
	} else {
			// 1. Collect arguments
//		bool no_args=false;
	Word_List lambda_function_args;
	Word_List& lambda_code=get_code(arg(0));

	if (lambda_code.size()>3   ){
		for(Word_List::iterator iter_=lambda_code.begin();iter_!=lambda_code.end();iter_++) {
			Word itemw= *iter_;
			if (getKind(itemw) == K_A){
				lambda_function_args.push_back(itemw);
			} else if (getKind(itemw)==K_R){
				break;
			}
		} // do "next"
	} else if (lambda_code.size()>2){ // (lambda 'x (...))
		lambda_function_args.push_back(lambda_code[1]);
	} else if (lambda_code.size()>1){ // (lambda (...))
		// do nothing, no args
//		no_args=true;
	} else {
		// no body, raise error
#if VERBOSE==1        
		std::cout << "ERROR in lambda code: "<<ppPayload(lambda_code)<<"\n";
#endif        
		exit(0);
	}
	/*
	- For every reference in the lambda
		- get the subtask i.e. code address
		- get a fresh one off the stack
		- map them
		*/
		Word result_symbol=NIHIL;
	    uint root_ref=1;
	    std::unordered_map<uint,uint> code_address_lookup;

	    for(Word_List::iterator iter_=lambda_code.begin();iter_!=lambda_code.end();iter_++) {
	    	Word ref_symbol_word=*iter_;
	        if(getKind(ref_symbol_word)==K_R) {
	        	uint new_code_address=code_address_stack.top();
	        	code_address_lookup[getSubtask(ref_symbol_word)]=new_code_address;

	        	code_address_stack.pop(); // FIXME: CRASH & BURN!
	        	// Create the new code reference
	        	Word new_ref_symbol=setSubtask(ref_symbol_word,new_code_address);
//	        	new_ref_symbol=setSCLId(new_ref_symbol,get_snid());
	        	// The root reference will be sent as a reference packet
	        	// The others become code packets, with the "return as" field containing this reference.
	        	// We use the SNId as the "to" field for all these packets, i.e. they are local
	        	if (root_ref==1 ){
	        		result_symbol=new_ref_symbol;
	        		root_ref=0;
	        	}
				Word_List& lambda_function_definition=get_code(ref_symbol_word);
				Word_List  appl_function;

				// So now,
				//- for every argument in the lambda, replace by the value
				//- for every ref in the lambda, replace old code address by new from lookup

				for(uint i=0;i<lambda_function_definition.size() ;i++) {
					Word symbol_word=lambda_function_definition[i];
					if ( getKind(symbol_word)==K_R){
						uint code_address=getSubtask(symbol_word);
						Word new_symbol_word=symbol_word;
						if (code_address_lookup.count(code_address)>0) {
							uint new_code_address = code_address_lookup[code_address];
							new_symbol_word=setSubtask(new_symbol_word,new_code_address);
							new_symbol_word=setSCLId(new_ref_symbol,get_snid());
						}
						appl_function.push_back(new_symbol_word);
					} else if ( getKind(symbol_word)!=K_A){
						// Neither K_A nor K_R, just copy
						appl_function.push_back(symbol_word);
					} else {
						// It's a K_A
						// Find its index, and the corresponding argument value
						// symbol_word 1 corresponds to argument value 1 etc
						for(uint j=0;j<lambda_function_args.size();j++) {
							if (setQuoted(symbol_word,0) == setQuoted(lambda_function_args[j],0)){
								Word val_word=arg(j+1); // arg(0) is the lambda ref
								appl_function.push_back(val_word);
							}
						}
					}
				}
				put_code(new_code_address,appl_function);
//				uint plength=appl_function.size();
//				Header_t code_packet_header = mkHeader(P_code,0,0,plength,0,0,0,0);
//				code_packet_header=setTo(code_packet_header,get_snid());
//				code_packet_header=setReturn_as(code_packet_header,new_ref_symbol);
//				Word_List code_packet_payload=appl_function;
//				Packet_t code_packet = mkPacket(code_packet_header,code_packet_payload);
//				// In fact, we only do local copy, so take a short cut!
////				sba_tile.transceiver.tx_fifo.push_back(code_packet);
//				SBA::ServiceManager& sm=service_manager();
//				sm.subtask_code_fifo.push_back(code_packet);
	         } // to emulate next
	    }  // of for
	    // I think we
	    // - dispatch the reference
	    // - set the core to managed just like in LET, unless it is a tail call, then we just exit.
	    dispatch_reference(result_symbol,0,get_snid());
	} // if nargs()>1

}
