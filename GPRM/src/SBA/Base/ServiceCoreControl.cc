//
// Gannet Service-based SoC project - SBA Service Core class
//
// (c) 2004-2012 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//


#include "SBA/System.h"
#include "SBA/Tile.h"
#include "Base/ServiceCoreControl.h"

using namespace std;
using namespace SBA;


void Base::ServiceCoreControl::run()  {
	SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	SBA::Tile& sba_tile=*(sba_system.nodes[address]);

        scid=sba_tile.service_manager.scid;
        sclid=sba_tile.service_manager.sclid;
        if (sba_tile.service_manager.core_status==CS_busy ){
            _current_subtask=sba_tile.service_manager.current_subtask;


#ifdef VERBOSE
            cout << "" <<service<< " Base::ServiceCoreControl: " <<sba_tile.service_manager.arg_addresses.size()<< " addresses"<<endl;
#endif // VERBOSE

            sba_tile.service_manager.core_status=CS_done;
            core_status=CS_done;

            Service service_id=sba_tile.service_manager.service_id;
            service=service_id;
            n_args=sba_tile.service_manager.n_args;
            core_status=sba_tile.service_manager.core_status;
            core_return_type=sba_tile.service_manager.core_return_type;
            ack_ok=sba_tile.service_manager.ack_ok;
            opcode=sba_tile.service_manager.opcode;
#ifdef VERBOSE
            cout << "CALL TO CORE (" <<sclid<< ") " <<scid<< " "<<endl;
#endif // VERBOSE
            //cout << "Ashkan: CALL TO CORE (" <<sclid<< ") " <<scid<< " "<<endl;
//            FuncPointer fp=sba_system.cfg.services[(sclid<<8)+scid].core;
//            (*fp)();
			select_wrapper( (sclid<<8)+scid );
#ifdef VERBOSE
        cout << "DONE CALL TO CORE"<<endl;
#endif // VERBOSE
            sba_tile.service_manager.core_return_type=core_return_type;
            sba_tile.service_manager.ack_ok=ack_ok;
            sba_tile.service_manager.n_args=n_args;
            sba_tile.service_manager.opcode=opcode;
            sba_tile.service_manager.core_status=core_status;
            if ((sba_tile.service_manager.subtask_code_fifo.size()>0) or (sba_tile.service_manager.subtask_reference_fifo.size()>0) or (sba_tile.service_manager.core_status!=CS_idle)){
              sba_tile.service_manager.status=true;
            }
#ifdef VERBOSE
            cout << "Base::ServiceCoreControl: CORE STATUS : " <<sba_tile.service_manager.core_status<< "=" <<core_status<< ""<<endl;
#endif // VERBOSE
        }
} // END of run()

void Base::ServiceCoreControl::repost_subtask(Subtask st) {
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);

#if VERBOSE==1
	 std::cout << "Base::ServiceCoreControl::repost(): REQUEUE subtask "<< st <<"\n";
#endif
       sba_tile.service_manager.pending_subtasks_fifo.push_back(st);

} // END of repost_subtask()

void Base::ServiceCoreControl::block_subtask(Subtask st) {
#if VERBOSE==1
	std::cout << "Base::ServiceCoreControl::block_subtask(): "<<st << "\n";
#endif

	SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	sba_tile.service_manager.subtask_list.status(sba_tile.service_manager.current_subtask,STS_blocked);
} // END of block_subtask()

void Base::ServiceCoreControl::reset_subtask(Subtask st) {
#if VERBOSE==1
	std::cout << "Base::ServiceCoreControl::reset_subtask(): "<<st << "\n";
#endif
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);

#if VERBOSE==1
	 std::cout << "NARGS: "<<nargs(st)<<"\n";
#endif
	 for (uint argidx=0;argidx<nargs(st);argidx++) {
#if VERBOSE==1
		 std::cout <<argidx<<"\t"<< ppSymbol( sba_tile.service_manager.subtask_list.symbol(st,argidx) ) <<"\n";
		 std::cout <<argidx<<"\t"<< (uint)(getKind(symbol(st,argidx))) <<"==K_R && "<<(uint)(getQuoted(symbol(st,argidx)))<<"==1\n";
#endif
  		if (  getKind(symbol(st,argidx))==K_R && getQuoted(symbol(st,argidx))==1) {
#if VERBOSE==1
  			std::cout << ppSymbol(symbol(st,argidx))<<" => "<<ppSymbol(arg(st,argidx) ) << "\n";
  			std::cout << "setStatus\n";
#endif
  			Word nref_symbol=setStatus(symbol(st,argidx),DS_requested);
#if VERBOSE==1
  			std::cout << "update symbol\n";
#endif
  			sba_tile.service_manager.subtask_list.symbol(st,argidx,nref_symbol);
#if VERBOSE==1
  			std::cout << "update argument\n";
#endif
  			sba_tile.service_manager.subtask_list.argument(st,argidx,symbol(st,argidx));
#if VERBOSE==1
  			std::cout << "nargs_absent("<<st<<") = ";
  			// WV: somehow this is wrong, don't now why ...
//  			sba_tile.service_manager.subtask_list.incr_nargs_absent(st);
  			std::cout <<  sba_tile.service_manager.subtask_list.nargs_absent(st) << "\n";
#endif
  		}
	 }
	sba_tile.service_manager.subtask_list.status(st,STS_blocked);

#if VERBOSE==1
	std::cout << "Base::ServiceCoreControl::reset_subtask(): "<<st << "\n";
	for (uint argidx=0;argidx<nargs(st);argidx++) {
	  			std::cout << ppSymbol(sba_tile.service_manager.subtask_list.argument(st,argidx)) << "\n";
		 }
#endif
} // END of reset_subtask()

void Base::ServiceCoreControl::block_current_subtask() {
	block_subtask(current_subtask());
}

void Base::ServiceCoreControl::reset_current_subtask() {
	reset_subtask(current_subtask());
}

 void Base::ServiceCoreControl::suspend() {
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        core_status=CS_managed;
        sba_tile.service_manager.subtask_list.status(sba_tile.service_manager.current_subtask,STS_blocked);
        sba_tile.service_manager.pending_subtasks_fifo.push_back(sba_tile.service_manager.current_subtask);
    }
    Word Base::ServiceCoreControl::iterate() {
        suspend();
        return 0;
    }
    Word Base::ServiceCoreControl::iterate(Word v) { // FIXME: can I use a template here?
        suspend();
        return v;
    }


 bool Base::ServiceCoreControl::init_state(unsigned int service_id) {
#ifdef KERNEL_HAS_STATE
 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
 if (sba_system.kernels.count(service_id)==0) {
	 // NOTE: the first slot in the pair is for a mutex lock, in case we want to have proper locking
	 // It is unused as I expect the programmer to use seq to control access.
	 	 sba_system.kernels[service_id]=std::make_pair((pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER,(void*)nullptr);
		return true;
 } else {
	 	return false;
 }
#else
	 	if (_state_register.isEmpty(service_id)) {
	 		_state_register.init(service_id);
	 		return true;
	 	} else {
	 		return false;
	 	}
#endif
}

 void Base::ServiceCoreControl::store_state(unsigned int service_id,void* state) {
#ifdef KERNEL_HAS_STATE
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 sba_system.kernels[service_id].second=state;
#else

	 _state_register.putvp(service_id,state);
#endif
    }

 void* Base::ServiceCoreControl::load_state(unsigned int service_id) {
#ifdef KERNEL_HAS_STATE
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 return sba_system.kernels[service_id].second;
#else

         return _state_register.getvp(service_id);
#endif
    }
#ifdef KERNEL_HAS_STATE
#ifdef KERNEL_LOCK
 pthread_mutex_t Base::ServiceCoreControl::get_mutex(unsigned int service_id) {
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 	 return sba_system.kernels[service_id].first;
 }
#endif
#endif

 Word_List* Base::ServiceCoreControl::unpack(Word w) {
         void* v_p = (void*)w;
	     Word_List* wl_p = (Word_List*)v_p;
         return wl_p;
    }

 /* Wim */

 Word Base::ServiceCoreControl::pack(Word_List* wl_p) {
        void* v_p=(void*)wl_p;
		Word w = (Word)v_p;
        return w;
    }

 /* Ashkan
 Word Base::ServiceCoreControl::pack(Word_List* res) {
	 //void* v = (void*) res;
	 Word w = (Word)res;
	 Word_List wl;
	 wl.at(0) = w;
	 //Uint64 temp = wl[0]; Uint64 temp2 = wl.at(0);
	 	// printf("same values? : %u,***%u\n\n\n", temp,temp2);
	 cout << "From Pack:  " << wl[0] <<endl;
	 return wl[0];
 }
*/

/*
 MemAddresses& Base::ServiceCoreControl::addresses() {
        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        return sba_tile.service_manager.arg_addresses;
    }
*/
Subtask_Argument_List& Base::ServiceCoreControl::args() {
        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        return sba_tile.service_manager.arg_addresses;
    }

// I guess this is OBSOLETE
 uint Base::ServiceCoreControl::argmode(uint argn) {
        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        return  sba_tile.service_manager.subtask_list.argmodes(sba_tile.service_manager.current_subtask)[argn] & 0x3;
    }

 Word Base::ServiceCoreControl::arg(uint argn) {
         SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
         // Ashkan: The Hack would not work for Extended values!
         //cout << "Ashkan from SC-Control calls the argument method from SM-Object and then does the UGLY HACK! "<< endl;
         //return sba_tile.service_manager.subtask_list.argument(sba_tile.service_manager.current_subtask,argn);
         return sba_tile.service_manager.subtask_list.argument(sba_tile.service_manager.current_subtask,argn) & 0x0000FFFFFFFFFFFF; //IT IS DEFINATELY CRUCIAL for 64-bit!!
     }

 // Works like arg, but doesn't omit the significant bits to keep the current code in CoreServices.cc
 Word Base::ServiceCoreControl::core_arg(uint argn) {
          SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
          SBA::Tile& sba_tile=*(sba_system.nodes[address]);
          // Ashkan: The Hack would not work for Extended values!
          //cout << "Ashkan from core_arg calls the argument method from SM-Object"<< endl;
          return sba_tile.service_manager.subtask_list.argument(sba_tile.service_manager.current_subtask,argn);
     }

 Word Base::ServiceCoreControl::symbol(uint argn) {
          SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
          SBA::Tile& sba_tile=*(sba_system.nodes[address]);
 		 return sba_tile.service_manager.subtask_list.symbol(sba_tile.service_manager.current_subtask,argn);
      }

 Word Base::ServiceCoreControl::arg(Subtask st,uint argn) {
          SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
          SBA::Tile& sba_tile=*(sba_system.nodes[address]);
 		 return sba_tile.service_manager.subtask_list.argument(st,argn);
      }


 Word Base::ServiceCoreControl::symbol(Subtask st, uint argn) {
           SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
           SBA::Tile& sba_tile=*(sba_system.nodes[address]);
  		 return sba_tile.service_manager.subtask_list.symbol(st,argn);
       }
 MemAddress Base::ServiceCoreControl::addr(uint argn) {
        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        return sba_tile.service_manager.arg_addresses[argn];
    }

 uint Base::ServiceCoreControl::nargs() {
        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        return sba_tile.service_manager.arg_addresses.size();
    }
 uint Base::ServiceCoreControl::nargs(Subtask st) {
        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
        return sba_tile.service_manager.subtask_list.nargs(st);
    }

void Base::ServiceCoreControl::result(Word_List wl) {
         SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
         sba_tile.service_manager.subtask_list.result(sba_tile.service_manager.current_subtask,wl[0]);
     }
void Base::ServiceCoreControl::result(Word w) {
         SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
         sba_tile.service_manager.subtask_list.result(sba_tile.service_manager.current_subtask,w);
     }

void Base::ServiceCoreControl::resultWord(Word w) {
         SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
         sba_tile.service_manager.subtask_list.result(sba_tile.service_manager.current_subtask,w);
     }


// void Base::ServiceCoreControl::put(MemAddress addr,Word_List wl) {
//        SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
//        SBA::Tile& sba_tile=*(sba_system.nodes[address]);
//        sba_tile.data_store.mput(addr,wl);
//    }
//
// void Base::ServiceCoreControl::put(MemAddress addr,Word w) {
//         SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
//         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
//         sba_tile.data_store.mputWord(addr,w);
// }
// Word Base::ServiceCoreControl::get(MemAddress addr) {
//         SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
//         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
//         return sba_tile.data_store.mgetWord(addr);
// }
 uint Base::ServiceCoreControl::method() {
        return opcode+(scid<< FS_SCId)+(sclid<< FS_SCLId);
    }
 // We always store the symbol
 bool Base::ServiceCoreControl::is_quoted_ref(uint idx) {
	 return (isQuoted(idx) && isRef(idx));
 }
 bool Base::ServiceCoreControl::isQuoted(uint idx) {
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	 Word nref_symbol=sba_tile.service_manager.subtask_list.argument(_current_subtask,idx);
	 return (getQuoted(nref_symbol)==1);
 }

 bool Base::ServiceCoreControl::isRef(uint idx) {
 	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
 	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
 	 Word nref_symbol=sba_tile.service_manager.subtask_list.argument(_current_subtask,idx);
 	 return (getKind(nref_symbol)==K_R);
  }

 bool Base::ServiceCoreControl::waiting() {
	// if the service in the Subtasl item has opcode "M_wait"
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	 Symbol_t sym=sba_tile.service_manager.subtask_list.called_as(sba_tile.service_manager.current_subtask);
	 return (getOpcode(sym)==M_wait);
 }
/*
 Dispatch means:
 - unquote a reference
 - wrap in a packet
 - set subtask status to "Not Ready" (or just increment nargs_absent)
 - set Core Control to CS_managed so no result packet is sent
*/
 void Base::ServiceCoreControl::dispatch_reference(uint idx) {
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	 Subtask st=current_subtask();
	Word ref_symbol=sba_tile.service_manager.subtask_list.symbol(st,idx);
	uint to=getSNId(ref_symbol);
	dispatch_reference(idx, to);
 } // END of dispatch_reference(uint idx)

 // need to refactor this to reduce the overlap!
 void Base::ServiceCoreControl::dispatch_reference(uint idx, uint dest_snid) {
 	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
 	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
 	 // Change subtask status, args etc
 	Word ref_symbol=sba_tile.service_manager.subtask_list.symbol(_current_subtask,idx);
	dispatch_reference(ref_symbol, idx, dest_snid);
 }  // END of dispatch_reference(uint idx, uint dest_snid)
/*
 	// This could just be arg(idx)!
 #if VERBOSE	== 1
 	std::cout << "Base::ServiceCoreControl::dispatch() ARG_SYMBOL: "<<ppSymbol(arg(idx))<<"\n";
 	std::cout << "Base::ServiceCoreControl::dispatch() REF_SYMBOL: "<<ppSymbol(ref_symbol)<<"\n";
 #endif
 	sba_tile.service_manager.subtask_list.incr_nargs_absent(_current_subtask);
 	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_blocked);
 	Word ncalled_as=sba_tile.service_manager.subtask_list.called_as(_current_subtask);
 	Return_to_t return_to=getSNId(ncalled_as);
 	sba_tile.service_manager.subtask_list.waiting(_current_subtask,true);
 	sba_tile.service_manager.subtask_list.called_as(_current_subtask,ncalled_as);
 	// change subtask status, required for core_control
 	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_processing);
 	// arg status is "requested" for the corresponding argument
 	Word nref_symbol=setStatus(ref_symbol,DS_requested);
 	sba_tile.service_manager.subtask_list.symbol(_current_subtask,idx,nref_symbol);
 	 // change Core status
 	core_status=CS_managed;
 	 // Create a reference packet
 	To_t to=dest_snid;
// 	getSNId(ref_symbol);
 	Word var_label = setSubtask(ref_symbol,address);
 	var_label = setName(var_label,return_to);
 	Word return_as=var_label; // SHOULD BE OBSOLETE BY NOW!
 	Packet_Type packet_type=P_reference;
 	Word subtask_argpos=arg(idx);
 #if VERBOSE	== 1
 	std::cout << "Current subtask: "<<_current_subtask<<"\n";
 	std::cout << "Arg pos: "<<idx<<"\n";
 #endif
 	subtask_argpos=setSubtask(subtask_argpos,_current_subtask);
 	subtask_argpos=setTask(subtask_argpos,idx);
 #if VERBOSE	== 1
 	std::cout << "Subtask_Argpos: "<<ppSymbol(subtask_argpos)<<"\n";
 #endif
 	Ctrl_t prio=0;
 	Redir_t redir=0;
 	Word_List reslist;
 	reslist.push_back(ref_symbol);
 	Length_t payload_length=1;
 	Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,subtask_argpos,return_as);
 	Word_List  ref_packet_payload=reslist;
 	Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
 #if VERBOSE	== 1
 	std::cout << "Base::ServiceCoreControl::dispatch(): DISPATCHING packet to "<< to <<"\n" << ppPacket(ref_packet) <<"\n";
 #endif
 	 // post it
 	if (to!=return_to){
 		sba_tile.transceiver->tx_fifo.push_back(ref_packet);
 	} else {
 		sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
 	}
  }
*/
 // need to refactor this to reduce the overlap!
 void Base::ServiceCoreControl::dispatch_reference(Symbol_t ref_symbol,uint idx, uint dest_snid) {
 	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
 	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	 // Unquote!
	 Word nref_symbol=setQuoted(ref_symbol,0);
	  	 // Change subtask status, args etc

 	sba_tile.service_manager.subtask_list.argument(_current_subtask,idx,nref_symbol);
 	// This could just be arg(idx)!
 #if VERBOSE	== 1
 	std::cout << "Base::ServiceCoreControl::dispatch() ARG_SYMBOL: "<<ppSymbol(arg(idx))<<"\n";
 	std::cout << "Base::ServiceCoreControl::dispatch() REF_SYMBOL: "<<ppSymbol(nref_symbol)<<"\n";
 #endif
 	sba_tile.service_manager.subtask_list.incr_nargs_absent(_current_subtask);
 	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_blocked);
 	Word ncalled_as=sba_tile.service_manager.subtask_list.called_as(_current_subtask);
 	Return_to_t return_to=getSNId(ncalled_as);
 	//cout << "return_to: "<<return_to << endl;
// 	sba_tile.service_manager.subtask_list.waiting(_current_subtask,true);
// 	sba_tile.service_manager.subtask_list.called_as(_current_subtask,ncalled_as);
 	// change subtask status, required for core_control
 	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_processing);
 	// arg status is "requested" for the corresponding argument
 	Word nnref_symbol=setStatus(ref_symbol,DS_requested);
 	sba_tile.service_manager.subtask_list.symbol(_current_subtask,idx,nnref_symbol);
 	 // change Core status
 	core_status=CS_managed;
 	 // Create a reference packet
 	To_t to=dest_snid;
// 	getSNId(ref_symbol);
 	Word var_label = setSubtask(ref_symbol,address);
 	var_label = setName(var_label,return_to);
 	Word return_as=var_label; // SHOULD BE OBSOLETE BY NOW!
 	Packet_Type packet_type=P_reference;
 	Word subtask_argpos=arg(idx);
 #if VERBOSE	== 1
 	std::cout << "Current subtask: "<<_current_subtask<<"\n";
 	std::cout << "Arg pos: "<<idx<<"\n";
 #endif
 	subtask_argpos=setSubtask(subtask_argpos,_current_subtask);
 	subtask_argpos=setTask(subtask_argpos,idx);
 #if VERBOSE	== 1
 	std::cout << "Subtask_Argpos: "<<ppSymbol(subtask_argpos)<<"\n";
 #endif
 	Ctrl_t prio=0;
 	Redir_t redir=0;
 	Word_List reslist;
 	reslist.push_back(ref_symbol);
 	Length_t payload_length=1;
 	Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,subtask_argpos,return_as);
 	Word_List  ref_packet_payload=reslist;
 	Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
 #if VERBOSE	== 1
 	std::cout << "Base::ServiceCoreControl::dispatch(): DISPATCHING packet to "<< to <<"\n" << ppPacket(ref_packet) <<"\n";
 #endif
 	 // post it
 	if (to!=return_to){
 		sba_tile.transceiver.tx_fifo.push_back(ref_packet);
 	} else {
 		sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
 	}
  } // END of dispatch_reference(Symbol_t ref_symbol,uint idx, uint dest_snid)

 //B Ashkan

 void Base::ServiceCoreControl::dispatch_reference_temp(uint idx) {
 	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
 	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
 	 Subtask st=current_subtask();
 	Word ref_symbol=sba_tile.service_manager.subtask_list.symbol(st,idx);
 	uint to=getSNId(ref_symbol);
 	dispatch_reference(idx, to);
  } // END of dispatch_reference(uint idx)

  // need to refactor this to reduce the overlap!
  void Base::ServiceCoreControl::dispatch_reference_temp(uint idx, uint dest_snid) {
  	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
  	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
  	 // Change subtask status, args etc
  	Word ref_symbol=sba_tile.service_manager.subtask_list.symbol(_current_subtask,idx);
 	dispatch_reference(ref_symbol, idx, dest_snid);
  }  // END of dispatch_reference(uint idx, uint dest_snid)
 /*
  	// This could just be arg(idx)!
  #if VERBOSE	== 1
  	std::cout << "Base::ServiceCoreControl::dispatch() ARG_SYMBOL: "<<ppSymbol(arg(idx))<<"\n";
  	std::cout << "Base::ServiceCoreControl::dispatch() REF_SYMBOL: "<<ppSymbol(ref_symbol)<<"\n";
  #endif
  	sba_tile.service_manager.subtask_list.incr_nargs_absent(_current_subtask);
  	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_blocked);
  	Word ncalled_as=sba_tile.service_manager.subtask_list.called_as(_current_subtask);
  	Return_to_t return_to=getSNId(ncalled_as);
  	sba_tile.service_manager.subtask_list.waiting(_current_subtask,true);
  	sba_tile.service_manager.subtask_list.called_as(_current_subtask,ncalled_as);
  	// change subtask status, required for core_control
  	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_processing);
  	// arg status is "requested" for the corresponding argument
  	Word nref_symbol=setStatus(ref_symbol,DS_requested);
  	sba_tile.service_manager.subtask_list.symbol(_current_subtask,idx,nref_symbol);
  	 // change Core status
  	core_status=CS_managed;
  	 // Create a reference packet
  	To_t to=dest_snid;
 // 	getSNId(ref_symbol);
  	Word var_label = setSubtask(ref_symbol,address);
  	var_label = setName(var_label,return_to);
  	Word return_as=var_label; // SHOULD BE OBSOLETE BY NOW!
  	Packet_Type packet_type=P_reference;
  	Word subtask_argpos=arg(idx);
  #if VERBOSE	== 1
  	std::cout << "Current subtask: "<<_current_subtask<<"\n";
  	std::cout << "Arg pos: "<<idx<<"\n";
  #endif
  	subtask_argpos=setSubtask(subtask_argpos,_current_subtask);
  	subtask_argpos=setTask(subtask_argpos,idx);
  #if VERBOSE	== 1
  	std::cout << "Subtask_Argpos: "<<ppSymbol(subtask_argpos)<<"\n";
  #endif
  	Ctrl_t prio=0;
  	Redir_t redir=0;
  	Word_List reslist;
  	reslist.push_back(ref_symbol);
  	Length_t payload_length=1;
  	Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,subtask_argpos,return_as);
  	Word_List  ref_packet_payload=reslist;
  	Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
  #if VERBOSE	== 1
  	std::cout << "Base::ServiceCoreControl::dispatch(): DISPATCHING packet to "<< to <<"\n" << ppPacket(ref_packet) <<"\n";
  #endif
  	 // post it
  	if (to!=return_to){
  		sba_tile.transceiver->tx_fifo.push_back(ref_packet);
  	} else {
  		sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
  	}
   }
 */
  // need to refactor this to reduce the overlap!
  void Base::ServiceCoreControl::dispatch_reference_temp(Symbol_t ref_symbol,uint idx, uint dest_snid) {
  	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
  	 SBA::Tile& sba_tile=*(sba_system.nodes[address]);
 	 // Unquote!
 	 Word nref_symbol=setQuoted(ref_symbol,0);
 	  	 // Change subtask status, args etc

  	sba_tile.service_manager.subtask_list.argument(_current_subtask,idx,nref_symbol);
  	// This could just be arg(idx)!
  #if VERBOSE	== 1
  	std::cout << "Base::ServiceCoreControl::dispatch() ARG_SYMBOL: "<<ppSymbol(arg(idx))<<"\n";
  	std::cout << "Base::ServiceCoreControl::dispatch() REF_SYMBOL: "<<ppSymbol(nref_symbol)<<"\n";
  #endif
  	sba_tile.service_manager.subtask_list.incr_nargs_absent(_current_subtask);
  	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_blocked);
  	Word ncalled_as=sba_tile.service_manager.subtask_list.called_as(_current_subtask);
  	Return_to_t return_to=getSNId(ncalled_as);
  	//cout << "return_to: "<<return_to << endl;
 // 	sba_tile.service_manager.subtask_list.waiting(_current_subtask,true);
 // 	sba_tile.service_manager.subtask_list.called_as(_current_subtask,ncalled_as);
  	// change subtask status, required for core_control
  	sba_tile.service_manager.subtask_list.status(_current_subtask,STS_processing);
  	// arg status is "requested" for the corresponding argument
  	Word nnref_symbol=setStatus(ref_symbol,DS_requested);
  	sba_tile.service_manager.subtask_list.symbol(_current_subtask,idx,nnref_symbol);
  	 // change Core status
  	core_status=CS_done; //TODO: This is the main change
  	 // Create a reference packet
  	To_t to=dest_snid;
 // 	getSNId(ref_symbol);
  	Word var_label = setSubtask(ref_symbol,address);
  	var_label = setName(var_label,return_to);
  	Word return_as=var_label; // SHOULD BE OBSOLETE BY NOW!
  	Packet_Type packet_type=P_reference;
  	Word subtask_argpos=arg(idx);
  #if VERBOSE	== 1
  	std::cout << "Current subtask: "<<_current_subtask<<"\n";
  	std::cout << "Arg pos: "<<idx<<"\n";
  #endif
  	subtask_argpos=setSubtask(subtask_argpos,_current_subtask);
  	subtask_argpos=setTask(subtask_argpos,idx);
  #if VERBOSE	== 1
  	std::cout << "Subtask_Argpos: "<<ppSymbol(subtask_argpos)<<"\n";
  #endif
  	Ctrl_t prio=0;
  	Redir_t redir=0;
  	Word_List reslist;
  	reslist.push_back(ref_symbol);
  	Length_t payload_length=1;
  	Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,subtask_argpos,return_as);
  	Word_List  ref_packet_payload=reslist;
  	Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
  #if VERBOSE	== 1
  	std::cout << "Base::ServiceCoreControl::dispatch(): DISPATCHING packet to "<< to <<"\n" << ppPacket(ref_packet) <<"\n";
  #endif
  	 // post it
  	if (to!=return_to){
  		sba_tile.transceiver.tx_fifo.push_back(ref_packet);
  	} else {
  		sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
  	}
   } // END of dispatch_reference(Symbol_t ref_symbol,uint idx, uint dest_snid)

 //E Ashkan



 SBA::ServiceManager& Base::ServiceCoreControl::service_manager() {
		 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
         SBA::Tile& sba_tile=*(sba_system.nodes[address]);
		 return sba_tile.service_manager;
 };

SBA::System& Base::ServiceCoreControl::system() {
		 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
		 return sba_system;
 };



// SBA::GatewayTile& Base::ServiceCoreControl::gatewaytile() {
//     System& sba_system=*((System*)sba_system_ptr);
//	    GatewayTile& gw_tile=sba_system.gw_instance;
//	    return gw_tile;
// };

Subtask Base::ServiceCoreControl::current_subtask() {
	return _current_subtask;
}
Word_List& Base::ServiceCoreControl::get_code(Word ref_symbol) {
	SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
//	SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	SBA::GatewayTile& gw_tile=sba_system.gw_instance;
    CodeAddress code_address = getSubtask(ref_symbol);
    uint code_store_id = getSCLId(ref_symbol);
//    const Word_List& code;
    if (code_store_id==0) {
    	return gw_tile.code_store.mget(code_address);
    } else {
    	return sba_system.nodes[code_store_id]->code_store.mget(code_address);
    }
//    return code;
}
void Base::ServiceCoreControl::put_code(Word ref_symbol, Word_List& code) {
	SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
//	SBA::Tile& sba_tile=*(sba_system.nodes[address]);
	SBA::GatewayTile& gw_tile=sba_system.gw_instance;
    CodeAddress code_address = getSubtask(ref_symbol);
//    uint code_store_id = getSCLId(ref_symbol);
//    if (code_store_id==0) {
    	gw_tile.code_store.mput(code_address,code);
//    } else {
//    	sba_system.nodes[code_store_id]->code_store.mput(code_address,code);
//    }
}
uint Base::ServiceCoreControl::get_snid() {
	return address;
}
