//
// :title: Gannet Service-based SoC project - Service Manager class
//
//
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
//
// ==============================================================================
//
//  Gannet Service-based SoC project - Service Manager class
//
// ==============================================================================
//

#include "System.h"
#include "Tile.h"
#include "ServiceManager.h"

#include "Schedule.h"

#include "API.h" //TODO: Ashkan: Change it!
using namespace GPRM;

#ifdef CYCLES
#include "../build/cycle.h"
#endif

using namespace std;
using namespace SBA;

#define RESET_COLOR "\e[m"
#define MAKE_GREEN "\e[32m"
#define MAKE_RED "\e[31m"
#define TASK_ERROR 1000000 //Ashkan: another hack! instead of NULL for task stealing

 void ServiceManager::run(bool steal)  {
#ifdef VERBOSE
	 std:: cout << service<<":"<<address<<"\t"<<"ServiceManager::run()\n";
#endif
        #ifdef CYCLES_DETAILED
              ticks t0=getticks();
        #endif

        #ifdef CYCLES
              ticks t1=getticks();
        #endif
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
        //printf("\n Tile Address: %d, tid: %ld \n", sba_tile.address, sba_tile.);

        #ifdef CYCLES_DETAILED
              ticks t1_1=getticks();
        #endif
        if (sba_tile.transceiver.rx_fifo.status()==1){//Ashkan: Do we really need to re-check?
            receive_packets();
        }
        #ifdef CYCLES_DETAILED
              ticks t1_2=getticks();
        #endif
        if (subtask_code_fifo.size()>0){
            store_subtask_code();
        }
//
//        if (request_fifo.size()>0){
//            dispatch_data_packets();
//        }

        #ifdef CYCLES_DETAILED
              ticks t1_3=getticks();
        #endif

        if (subtask_reference_fifo.size()>0){
            activate_subtask();
        }
        #ifdef CYCLES_DETAILED
              ticks t1_4=getticks();
        #endif

        if (data_fifo.size()>0){
            store_data();
        }
        #ifdef CYCLES_DETAILED
              ticks t1_5=getticks();
        #endif

        if (subtask_fifo.size()>0){
            parse_subtask();
        }

        /*if (migration_fifo.size()>0){ //obselete
        	//cout << "Ashkan's Debug: I am checking the migration_fifo" << endl;
        	migrate_subtask();
        }*/

        if (clean_up_fifo.size()>0){
        	//cout << "Ashkan's Debug: I am checking the clean_up_fifo" << endl;
        	clean_up();
        	//Clean_up migrated task;
        }

#ifdef CYCLES_DETAILED
	  ticks t1_6=getticks();
#endif

        core_control(steal); // core_control is Here!

#ifdef CYCLES_DETAILED
	  ticks t1_7=getticks();
#endif

#ifdef CYCLES_DETAILED
	  ticks t1_8=getticks();
#endif

#ifdef CYCLES_DETAILED
	  ticks t1_9=getticks();
#endif

#ifdef CYCLES_DETAILED
	  ticks t12=getticks();
#endif

#ifdef STEAL
	  //B Ashkan
        pthread_mutex_lock(&_subtask_lock);
        bool pending_task=(pending_subtasks_fifo.size()>0);
        pthread_mutex_unlock(&_subtask_lock);
        //E Ashkan
        //Ashkan: I made it possible in the Tile.cc to come here once again after the job is done to steal! and then we sleep on tranceiver fifo
        status = ( pending_task || (core_status != CS_idle) || ((data_fifo.size()>0) || (request_fifo.size()>0) || (subtask_reference_fifo.size()>0)) );
#else
        status = (pending_subtasks_fifo.size()>0) || (core_status != CS_idle) || ((data_fifo.size()>0) || (request_fifo.size()>0) || (subtask_reference_fifo.size()>0));
#endif
        //printf("status from service %d: %d\n", service,status);
#ifdef VERBOSE
        if (debug_all or service==debug_service){
        }
#endif // VERBOSE


         //status=true;
        #ifdef CYCLES_DETAILED
              ticks t22=getticks();
        #endif
#if MEM==1
            cout << "" <<service<< " MEM USAGE: " <<DATA_SZ-data_address_stack.size()<< ""<<endl;
#endif // MEM
        #ifdef CYCLES
              ticks t2=getticks();
        #endif // CYCLES
        #ifdef CYCLES_DETAILED
              std::cout << "CYCLES[sanity]: "<<service<<": "<< elapsed(t1,t0) <<"\n";
              std::cout << "CYCLES[tile]: "<<service<<": "<< elapsed(t1_1,t1) <<"\n";
              std::cout << "CYCLES[receive_packets]: "<<service<<": "<< elapsed(t1_2,t1_1) <<"\n";
              std::cout << "CYCLES[store_subtask_code]: "<<service<<": "<< elapsed(t1_3,t1_2) <<"\n";
              std::cout << "CYCLES[activate_subtask]: "<<service<<": "<< elapsed(t1_4,t1_3) <<"\n";
              std::cout << "CYCLES[store_data]: "<<service<<": "<< elapsed(t1_5,t1_4) <<"\n";
              std::cout << "CYCLES[parse_subtask]: "<<service<<": "<< elapsed(t1_6,t1_5) <<"\n";
              std::cout << "CYCLES[core_control]: "<<service<<": "<< elapsed(t1_7,t1_6) <<"\n";
              std::cout << "CYCLES[prepare_subtask]: "<<service<<": "<< elapsed(t1_8,t1_7) <<"\n";
//              std::cout << "CYCLES[dispatch_data_packets]: "<<service<<": "<< elapsed(t1_9,t1_8) <<"\n";
              std::cout << "CYCLES[transmit_packets]: "<<service<<": "<< elapsed(t12,t1_9) <<"\n";
              std::cout << "CYCLES[status]: "<<service<<": "<< elapsed(t22,t12) <<"\n";
        #endif // CYCLES_DETAILED
        #ifdef CYCLES
              std::cout << "CYCLES[run]: "<<service<<": "<<  elapsed(t2,t1) <<"\n";
        #endif
    } //  of run()


 void ServiceManager::demux_packets_by_type(Packet_t& packet) {
        Packet_type_t packet_type=getType(getHeader(packet));
#ifdef VERBOSE
        if (debug_all or service==debug_service){
                cout << service <<" demux_packets_by_type(): Got packet of type "<<(int)packet_type<<"\n";
        }
#endif // VERBOSE
         switch (packet_type) {
         case P_data :
         {
	    //printf(MAKE_RED "\nReceived a DATA packet\n" RESET_COLOR);
            data_fifo.push_back(packet);
          break;
         }
         case P_code :
         {
   	    //printf(MAKE_RED "\nReceived a CODE packet\n" RESET_COLOR);
            subtask_code_fifo.push_back(packet);
          break;
         }
         case P_subtask :
         {
	    //printf(MAKE_RED "\nReceived a SUBTASK packet\n" RESET_COLOR);
            subtask_code_fifo.push_back(packet);
          break;
         }
//         case P_request :
//         {
//            request_fifo.push_back(packet);
//          break;
//         }
         case P_reference :
         {
	    //printf(MAKE_RED "\nReceived a REFERENCE packet\n" RESET_COLOR);
            subtask_reference_fifo.push_back(packet);
             break;
         }
         case P_migrate :
         {
        	migration_fifo.push_back(packet);
        	break;
		 }
         case P_clean_up :
	 {
	    printf(MAKE_RED "\nASHKAN: This is a CLEANUP packet\n" RESET_COLOR);
	    clean_up_fifo.push_back(packet);
	    break;
	 }
         default:
            cerr << "Packet Type " <<packet_type<< " not recognised";
            exit(1);
        }
    } // of demux_packets_by_type


 void ServiceManager::receive_packets()  {
        #ifdef CYCLES_INT
              ticks t1=getticks();
        #endif
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
        while (sba_tile.transceiver.rx_fifo.has_packets()){
        	Packet_t rx_packet=  sba_tile.transceiver.rx_fifo.pop_front();
#ifdef VERBOSE
                cout << "" <<service<< " receive_packets(): received packet from TRX"  <<endl;
#endif // VERBOSE
            demux_packets_by_type(rx_packet);
        }
        #ifdef CYCLES_INT
              ticks t2=getticks();
              std::cout << "CYCLES_INT[receive_packets]: "<<service<<": "<< elapsed(t2,t1) <<"\n";
        #endif
    } // of receive_packets


 void ServiceManager::activate_subtask_helper(CodeAddress code_address,Name_t tservice_id,Word_List& packet,bool is_subtask_packet) {
#ifdef VERBOSE
        if (debug_all or service==debug_service){
        if ((service!=tservice_id)){
        	cout << "" <<service<< " activate_subtask_helper(): service_id " <<tservice_id<< " <> service " <<service<< ""<<endl;
        }
        }
            if (debug_all or service==debug_service){
            cout << "" <<service<< " SUBTASK STACK SIZE: " <<subtasks_address_stack.size()<< ""<<endl;
            }
#endif // VERBOSE
            if (subtasks_address_stack.size()==0){
            	std::cerr << service << " SUBTASK STACK ("<< SUBTASKS_SZ <<") OVERFLOW\n"; exit(0);
            }
            CodeAddress subtask_address=subtasks_address_stack.pop();
#if VERBOSE==1
            cout << "" <<service<< " pop " <<subtask_address<< " off SUBTASK STACK"<<endl;
            cout << "" <<service<< " NEW SUBTASK for code " <<code_address<< ": " <<subtask_address<< " " <<SUBTASKS_SZ-subtasks_address_stack.size()<< ""<<endl;
            cout << "" <<service<< " SUBTASK requested by \n" <<ppPacket(packet)<< ""<<endl;
        if (debug_all or service==debug_service){
            cout << "" <<service<< " activate_subtask_helper(): address: " <<subtask_address<< ""<<endl;
        }
#endif // VERBOSE
        if (is_subtask_packet){
        	subtask_list.add(subtask_address);
        }

            Word subtask_word=mkSubtaskCodeAddressTup(subtask_address,code_address);
#if VERBOSE==1
        cout << "service" <<service<< ", activate_subtask_helper(), subtask_address:<" <<subtask_address<< ">"<<endl;
        cout << "service" <<service<< ": activate_subtask_helper(), code_address: <" <<code_address<< ">"<<endl;
        cout << "service" <<service<< ": activate_subtask_helper(), tuple of the aboves in hex (subtask_word): <0x" << hex <<subtask_word<< dec<<">"<<endl;
        cout << ppPacket(packet)<<endl;
#endif // VERBOSE
            subtask_fifo.push_back(subtask_word);
#if VERBOSE==1
         cout << "Ashkan: subtask_word: 0x" << hex <<subtask_word<< dec<< " has pushed into the subtask_fifo. parse_subtask() will be activated" <<endl;
#endif
        if (is_subtask_packet){ // or reference packet!
            subtask_list.return_as(subtask_address,getReturn_as_p(packet));
            subtask_list.return_to(subtask_address,getReturn_to_p(packet));
            subtask_list.to(subtask_address,getReturn_to_p(packet));
            subtask_list.redir(subtask_address, getRedir_p(packet));

            subtask_list.ack_to(subtask_address, getAck_to_p(packet));
            subtask_list.code_address(subtask_address,code_address);
            // This was unused I think, I'll use it to store the address of the code store
            subtask_list.service_id(subtask_address,tservice_id);
            service_id=tservice_id;
        }
    } // of activate_subtask_helper



 void ServiceManager::store_subtask_code()  {
        #ifdef CYCLES_INT
              ticks t1=getticks();
        #endif
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "\n" <<service<< " store_subtask_code()"<<endl;
        }
#endif // VERBOSE

        uint fifo_len = subtask_code_fifo.size();
        for(uint i=1;i<=fifo_len ;i++) {
            Packet_t subtask_code_packet= subtask_code_fifo.front();subtask_code_fifo.pop_front();
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_subtask_code(): packet:"<<endl;
                cout << ppPacket(subtask_code_packet)<<endl;
            }
#endif // VERBOSE

            Word_List subtask_code=getPayload(subtask_code_packet);
/*
Now, what we can do as a temporary measure:
- we introduce a constants store
- any extended symbol gets stored in this constants store
- the value field becomes the address in the constants store, i.e. the pointer
- what we store there must be a string of bytes, i.e. vector<uint8_t>

- the complication is that I want to be able to store extended symbols as return values,
e.g. a 64-bit number must be extended, so I have to put it somewhere and return a pointer.
The simplest way seems to be that we malloc some space and return that pointer.
*/
	Word_List n_subtask_code;
	int extidx = 0;
	uint nwords =0;
	Word* extsym;
	for (Word_List::iterator _iter=subtask_code.begin(); _iter!=subtask_code.end();_iter++) {
		Word symbol = *_iter;
		if (extidx==0 && getExt(symbol)>0) { // OK, extended symbol
			nwords = getNSymbols(symbol); // number of extension words
			uint npadbytes = getNPadBytes(symbol); // padding
			extidx=nwords;
			// Allocate memory
			extsym = new Word[nwords+2];
			extsym[0]=nwords;
			extsym[1]=npadbytes;
			void* vp=(void*)extsym;
			Word vpw = (Word)vp;
			symbol=setExtValue(symbol,vpw);
			n_subtask_code.push_back(symbol);
		} else {
			if (extidx>0) {
				extsym[2+nwords-extidx]=symbol;
				extidx--;
			} else {
				n_subtask_code.push_back(symbol);
			}
		}
	}


#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "subtask code:"<<endl;
                cout << ppPayload(subtask_code)<<endl;
                cout << "subtask code after removal of extended symbol values:"<<endl;
                cout << ppPayload(n_subtask_code)<<endl;
            }

#endif // VERBOSE
            Word code_label= subtask_code_packet[2];
            CodeAddress code_address=getCodeAddress(code_label);
            Name_t tservice_id=getName(code_label);
            code_status[code_address]=(code_status[code_address]==1)?1:0;
            sba_tile.code_store.mput(code_address,n_subtask_code);
            code_status[code_address]=code_status[code_address]|2;

            bool is_subtask_packet = (getPacket_type(getHeader(subtask_code_packet))==P_subtask);
            if (is_subtask_packet or (code_status[code_address]&1)==1){
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " store_subtask_code(): ACTIVATE " <<code_address<< " NOW!" <<endl;
                }
#endif // VERBOSE
                code_status[code_address]=code_status[code_address]&2;
                activate_subtask_helper(code_address,tservice_id,subtask_code_packet,is_subtask_packet);
            }
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_subtask_code(): stored " <<code_label<< " at " <<code_address<< "" <<endl;
                //cout << ppPayload(sba_tile.code_store.mget(code_address))<<endl;
            }
#endif // VERBOSE
        }
#ifdef VERBOSE
#endif // VERBOSE
        #ifdef CYCLES_INT
              ticks t2=getticks();
              std::cout << "CYCLES_INT[store_subtask_code]: "<<service<<": "<< elapsed(t2,t1) <<"\n";
        #endif
    } // of store_subtask_code()


 void ServiceManager::activate_subtask()  {
        #ifdef CYCLES_INT
              ticks t1=getticks();
        #endif

#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " activate_subtask()"<<endl;
        }
#endif // VERBOSE
        while (subtask_reference_fifo.size()>0){
            Packet_t subtask_ref_packet= subtask_reference_fifo.front();subtask_reference_fifo.pop_front();
#ifdef VERBOSE
            cout << "Service: " <<service<< " : REF PACKET: \n";
            cout << ppPacket(subtask_ref_packet)<<endl;
#endif // VERBOSE
            Word_List ref_label_symbol_l=getPayload(subtask_ref_packet);
            Word ref_label_symbol=ref_label_symbol_l[0];

#ifdef VERBOSE
            cout << "Service: " <<service<< " : REF_LABEL: " <<ref_label_symbol<< ""<<endl;
            cout << "Service: " <<service<< " : REF_LABEL: " <<ppSymbol(ref_label_symbol)<<endl;
            cout << "Service: " <<service<< " : SNId: " <<(int)getSNId(ref_label_symbol)<<endl;
#endif // VERBOSE
           // printf("Ashkan- Service: %d : SNId: %d\n",service,(int)getSNId(ref_label_symbol));
            CodeAddress code_address=getCodeAddress(ref_label_symbol);
//            Name_t tservice_id=getName(ref_label_symbol);
//            uint csid = getSCLId(ref_label_symbol);
//            code_address=setSNId(code_address, csid);
            Name_t tservice_id=getSNId(ref_label_symbol);
            activate_subtask_helper(code_address,tservice_id,subtask_ref_packet,true);
        } // of while
        #ifdef CYCLES_INT
              ticks t2=getticks();
              std::cout << "CYCLES_INT[activate_subtask]: "<<service<<": "<< elapsed(t2,t1) <<"\n";
        #endif
    } // of activate_subtask()


 void ServiceManager::store_data()  {
//        System& sba_system=*((System*)sba_system_ptr);
//        Tile& sba_tile=*(sba_system.nodes[address]);
#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " store_data()"<<endl;
        }
#endif // VERBOSE
        while (data_fifo.size()>0){
            Packet_t data_packet= data_fifo.front();data_fifo.pop_front();
            uint ctrl=getCtrl_p(data_packet);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
            cout <<  "" <<service<< " store_data(): Ctrl=" <<ctrl<< ""<<endl;
            if ((ctrl&1)==1){
                cout <<  "" <<service<< " store_data(): got ACK"<<endl;
            }
            cout << ppPacket(data_packet);
            }
#endif // VERBOSE

//            Word label= getReturn_as(getHeader(data_packet));
            Header_t data_packet_header = getHeader(data_packet);
            Word subtask_argpos = getAck_to(data_packet_header);
            Subtask subtask=getSubtask(subtask_argpos);
            //cout << "Ashkan: This is the Subtask: " << subtask<<endl;
            uint argpos=getTask(subtask_argpos);
            //cout << "Ashkan: This is the Argument position: " << argpos<<endl;
//if (subtask_list.exists(subtask)) {
            MemAddress data_address=subtask_list.argument(subtask,argpos);

//            MemAddress data_address=getDataAddress(label);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() ADDRESS: " <<hex<<data_address<<dec<< ""<<endl;
            }
#endif // VERBOSE

//            Word data_symbol=symbol_table[data_address];
            Word data_symbol=subtask_list.symbol(subtask,argpos);
            //cout << "Ashkan: data_symbol: "<< hex<<data_symbol<<dec<<endl;
            uint data_status=getStatus(data_symbol);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " store_data() STATUS: ";
                cout << data_status<<endl;
            }
#endif // VERBOSE

            if (data_status!=DS_present and data_status!=DS_eos){


                //Payload_t data_packet_payload=getPayload(data_packet);
            	Word data_packet_payload=getPayload_Word(data_packet);
            	//if(getKind(data_packet_payload) == K_P) data_packet_payload=(Word)getPointer(data_packet_payload);
#if VERBOSE==1
            	cout << "" <<service<< " store_data() DATA: "<<ppSymbol(data_packet_payload) <<"\n";
#endif
            	//cout << "Ashkan says DATA:" << hex << data_packet_payload << endl;
            	/*
                uint fsize=0;
                if ((data_address<DATA_OF+NREGS)){
                    fsize=register_set[data_address].fsize;
                }
                if (fsize==0){
*/
                    //sba_tile.data_store.mput(data_address,data_packet_payload);
//                	sba_tile.data_store.mputWord(data_address,data_packet_payload);
            	//cout << "Ashkan:ServiceManager goes to SMObject and the arg based on this data_packet_payload: " << hex << data_packet_payload << dec << endl;
            	subtask_list.argument(subtask,argpos,data_packet_payload);
/*
                } else {
                	// WV: OBSOLETE!
                    uint offset=register_set[data_address].offset;
                    //Word_List current_content = sba_tile.data_store.mget(data_address);
                    //TODO: Remove offset and fsize
                    Word current_content_Word = sba_tile.data_store.mgetWord(data_address);
                    for(uint i=0 ;i<= fsize-1 ;i++) {

                        #ifdef STATIC_ALLOC
                         current_content.at(i+offset,data_packet_payload[i]);
                        #else
                        //current_content[i+offset]=data_packet_payload[i];
                         current_content_Word=data_packet_payload;
                        #endif
                    }
                    //sba_tile.data_store.mput(data_address,current_content);
                    sba_tile.data_store.mputWord(data_address,current_content_Word);
                }
*/
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " store_data() address " <<hex<<data_address<<dec<< " STATUS: " <<data_status<< ""<<endl;
                }
#endif // VERBOSE

//                Subtask subtask=getSubtask(data_symbol);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " store_data() SUBTASK: " <<subtask<< ""<<endl;
                }
#endif // VERBOSE


                if ((data_address<DATA_OF+NREGS)){
                    register_set[data_address].status=RDS_present;
                }

                uint skip=0;
                uint eos=0;
                uint eosctrl=ctrl&6;
                if (eosctrl==6){
                    skip=1;
                } else if (eosctrl==2){
                    eos=1;
//                    symbol_table[data_address]=setStatus(data_symbol,DS_eos);
                    subtask_list.symbol(subtask,argpos,setStatus(data_symbol,DS_eos));
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() EOS on " <<hex<<data_address<<dec<< ""<<endl;
                    }
#endif // VERBOSE
                } else {
//                    symbol_table[data_address]=setStatus(data_symbol,DS_present);
                    subtask_list.symbol(subtask,argpos,setStatus(data_symbol,DS_eos));
                }

                if (subtask_list.status(subtask)!=STS_deleted){
                    subtask_list.decr_nargs_absent(subtask);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " in list"<<endl;
                        cout << "" <<service<< " store_data() nargs_absent=" <<subtask_list.nargs_absent(subtask)<< " eos=" <<eos<< " skip=" <<skip<< ""<<endl;
                    }
#endif // VERBOSE
                    if (subtask_list.nargs_absent(subtask)==0 or eos==1 or skip==1){
                        if (eos==0 and skip==0){
                            subtask_list.status(subtask,STS_pending);
                        } else if (eos==1){
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " store_data() EOS: setting subtask status from " <<subtask_list.status(subtask)<< " to STS_eos"<<endl;
                            }
#endif // VERBOSE
                            subtask_list.status(subtask,STS_eos);
                        } else if (skip==1){
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " store_data() SKIP: setting subtask status from " <<subtask_list.status(subtask)<< " to STS_skip"<<endl;
                            }
#endif // VERBOSE
                            subtask_list.status(subtask,STS_skip);
                        }
/*#ifdef STEAL
			pthread_mutex_lock(&_subtask_lock); // although we are poping from the other side, this has to be protected
			pending_subtasks_fifo.push_back(subtask);
			pthread_mutex_unlock(&_subtask_lock);
#else*/
                        pending_subtasks_fifo.push_back(subtask);
//#endif

#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " pushed onto pending_subtasks_fifo"<<endl;
                        }
#endif // VERBOSE
                    } else {
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " store_data() SUBTASK " <<subtask<< " still missing " <<subtask_list.nargs_absent(subtask)<< " arg(s)"<<endl;
                        }
#endif // VERBOSE
                    }
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " store_data() done for " <<hex<<data_address<<dec<< " of subtask " <<subtask<< "" <<endl;
                    }
#endif // VERBOSE
                }
                // Set target for migration based on source of data packet
                // The idea is to move the parent to the node of the last child to return
                //Service from = getReturn_to_p(data_packet); // FIXME: This is wrong!!
                /*
                 * Hack for service 1, SEQ mainly // 1 would be from the yml file (which would mean only control services).
                   This parent service is not migrated yet. So it cannot be a service with another ID which is migrated to 1
                 */
                if(service != 1) {
					//Word h=data_packet_header[1];
					//Service from = (uint)getSNId(h);
                	Service from = getReturn_to_p(data_packet);
					//cout << ppPacket(data_packet)<<endl;
					//printf("target is set for service: %d to %d\n",service,from);
					subtask_list.set_target(subtask,from);
                }
            }
//}
        } // of while
    } // of store_data

// This is to handle requests for K_L
// We need the data store for K_L so we need to keep the symbol table etc.
// Unless of course we use READ instead. With the current state of the VM, we can repost tasks
// So I think we don't need to use request packets etc.
 /*
 void ServiceManager::dispatch_data_packets()  {
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " dispatch_data_packets()"<<endl;
        }
#endif // VERBOSE
        Packet_Fifo pending_requests_fifo;
        while (request_fifo.size()>0){
            Packet_t request= request_fifo.front();request_fifo.pop_front();
            Word packet_label= getReturn_as(getHeader(request));
            Word var_label=getPayload(request)[0];
            uint offset=0;
            uint fsize=0;
            if (getExt(var_label)==1){
                Word var_label_ext=getPayload(request)[1];
                offset=getOffset(var_label_ext);
                fsize=getSize(var_label_ext);
            }

            MemAddress data_address=0;
            bool has_label=0;
            uint data_status=DS_absent;
             uint mode;
            uint eosreq=2;
            uint eos=0;
            uint skip=0;
            if (getKind(var_label)==K_D){
                uint reg_address=getReg(var_label);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: symbol: " <<var_label<< " reg address: " <<reg_address<< ""<<endl;
                }
#endif // VERBOSE
                data_address=reg_address;
                mode=getMode(var_label);
                Word reg_symbol=symbol_table[reg_address];

                data_status=getStatus(reg_symbol);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: reg address: " <<reg_address<< " mode " <<mode<< " status " <<data_status<< ""<<endl;
                }
#endif // VERBOSE
                if (mode==M_eos){
#ifdef VERBOSE
                    uint ds_eos=DS_eos;
                    cout << "" <<service<< " dispatch_data_packets(): EOS REQUEST " <<reg_address<< ": " <<data_status<< "<>" <<ds_eos<< " " <<(data_status==DS_eos)<< ""<<endl;
#endif // VERBOSE
                    has_label=1;
                    if (data_status==DS_eos){
                        eosreq=1;
                    } else {
                        eosreq=0;
                    }
                } else {

                    if (data_status==DS_present){
                        has_label=1;
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: reg address " <<reg_address<< " status DS_present"<<endl;
                        }
#endif // VERBOSE

                        if (mode==M_stream){
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " dispatch_data_packets(): Mode: STREAM: " <<reg_address<< " (" <<register_set[reg_address].data_address<< ")"<<endl;
                            }
#endif // VERBOSE
                            symbol_table[reg_address]=setStatus(reg_symbol,DS_requested);
                            restart_subtask(reg_address);
                        }
                    } else if (data_status==DS_eos){
                        eos=1;
                        has_label=1;
#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: reg address " <<reg_address<< " status DS_eos"<<endl;
                        }
#endif // VERBOSE
                    } else if (data_status==DS_requested or data_status==DS_absent){
                        if (mode==M_stream){
                            skip=1;
                            has_label=1;
                        } else {
                            request_table.push(reg_address,packet_label);
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< " dispatch_data_packets(): Mode: CACHE: queue request for reg address " <<reg_address<< " (" <<register_set[reg_address].data_address<< ")"<<endl;
                            }
#endif // VERBOSE
                        }
                    }

                }

            } else if (getKind(var_label)==K_L){
                if (sba_tile.lookup_table.count(getName(var_label))==1){
                    has_label=1;
                    Word word=sba_tile.lookup_table.read(getName(var_label));
                    data_address= getSubtask(word);
                     data_status=(Data_Status)getStatus(word);
                }
            } else {
                cerr << "WHY " <<getKind(var_label)<< "?";
                exit(1);
                data_address=getSubtask(var_label);
            }

            if (has_label==1 ){

                Length_t payload_length=0;
                 Word_List tdata;
                uint ctrl=0;
                if (eosreq==2 and eos==0 and skip==0 ){
                    tdata=getField(sba_tile.data_store.mget(data_address),offset,fsize);
                } else if (eosreq!=2){
                    Bool_t tdatasym = mkBool(eosreq);
#ifdef VERBOSE
                    cout << "" <<service<< " dispatch_data_packets(): EOS REQUEST: " <<ppSymbol(tdatasym)<< " (" <<eosreq<< ")"<<endl;
#endif // VERBOSE
                    tdata.push_back(tdatasym);
                } else if (eos==1){
#ifdef VERBOSE
                    cout << "" <<service<< " dispatch_data_packets(): data status for " <<data_address<< " is EOS"<<endl;
#endif // VERBOSE
                    ctrl=2;
                } else if (skip==1){
#ifdef VERBOSE
                    cout << "" <<service<< " dispatch_data_packets(): data status for " <<data_address<< " is SKIP"<<endl;
#endif // VERBOSE
                    ctrl=6;
                }
                payload_length=tdata.size();

                Header_t packet_header = mkHeader(P_data,ctrl,0,payload_length, getReturn_to(getHeader(request)), NA,0,packet_label);
                Word_List packet_payload=tdata;
                Packet_t packet = mkPacket(packet_header,packet_payload);
#ifdef VERBOSE
#endif // VERBOSE

                if (getTo(packet_header) != service){
                    //sba_tile.transceiver.tx_fifo.push_back(packet);
                	sba_tile.transceiver->tx_fifo.push_back(packet);
                } else {
                    data_fifo.push_back(packet);
                }

            } else {
                #if SYSC_FIXME==0
                if (getKind(var_label)==K_L){
                    pending_requests_fifo.push_back(request);
                }
                #endif // SYSC_FIXME
            } // of request there or not
        } // of while
        #if SYSC_FIXME==0
        while (pending_requests_fifo.size()>0){
            Packet_t pending_request =   pending_requests_fifo.front();  pending_requests_fifo.pop_front();
            request_fifo.unshift(pending_request);
        }
        #endif // SYSC_FIXME
    } // of dispatch_data_packets
*/

 void ServiceManager::parse_subtask()  {
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
	    GatewayTile& gw_tile=sba_system.gw_instance;
        while (subtask_fifo.size()>0){
             Word subtask_word=(Word)subtask_fifo.front();subtask_fifo.pop_front();

                Subtask parent_subtask=getSubtaskFromTup(subtask_word);
//                uint csid = getSNIdFromTup(subtask_word);
                MemAddress code_address= getCodeAddressFromTup(subtask_word);

#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): parsing " <<parent_subtask<< " (" <<code_address<< ")"<<endl;
            }
#endif // VERBOSE

            Word_List subtask=sba_tile.code_store.mget(code_address);
//            if (csid==0) {
            	subtask=gw_tile.code_store.mget(code_address);
#if VERBOSE==1
//            	cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): code:\n"<<ppPayload(subtask)<<endl;
#endif

//            } else {
//            	subtask=sba_system.nodes[csid]->code_store.mget(code_address);
//            }

            Word service_symbol=subtask.front(); subtask.pop_front();

            if (getExt(service_symbol)==1){
                 Word service_symbol_ext=subtask.front(); subtask.pop_front();
                uint offset=getOffset(service_symbol_ext);
                uint fsize=getSize(service_symbol_ext);
                subtask_list.offset(parent_subtask,offset);
                subtask_list.fsize(parent_subtask,fsize);
            }
            subtask_list.called_as(parent_subtask,service_symbol);
            subtask_list.nargs(parent_subtask,getNArgs(service_symbol));
            uint nargs_absent=getNArgs(service_symbol);
            subtask_list.nargs_absent(parent_subtask,nargs_absent);

            uint mode=getMode(service_symbol);
             uint reg_addr=0;
            if (mode!=M_normal ){ // STREAMING
                reg_addr=getReg(service_symbol);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(): Mode " <<mode<< ": reg " <<reg_addr<< ""<<endl;
                }
#endif // VERBOSE
                subtask_list.mode(parent_subtask,mode);
                subtask_list.reg(parent_subtask,reg_addr);

                if ( register_set[reg_addr].status==RDS_absent){
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "parse_subtask(): Mode " <<mode<< ": reg " <<reg_addr<< ""<<endl;
                    }
#endif // VERBOSE
                     register_set[reg_addr].data_address=reg_addr;register_set[reg_addr].code_address=code_address;register_set[reg_addr].subtask_address=parent_subtask;
                }
            } else {
//#ifndef NO_DATA_ADDRESS_STACK
                if (subtask_list.result_address(parent_subtask)==0){
                    if (data_address_stack.size()==0){
                        cerr << "" <<service<< " parse_subtask(" <<parent_subtask<< "): ADDRESS STACK (" <<DATA_SZ<< ") OVERFLOW for subtask " <<parent_subtask<< "";
                        exit(1);
                    }
                    subtask_list.result_address(parent_subtask,data_address_stack.pop());
                }
//#endif
            }
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): NARGS:" <<subtask_list.nargs(parent_subtask)<< ""<<endl;
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): Subtask length: " <<subtask.size()<< ""<<endl;
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): stack size " <<data_address_stack.size()<< ""<<endl;
            }
#endif // VERBOSE


            uint argidx=0;
            while (subtask.size()>0){
                 Word elt=subtask.front(); subtask.pop_front();
//                if (data_address_stack.size()==0){
//                    cerr << "" <<service<< " parse_subtask(" <<parent_subtask<< "): ADDRESS STACK (" <<DATA_SZ<< ") OVERFLOW for subtask " <<parent_subtask<< "";
//                    exit(1);
//                }
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): STATUS: " <<subtask_list.status(parent_subtask)<< ""<<endl;
                }
#endif // VERBOSE

                MemAddress data_address;
//                uint pass_by_value = 0;
//                unsigned char argmode=0;
                if (getKind(elt)!=K_C){
//                	// Pass by value: either it's quoted, or not. If not, the status should be "requested"; the mode is obsolete
//                    if ((getKind(elt)==K_B && getExt(elt)==0) || (getQuoted(elt)==1 && getExt(elt)==0 && getKind(elt)!=K_R)){
//                        argmode=1;
//                        pass_by_value = 1;
//                    } else if (getKind(elt)==K_B && getExt(elt)==1 && getNSymbols(elt)==1){
//                        argmode=2;
//                        pass_by_value = 1;
//                    }
//                    if (pass_by_value==0                        ){
//                        data_address=data_address_stack.pop();
//                    }
                } else {
                    data_address= getReg(elt);
                    if (getExt(elt)==1){
                        Word elt_ext=subtask.front(); subtask.pop_front();
                        uint offset=getOffset(elt_ext);
                        uint fsize=getSize(elt_ext);
                        register_set[data_address].offset=offset;
                        register_set[data_address].fsize=fsize;
                    }
                }
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): data address " <<data_address<< " for " <<ppSymbol(elt)<< ""<<endl;
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
                }
#endif // VERBOSE
                Word arg_symbol=elt;
                arg_symbol=setSubtask(arg_symbol,parent_subtask);
                arg_symbol=setStatus(arg_symbol,DS_absent);
				// WV 27dec2012 new approach, storing subtask+argpos in packet (using ack_to) & arg status in Subtask_List_Item
                // Store in packet
                Word subtask_argpos=elt;
                subtask_argpos=setSubtask(subtask_argpos,parent_subtask);
                subtask_argpos=setTask(subtask_argpos,argidx);
                // Store in Subtask_List_Item. This is step 1; in step 2 we will totally remove the data store!
				Word arg_symbol_status=elt;
				arg_symbol_status=setTask(arg_symbol_status,DS_absent);

                #ifndef STATIC_ALLOC
//                if (argmode==0 or pass_by_value==0){
//                    subtask_list.arguments(parent_subtask).push_back(data_address);
//                } else if (argmode==1 and pass_by_value==1){
                    subtask_list.arguments(parent_subtask).push_back(elt);
//                } else if (argmode==2 and pass_by_value==1){
//                    cerr << "BOOM!";
//                    exit(1);
//                    subtask_list.arguments(parent_subtask).push_back(subtask.shift());
//                }
                // argmode encodes the Kind and Datatype as well as the original "mode", in 8 bits
                // with the new "symbols" entry this actually redundant
//                argmode+=((getKind(elt)&0x7)<<5)+((getDatatype(elt)&0x7)<<2);
//				subtask_list.argmodes(parent_subtask,argidx,argmode);
                #else
				subtask_list.arguments(parent_subtask)[argidx]=data_address;
                #endif
                argidx=argidx+1;
                if ( getQuoted(elt)==0) { // NOT QUOTED
                    Word var_label = elt;
                    var_label = setName(var_label,service);
                    var_label = setSubtask(var_label,data_address);
                    arg_symbol=setStatus(arg_symbol,DS_requested);
                    arg_symbol_status=setTask(arg_symbol_status,DS_requested);
//                    symbol_table[data_address]=arg_symbol;
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): LABEL for " <<ppSymbol(elt)<< " is " <<ppSymbol(arg_symbol_status)<< ""<<endl;
                    }
#endif // VERBOSE
                    uint store=getSNId(elt); // the Service Node ID
                    Packet_type_t packet_type=P_reference;

                    if (getKind(elt)== K_L){
                        packet_type=P_request;
                    } else if (getKind(elt)== K_D){
                        packet_type=P_request;
#ifdef VERBOSE
	if (debug_all or service==debug_service){
		cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): Mode: K_D REQ: subtask " <<getSubtask(elt)<< ""<<endl;
		cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): Mode: K_D REQ (" <<getMode(elt)<< ") from reg " <<reg_addr<< ""<<endl;
	}
#endif // VERBOSE
                    } else if (getKind(elt)== K_A ){
                    }

                    Length_t payload_length=1;
                    Ctrl_t ctrl=0;
                    Redir_t redir=0;
//                    Word ack_to=0;
                    Header_t requestpacket_header = mkHeader(packet_type,ctrl,redir,payload_length,store,service,subtask_argpos,var_label);
                    // There is a problem here: If the code is from APPLY, SNId should not be 0
                    // But currently SNId is used to know where to send the reference packet
                    // If we change it to 0 in the compiler, we would no longer support that model
                    // OTOH, we don't need the SCLId etc fields, so I could use those
                     Word_List requestpacket_payload; requestpacket_payload.push_back(elt);//setSCLId(elt,0)); // FIXME!
                    Packet_t request_packet = mkPacket(requestpacket_header,requestpacket_payload);

					// FIXME!
					// We need to compare the node address with the SNId
					// Rather ad-hoc, I use a mapping ((snid-1)%NSERVICES)+1
					// So if NSERVICES is large enough, it should not matter.
                    if (store != service){
                        sba_tile.transceiver.tx_fifo.push_back(request_packet);
#ifdef VERBOSE
#endif // VERBOSE
                    } else {
#ifdef VERBOSE
#endif // VERBOSE
                        demux_packets_by_type(request_packet);
                    }


                } else {
                	/*
                    if (pass_by_value==0){ // OBSOLETE
                         Word_List* elt_val_p = new Word_List;
                         Word_List& elt_val = *elt_val_p;
                         elt_val.push_back(elt);
                        if ((getKind(elt) == K_B)){
                            if (getExt(elt)==1){ // OBSOLETE!
#ifdef VERBOSE
                                if (debug_all or service==debug_service){
                                    cout << "Quoted Extended Builtin"<<endl;
                                }
#endif // VERBOSE
                                if (getNSymbols(elt)==1                                    ){
                                    elt_val.push_back(subtask.shift());
                                } else {
                                     for (int i=0;i<getNSymbols(elt);i++) {
                                         Word elt_sym=subtask.front(); subtask.pop_front();
                                        elt_val.push_back(elt_sym);
                                    }
                                }
                            } else {
                            }
                        } else {

                        }

#ifdef VERBOSE
if (debug_all or service==debug_service){
                        cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): storing " <<ppPayload(elt_val)<< " (" <<elt_val[0]<< ") in address " <<data_address<< ""<<endl;
}
#endif // VERBOSE
                        sba_tile.data_store.mput(data_address,elt_val);

                        arg_symbol=setStatus(arg_symbol,DS_present);
                        arg_symbol_status=setTask(arg_symbol_status,DS_present);
                        symbol_table[data_address]=arg_symbol;

#ifdef VERBOSE
                        if (debug_all or service==debug_service){
                            cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): LABEL for " <<ppSymbol(elt)<< " is " <<ppSymbol(symbol_table[data_address])<< ""<<endl;
                        }
#endif // VERBOSE
                    } // of not pass_by_value
                    */
                    subtask_list.decr_nargs_absent(parent_subtask);
                    nargs_absent=nargs_absent-1;
                } // of quoted or not
                subtask_list.symbols(parent_subtask).push_back(arg_symbol_status);
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): subtask length is " <<subtask.size()<< "; status is " <<subtask_list.status(parent_subtask)<< ""<<endl;
                }
#endif // VERBOSE
            }

            if ( subtask_list.status(parent_subtask)==STS_new and subtask_list.nargs_absent(parent_subtask)==0){
                subtask_list.status(parent_subtask,STS_pending);
/*#ifdef STEAL
		pthread_mutex_lock(&_subtask_lock); // although we are poping from the other side, this has to be protected
                pending_subtasks_fifo.push_back(parent_subtask);
                pthread_mutex_unlock(&_subtask_lock);
#else*/
                pending_subtasks_fifo.push_back(parent_subtask);
//#endif
            }
/*#ifdef LAUNCHER
            if(service != 1 && subtask_list.status(parent_subtask)== STS_pending) { //Ashkan: HACK for the SEQ service (simply ignore it!)
            	printf("Subtask is pending in service: %d***\n",service);
            	pin_target(service);
            }
#endif*/
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): subtask status " <<subtask_list.status(parent_subtask)<< ""<<endl;
                cout << "" <<service<< " parse_subtask(" <<parent_subtask<< "): end parsing " <<parent_subtask<< "" <<endl;
            }
#endif // VERBOSE
        } // of while subtask fifo not empty
    } // of parse_subtask




// WV: Should be obsolete I guess
/*
    #ifndef STATIC_ALLOC
     MemAddresses ServiceManager::build_value_address_list() {
        MemAddresses  addresses;
        uint nargs=subtask_list.nargs(current_subtask);
#ifdef VERBOSE
            cout << "" <<service<< " build_value_address_list(): " <<nargs<< "<>" <<subtask_list.arguments(current_subtask).size()<< ""<<endl;
#endif // VERBOSE

        if (nargs>0){
            Subtask_Argument_List& args=subtask_list.arguments(current_subtask);
            for(Subtask_Argument_List::iterator iter_=args.begin();iter_!=args.end();iter_++) {
            	MemAddress address=(MemAddress)(*iter_);
                addresses.push_back(address);
                nargs=nargs-1;
                if (nargs==0){
                    break;
                }
            }
        }
        return addresses;
    } // of build_value_address_list
    #endif
*/


/*
 void ServiceManager::send_ack()  {
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " ACK: send_ack()"<<endl;
        }
#endif // VERBOSE
        Service to=getName( subtask_list.ack_to(current_subtask) );
        Service return_to=service;
        Word return_as=subtask_list.ack_to(current_subtask);
        Length_t payload_length=1;
        Ctrl_t ctrl=1;
        Redir_t redir=0;
        Word ack_to=subtask_list.ack_to(current_subtask);
        Header_t packet_header = mkHeader(P_data,ctrl,redir,payload_length,to,return_to,ack_to,return_as);
         Word_List packet_payload; packet_payload.push_back(return_as);
        Packet_t packet = mkPacket(packet_header,packet_payload);
        if (to != service){
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " send_ack(): Sending packet via NoC: " <<to<< "<>" <<service<< ""<<endl;
            }
#endif // VERBOSE
            #ifdef SC_VERBOSE
            #endif
            //sba_tile.transceiver.tx_fifo.push_back(packet);
            sba_tile.transceiver->tx_fifo.push_back(packet);
        } else {
            demux_packets_by_type(packet);
        }
    } // of send_ack
*/

 void ServiceManager::restart_subtask(MemAddress regaddr) {

#ifdef VERBOSE
        if (debug_all or service==debug_service){
            cout << "" <<service<< " restart_subtask(" <<regaddr<< ")"<<endl;
        }
#endif // VERBOSE
        MemAddress subtask_address=register_set[regaddr].subtask_address;

        subtask_list.status(subtask_address,STS_new);

            MemAddress code_address= register_set[regaddr].code_address;
            Word subtask_word=mkSubtaskCodeAddressTup(subtask_address,code_address);
#ifdef VERBOSE
            cout << "restart_subtask VM=1: pushing " <<subtask_address<< ""<<endl;
#endif // VERBOSE
            subtask_fifo.push_back(subtask_word);
            printf("subtask_word %d is pushed in service: ",subtask_word, service);
    } // of restart_subtask

#ifdef STEAL
 Subtask ServiceManager::work_steal() {
	 System& sba_system=*((System*)sba_system_ptr);
	 //for(int i=2; i<=NSERVICES; i++) { //TODO: Ashkan: I changed it! hopefully, no more change is required!
	//Instead of i=2, consider i=1 and do not steal when service==1 (control service)
	// Once again: service and address are the same! try it!
	//printf("address: %d service: %d\n",address,service);
	for(int j=address; j < address+sba_system.nservicenodes; ++j) { // excluding service 1
		 int i = (j%(sba_system.nservicenodes))+1;
		 System& sba_system=*((System*)sba_system_ptr);
		 Tile& victim_tile=*(sba_system.nodes[i]);
		 //printf("Tile: %d, pending_fifo_size: %d\n", i, victim_tile.service_manager.pending_subtasks_fifo.size());
		 if(victim_tile.service_manager.pending_subtasks_fifo.size()!=0) {
			 Subtask stolen = victim_tile.service_manager.lock_next_subtask(1); // A thief is locking
			 if(stolen!= TASK_ERROR) {
				 //NOTE: Subtask number is unique per service, not through the entire system
				 //printf("service: %d found a victim: %d \n",service, victim_tile.service_manager.service);
				 //printf("Subtask %d is stolen by %d!\n",stolen,service);
				 Subtask_List_Item src_record = victim_tile.service_manager.subtask_list.get(stolen);
				 victim_tile.service_manager.subtask_list.remove(stolen);
				 if(src_record.to ==0) {
					 printf("I don't now why this happens\n");
					 printf("stolen task address: %d, return_as: %d, approx victim Q size: %d\n",stolen,src_record.return_as, victim_tile.service_manager.pending_subtasks_fifo.size()); 
				 }
				 if (subtasks_address_stack.size()==0){
					std::cerr << service << " SUBTASK STACK ("<< SUBTASKS_SZ <<") OVERFLOW\n"; exit(0);
				 }
				 Subtask stolen_subtask=subtasks_address_stack.pop(); //Ashkan: Subtask and Subtask_Address are basically the same! We pop a new one to use for the stolen
				 subtask_list.cpy(stolen_subtask,src_record);
				 /*if(src_record.to==0) {
					 printf(MAKE_RED "\n\n*Something went wrong*\n" RESET_COLOR "\n");
					 //printf("This tile's address: %d \n",address);
					 //printf("Victim's address: %d \n", victim_tile.address);
				 }*/

				 //subtask_list.status(stolen_subtask,STS_stolen); //means that I have also stolen it. So Get Out of my way!! TODO: Maybe no need
				 //pending_subtasks_fifo.push_back(stolen_subtas); no need!
				 return stolen_subtask; //Do not Steal more. 1 is enough!!
			 }
		 }
	 }
  return TASK_ERROR;
 }// end of work_steal()

 Subtask ServiceManager::lock_next_subtask(int thief) {
	 Subtask ret;
	 pthread_mutex_lock(&_subtask_lock);
	 if(pending_subtasks_fifo.size()>0) {
		 ret = pending_subtasks_fifo.front();
//TODO: For the future, give weight to the tasks. So in the TD file, if the weight is 0, it is a reg, and do not steal it.
		 //IMPORTANT NOTE: I think you should be very careful about the SEQ arguments (no to steal them!)
		 if(thief==1 and (subtask_list.get(ret).nargs<1 or pending_subtasks_fifo.size()<1)) { //or service == 1)) { //HACK: Do not steal subtasks with less than x args!! or from pending_fifo with small waiting list! (Is not worth it!!)
			 pthread_mutex_unlock(&_subtask_lock);
			 return TASK_ERROR;
		 }
		 pending_subtasks_fifo.pop_front();
	 } else ret = TASK_ERROR;
	 pthread_mutex_unlock(&_subtask_lock);
	 return ret;
 }
#endif //of STEAL

 void ServiceManager::core_control(bool steal_state)  { // steal_state comes from the Tile.cc
        System& sba_system=*((System*)sba_system_ptr);
        Tile& sba_tile=*(sba_system.nodes[address]);
        //cout << "Ashkan: core_control start: service:" <<service<<endl;
        //if (core_status == CS_idle and pending_subtasks_fifo.size()==0 and service!=1 ) {
#ifdef STEAL
        if (steal_state and service!= 1) { // Means that steal, only if you are sent here to steal(with the chance given at Tile.cc)
		/*
		 * Work Stealing: service is looking for work (work stealing)
		 * service is always from 2-63(Tilera). service_id%NSERVICES is the same as that. HACK for service 1
		 */
        	Subtask new_work = work_steal();
        	if (new_work!=TASK_ERROR) {
        		current_subtask=new_work;
        		//uint subtask_status=subtask_list.status(current_subtask);
        		subtask_list.status(current_subtask,STS_processing); //TODO: FIXME: from STS_stolen which had no role!
        		if (core_status != CS_eos and core_status != CS_skip){
					core_status = CS_ready;
				}
        	}
        }
        else if (core_status == CS_idle){
		/*
		 * Important NOTE: Here (When STEAL in on) there is No need to check for the pending_subtasks_fifo.size,
		 * because later, we will call lock_next_subtask(0) to see whether the fifo is empty/stolen or not.
		 * We will check it by comparing the result to NULL
		 */
#else
        if (core_status == CS_idle and pending_subtasks_fifo.size()>0 ){
#endif

#ifdef STEAL
            Subtask next=lock_next_subtask(0); // I am not the owner, hence 0
            if(next!=TASK_ERROR) {
            	current_subtask=next;
#else
            current_subtask = pending_subtasks_fifo.front();
            pending_subtasks_fifo.pop_front();
#endif
#ifdef VERBOSE
            if (debug_all or service==debug_service){
            cout << "" <<service<< " core_control(): shifted " <<current_subtask<< " off pending_subtasks_fifo" <<endl;
            }
#endif // VERBOSE
				uint subtask_status=subtask_list.status(current_subtask);
            //cout << "" <<service<< "Ashkan: core_control(): subtask status " <<subtask_status<< ""<<endl;
#ifdef VERBOSE
            if (debug_all or service==debug_service){
            cout << "" <<service<< " core_control(): subtask status " <<subtask_status<< ""<<endl;
            }
#endif // VERBOSE
				if (subtask_status==STS_pending){
				//  Here we can intercept the execution & migrate the subtask
			   /*
			    * put the migration decision here ********** instead I migrate every task once! Maybe!
			    */
               //printf("Ashkan: service_id %d, current_subtask: %d, service: %d\n",service_id, current_subtask, service);
					/*if (subtask_list.migrated(current_subtask) and (service_id != 1) and (service != 1)) { // TODO: DIRTY HACK! to get rid of SEQ!
				   if(request_migration(current_subtask)) {
					   subtask_list.status(current_subtask,STS_migrated); // status to be used to clean up later
					   return;
				   } else subtask_list.status(current_subtask,STS_processing);
					} else {

					}*/
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                cout << "" <<service<< " core_control(): set status for subtask " <<current_subtask<< " to STS_processing"<<endl;
                }
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_processing);
				} else if (subtask_status==STS_eos or subtask_status==STS_skip){
            	// HACK!!!
#ifdef WV_HACK
                if (service!=S_LET and service!=S_IF ){
                    if (subtask_status==STS_skip){
                        core_status = CS_skip;
                    } else {
                        core_status = CS_eos;
                    }
                    core_return_type=P_data;
                } else {
                    if (debug_all or service==debug_service){
                    }
                }
#endif // WV_HACK
					subtask_list.status(current_subtask,STS_processed);
				} else {
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                cout << "" <<service<< " core_control(): status for subtask " <<current_subtask<< " is " <<subtask_list.status(current_subtask)<< ""<<endl;
                }
#endif // VERBOSE
				}
            //cout << "" <<service<< "Ashkan: again: subtask status " <<subtask_status<< ""<<endl;
				if (core_status != CS_eos and core_status != CS_skip){
					core_status = CS_ready;
				}
#ifdef VERBOSE
            if (debug_all or service==debug_service){
            cout << "" <<service<< " core_control(): set core_status to CS_Ready"<<endl;
            }
#endif // VERBOSE

#ifdef STEAL
			}
#endif
        }

        if (core_status == CS_ready or core_status == CS_eos or core_status == CS_skip){
            //cout << "Ashkan: core_status: " << core_status <<endl;
        	n_args=subtask_list.nargs(current_subtask);
//            #ifndef STATIC_ALLOC
//            arg_addresses=build_value_address_list();
//            #else
             arg_addresses=subtask_list.arguments(current_subtask);
//             arg_addresses.size(n_args);
//            #endif
            Symbol_t called_as=subtask_list.called_as(current_subtask);
            opcode=getOpcode(called_as);
            scid=getSCId(called_as);
            sclid=getSCLId(called_as);
#ifdef VERBOSE
            if (debug_all or service==debug_service            ){
                cout << "" <<service<< " CORE CONTROL: SCLId:" <<sclid<< ", SCId:" <<scid<< ", Opcode:" <<opcode<< ", SId:" <<service_id<< ""<<endl;
                cout << "" <<service<< " CORE CONTROL: [";
                for(Subtask_Argument_List::iterator iter_=arg_addresses.begin();iter_!=arg_addresses.end();iter_++) {
                	Word arg_addr=*iter_;
                	cout << hex << arg_addr << dec << " ";
 //                   cout << " " <<ppSymbol(arg_addr)<< " ";
                }
                cout << " ]"<<endl;
            }
#endif // VERBOSE
//#ifndef NO_DATA_ADDRESS_STACK
            // WV_PTR This should change to contain the pointer to the result.
            MemAddress results_address=subtask_list.result_address(current_subtask);
            if (results_address!=0){
            	data_address_stack.push_back(results_address);
                subtask_list.result_address(current_subtask,0);
            }
//#endif
            if (core_status==CS_ready){
            	core_status=CS_busy;
            }
        }
        if ((core_status == CS_done) or (core_status == CS_done_eos) or (core_status == CS_eos) or (core_status == CS_skip)){
#ifdef VERBOSE
            if (debug_all or service==debug_service   ){
            string ppstatus=(core_status == CS_done)?"CS_done":((core_status == CS_skip)?"CS_skip":"CS_eos");
            cout << "" <<service<< " core_control():  ServiceCore status is " <<ppstatus<< " for <" <<current_subtask<< ">"<<endl;
            }
#endif // VERBOSE
            if (subtask_list.status(current_subtask)==STS_processing){
            	subtask_list.status(current_subtask,STS_processed);
            }
            Service to = subtask_list.to(current_subtask);
            //if(to==0) printf(MAKE_GREEN "\n\n\n\n*****WHY******\n\n\n\n" RESET_COLOR "\n");
            Service return_to=subtask_list.return_to(current_subtask);
            Word return_as=subtask_list.return_as(current_subtask);
            Word caller_subtask_argpos=subtask_list.ack_to(current_subtask);
            uint ctrl=0;
            uint eos=0;
            uint skip=0;
            uint empty_packet=0;
            if ((core_status==CS_skip)){
                skip=1;
                ctrl=6;
            } else if ((core_status==CS_eos) or (core_status == CS_done_eos) ){
                eos=1;
                ctrl=2;
            }
            if ((core_status==CS_skip) or (core_status==CS_eos) or (core_status == CS_done_eos)){
                if (core_return_type==P_data){
                    empty_packet=1;
                }
            }
#ifdef VERBOSE
#endif // VERBOSE
//            uint redir=subtask_list.redir(current_subtask);
//            Word ack_to=subtask_list.ack_to(current_subtask);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
//                cout << "" <<service<< " core_control(): REDIR: " <<redir<< "; ACK_TO: " <<ack_to<< ""<<endl;
                cout << "" <<service<< " core_control(): RETURN_TO: " <<return_to<< " (" <<current_subtask<< ")"<<endl;
                cout << "" <<service<< " core_control(): RETURN_AS: " <<hex<<return_as<<dec<< " (" <<current_subtask<< ")"<<endl;
            }
#endif // VERBOSE

//             Length_t payload_length;
             //Word_List packet_payload;
             Word packet_payload = NIHIL;

            uint mode=subtask_list.mode(current_subtask);
//            uint offset=subtask_list.offset(current_subtask);
//            uint fsize=subtask_list.fsize(current_subtask);
            Length_t payload_length=1; // it is always one now!
            if (mode==M_normal){
                if (empty_packet==0){
//                    MemAddress results_address=subtask_list.result_address(current_subtask);
//                    packet_payload = sba_tile.data_store.mgetWord(results_address);
                    packet_payload=subtask_list.result(current_subtask);
                    //cout << "Ashkan says packet_payload: " << packet_payload <<endl;
                } else {
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " core_control(): EOS on " <<current_subtask<< ""<<endl;
                    }
#endif // VERBOSE
                }
            } else { // Streaming
                uint reg_addr = subtask_list.reg(current_subtask);
                Word reg_symbol=symbol_table[reg_addr];
#ifdef VERBOSE
                if (debug_all or service==debug_service){
                    cout << "" <<service<< " core_control(): Mode " <<mode<< ": buffering; reg: " <<reg_addr<< " <" <<reg_symbol<< "> "<<endl;
                    cout << " in reg " <<reg_addr<< ""<<endl;
                }
#endif // VERBOSE

                register_set[reg_addr].status=RDS_present;
                Word reg_status_symbol = NIHIL;
                if (empty_packet==0                   ){
//                    MemAddress results_address=subtask_list.result_address(current_subtask);
//                    Word_List results=sba_tile.data_store.mget(results_address);
//                    sba_tile.data_store.mput(reg_addr,getField(results,offset,fsize));
                    sba_tile.data_store.mputWord(reg_addr,subtask_list.result(current_subtask));
                    reg_status_symbol=setStatus(reg_symbol,DS_present);

                } else if (eos==1){
                    reg_status_symbol=setStatus(reg_symbol,DS_eos);
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< " core_control(): REG " <<reg_addr<< " STATUS: EOS"<<endl;
                        cout << "" <<service<< " core_control(): EOS on " <<current_subtask<< ""<<endl;
                    }
#endif // VERBOSE


                } else if (skip==1){
                    reg_status_symbol=setStatus(reg_symbol,DS_requested);
                }
                symbol_table[reg_addr]=reg_status_symbol;
#ifdef VERBOSE

                if (debug_all or service==debug_service){
                }
#endif // VERBOSE
/*
                if (request_table.size(reg_addr)>0  and empty_packet==0){
#ifdef VERBOSE
                        cout <<  "" <<service<< " core_control(): PENDING REQUESTS for " <<reg_addr<< ": " <<request_table.size(reg_addr)<< ""<<endl;
#endif // VERBOSE
                    uint n_pending_reqs= request_table.size(reg_addr);
                    for(uint req=1;req<=n_pending_reqs ;req++) {
                        Word packet_label=request_table.shift(reg_addr);
                        MemAddress data_address=register_set[reg_addr].data_address;
                        Word_List packet_payload=sba_tile.data_store.mget(data_address);
                        payload_length=packet_payload.size();
                        Header_t packet_header = mkHeader(P_data,ctrl,0,payload_length, getName(packet_label), NA,0,packet_label);
                        Packet_t packet = mkPacket(packet_header,packet_payload);
                        if (getTo(packet_header) != service){
                            //sba_tile.transceiver.tx_fifo.push_back(packet);
                        	sba_tile.transceiver->tx_fifo.push_back(packet);
                        } else {
                            data_fifo.push_back(packet);
                        }
                    }
                }
*/
            }
            Header_t packet_header;
            if(core_return_type == P_data) { // TODO: Ashkan: to use the target for the parents (I guess they do not need return_to)
            	packet_header = mkHeader(core_return_type,ctrl,0,payload_length,to,service,caller_subtask_argpos,return_as);
            } else
            	packet_header = mkHeader(core_return_type,ctrl,0,payload_length,to,return_to,caller_subtask_argpos,return_as);
                        //Packet_t packet = mkPacket(packet_header,packet_payload);
            //if(to==0) printf(MAKE_GREEN"***DANGER, if it is not the final Result!***\n"RESET_COLOR);
            Packet_t packet = mkPacket_new(packet_header,packet_payload);
#ifdef VERBOSE
            if (debug_all or service==debug_service){
                cout << "" <<service<< " core_control(): PACKET:\n" <<ppPacket(packet)<< ""<<endl;
            }
#endif // VERBOSE
#ifdef LAUNCHERs
            if(subtask_list.get_target(current_subtask) != -1) {// HACK for the control services with service=1, here we cannot use (service==1), because a subtask might be migrated to 1, but not being a control subtask
				//printf("service: %d before sending the packet to the parent service ", service);
				//printf("RELEASE: Service: %d Subtask: %d Target: %d\n",service,current_subtask,subtask_list.get_target(current_subtask));
				release_target(subtask_list.get_target(current_subtask)); //FIXME: you might end up waiting for the lock, while the parent receives the packet!
			}
#endif
            if (to != service){
                sba_tile.transceiver.tx_fifo.push_back(packet);
            } else {
                demux_packets_by_type(packet);
            }
            core_status = CS_done;
        } // of CS_done || CS_done_eos || CS_eos || CS_skip
        if (core_status == CS_done or core_status == CS_managed ){
        	Subtask_Status sts=subtask_list.status(current_subtask);
            if ((sts==STS_processed or sts==STS_cleanup or sts==STS_inactive) ){
                if (sts==STS_processed or sts==STS_inactive){
#ifdef VERBOSE
                        cout << "" <<service<< " CLEAN-UP: clean_up() " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                    // clean_up(); Ashkan: In the NewGPRM, clean_up is not used. I removed this part to use the clean_up() for the subtask migration procedure
                    if (sts==STS_processed){
                        subtask_list.status(current_subtask,STS_cleanup);
                        sts=STS_cleanup;
                    }
                }
                if (sts==STS_cleanup ){

#ifdef VERBOSE
      cout << "" <<service<< " CLEAN-UP: remove " <<current_subtask<< ", set status to STS_deleted"<<endl;
#endif // VERBOSE
					subtask_list.remove(current_subtask);
                }
                if (sts==STS_cleanup or sts==STS_inactive){
#ifdef VERBOSE
      cout << "" <<service<< " CLEAN-UP: push " <<current_subtask<< " onto subtasks_address_stack"<<endl;
#endif // VERBOSE
                    if (subtask_list.reg(current_subtask)==0){
                    	subtasks_address_stack.push_back(current_subtask);
                    }
                    current_subtask=0;
                }
            } else if (sts==STS_blocked ){
#ifdef VERBOSE
                    cout << "" <<service<< " CLEAN-UP: reset " <<current_subtask<< " status to STS_new"<<endl;
#endif // VERBOSE
                subtask_list.status(current_subtask,STS_new);
            } else {
#ifdef VERBOSE
                    cout << "" <<service<< " CLEAN-UP: STS=" <<sts<< ", NO CLEAN-UP for " <<current_subtask<< ""<<endl;
                if (core_status == CS_managed){
                    //clean_up(); Ashkan commented this out
                }
#endif // VERBOSE
            }

            core_status= CS_idle;
        } // of CS_done || CS_managed
        //cout << "Ashkan:End of core_control" << endl;
    } // END of core_control()


 bool ServiceManager::request_migration(uint current_subtask) {
	 System& sba_system=*((System*)sba_system_ptr);
	 Tile& sba_tile=*(sba_system.nodes[address]);
	 // create a packet of type P_migration
	 // To = target SNId, from allocation logic
	 // Return_to = current service
	 // Return_as contains current subtask, for clean-up
	 // Payload is the current subtask address, so actually no need?
	 //cout << "Ashkan's debug: Subtask to migrate:" << current_subtask << endl;
	 Packet_type_t packet_type = P_migrate;
	 Service to;
#ifdef LAUNCHER
#ifdef MULTIPROGRAMMING
	/*
	  * if (subtask_list.get_target(current_subtask)==-1) is simple check to examine whether the subtask has a default target (-1), or
	  * the target is set by one of its children. If you have a child that has worked on the core X, it means that the load of X was
	  * prev_load+2 at the beginning of the child's work, and prev_load+1 at the end of it. We have done this to give the priority of
	  * that core to the parents. So if the current subtask is a parent, it has to change the load to the prev_load before searching
	  * for the best_target; but it has to do it only when it gets the lock. This way, all others see the load as prev_load+1, and
	  * prefer not to use it, but the current subtask sees it at prev_load
	  */
	 if (subtask_list.get_target(current_subtask)== -1) {
		 subtask_list.set_target(current_subtask,service); // First, set it to the service. then find the best one, considering "service" as the initial value
		 to = find_target(subtask_list.get_target(current_subtask), 0); // based on the above explanation, it is not a parent
	 } else
		 to = find_target(subtask_list.get_target(current_subtask), 1); // based on the above explanation, it is a parent
	 //printf("****\nservice_id: %d previously on: %d To: %d\n****\n",service, subtask_list.get_target(current_subtask),((to-1) % NSERVICES)+1);
	 subtask_list.set_target(current_subtask, to);
	 //printf(MAKE_GREEN "APP: %d Service: %d Subtask: %d Target: %d" RESET_COLOR "\n", appno, service,current_subtask,subtask_list.get_target(current_subtask)%NSERVICES);
	 //Service to = ((service_id-1)%4)+1; //TODO: target SNId, from allocation logic
#endif // MULTIPROGRAMMING
#else
	 to = service; // NO MIGRATION AT ALL
#endif
	 if(((to-1)%sba_system.nservicenodes)+1 == ((service-1)%sba_system.nservicenodes)+1) return false; //If src and dest are the same DO NOT migrate!
	 Service return_to = service;
	 Word return_as = current_subtask;
	 // The rest is not important!
	 Word ctrl=0;
	 Word redir=0;
	 Length_t payload_length=1;
	 Word caller_subtask_argpos=0;
	 Header_t req_migration_header = mkHeader(packet_type,ctrl,redir,payload_length,to,return_to,caller_subtask_argpos,return_as);
	 Word_List req_migration_payload; req_migration_payload.push_back(0);
	 Packet_t request_packet = mkPacket(req_migration_header,req_migration_payload);
#ifdef VERBOSE
	if (debug_all or service==debug_service){
		cout << "" <<service<< " request_migration(): PACKET:\n" <<ppPacket(request_packet)<< ""<<endl;
	}
#endif // VERBOSE
	// send packet
	sba_tile.transceiver.tx_fifo.push_back(request_packet);
	return true;
 }

 void ServiceManager::migrate_subtask() {
	 // wait on migration_fifo;
	 // get the node id of the source and the subtask address
	 // access the source via the system pointer
	 System& sba_system=*((System*)sba_system_ptr);
	 while (migration_fifo.size()>0){ // TODO: Ashkan: Why was it a "while"?
		 Packet_t migration_packet= migration_fifo.front();migration_fifo.pop_front();
//#ifdef VERBOSE
	 cout << "Service: " <<service<< " : Migration_Packet: \n";
	 cout << ppPacket(migration_packet) << endl;
//#endif // VERBOSE
	 Service source_address = getReturn_to_p(migration_packet);
	 Tile& src_tile=*(sba_system.nodes[source_address]);
	 Tile& sba_tile=*(sba_system.nodes[address]);
	 //const
	 Subtask_List_Item src_record = src_tile.service_manager.subtask_list.get(getReturn_as_p(migration_packet));
     if (subtasks_address_stack.size()==0){
           std::cerr << service << " SUBTASK STACK ("<< SUBTASKS_SZ <<") OVERFLOW\n"; exit(0);
      }
     Subtask migrated_subtask_address=subtasks_address_stack.pop(); //Ashkan: Subtask and Subtask_Address are basically the same!
     subtask_list.cpy(migrated_subtask_address,src_record);

     if(src_record.to==0) printf(MAKE_RED "\n\n*****WHY******\n\n" RESET_COLOR "\n");

	 subtask_list.migrated(migrated_subtask_address,true);
	 subtask_list.status(migrated_subtask_address,STS_pending);
	 //cout << "Ashkan's Debug: Subtask number just before migration to here: and I will change its status to pending"<< getReturn_as_p(migration_packet)<<endl;
	 pending_subtasks_fifo.push_back(migrated_subtask_address);
	 /*Packet_type_t packet_type = P_clean_up;
	 Service to = source_address; //TODO: target SNId, from allocation logic
	 Word return_as = current_subtask; //Ashkan: So, why do I need a sts_migrated?
	 // The rest is not important!
	 Service return_to = 0;
	 Word ctrl=0;
	 Word redir=0;
	 Length_t payload_length=1;
	 Word caller_subtask_argpos=0;
	 Header_t cleanup_header = mkHeader(packet_type,ctrl,redir,payload_length,to,return_to,caller_subtask_argpos,return_as);
	 Word_List cleanup_payload; cleanup_payload.push_back(0);
	 Packet_t cleanup_packet = mkPacket(cleanup_header,cleanup_payload);
#ifdef VERBOSE
	if (debug_all or service==debug_service){
		cout << "" <<service<< " Clean_up PACKET:\n" <<ppPacket(cleanup_packet)<< ""<<endl;
	}
#endif // VERBOSE
	// send packet
	sba_tile.transceiver->tx_fifo.push_back(cleanup_packet);*/
	 //subtask_list.remove(current_subtask); //TODO: is it gonna be the clean_up?? No!!! it made problems for SEQ!!
	 // allocate a new subtask (pop from stack)
	 // copy content of src_record into it
	 // push on pending_subtasks_fifo
	 // send clean-up request to source -> factor out
	 }
 }

 // WV 28dec2012: surprisingly, this whole clean-up is obsolete!
 // Need to replace it with a way to deallocate results instead
 void ServiceManager::clean_up()  {
	 //System& sba_system=*((System*)sba_system_ptr);
	 //Tile& sba_tile=*(sba_system.nodes[address]);
	 Packet_t cleanup_packet= clean_up_fifo.front();clean_up_fifo.pop_front(); // TODO: migrated status should be taken into account
	 //subtask_list.remove(getReturn_as_p(cleanup_packet));
//        if (getKind(arg_symbol)!= K_L){
        	// get pointer & deallocate
//        }

/*
        uint nargs=arg_addresses.size();
#ifdef VERBOSE
        if (debug_all or service==debug_service){
                cout << "" <<service<< ": clean_up " <<current_subtask<< ":  ADDRESSES: " <<nargs<< "" <<endl;
        }
#endif // VERBOSE
        if (nargs>0){
            for(uint iter_=0;iter_<=nargs-1 ;iter_++) {
                if (subtask_list.argmodes(current_subtask)[iter_]==0){
                    MemAddress arg_address=arg_addresses[iter_];
#ifdef VERBOSE
                    if (debug_all or service==debug_service){
                        cout << "" <<service<< ": clean_up:  ADDRESS: " <<arg_address<< ""<<endl;
                    }
#endif // VERBOSE
                    Word arg_symbol=symbol_table[arg_address];
                    uint arg_status=getStatus(arg_symbol);
                    if (arg_status==DS_present or  arg_status==DS_eos){
                        if (arg_address>NREGS+DATA_OF){
                            arg_symbol=setStatus(arg_symbol,DS_cleared);
                            data_address_stack.push_back(arg_address);
#ifdef VERBOSE
                            if (debug_all or service==debug_service){
                                cout << "" <<service<< ": clean_up " <<current_subtask<< ": push ADDRESS " <<arg_address<< " onto STACK"<<endl;
                                cout << "" <<service<< ": clean_up(): stack size/pointer " <<data_address_stack.size()<< ""<<endl;
                                cout << "" <<service<< ": clean_up: REMOVED " <<arg_address<< ""<<endl;
                            }
#endif // VERBOSE

                            if (getKind(arg_symbol)!= K_L){
                            	// get pointer & deallocate
                            }
                            sba_tile.data_store.remove(arg_address);
                        } else {
                            arg_symbol=setStatus(arg_symbol,DS_absent);
                        }
                        symbol_table[arg_address]=arg_symbol;
                    }
                }
            } // of for
        }
*/
    } // of clean_up


 /* Subtask migration

  // in core_control():
     if (subtask_status==STS_pending){
//  Here we can intercept the execution & migrate the subtask
	 bool migrate_ok = false; // put the migration decision here
	 if (migrate_ok) {
		 request_migration(current_subtask);
			 subtask_list.status(current_subtask,STS_migrated); // status to be used to clean up later
		 } else {
                subtask_list.status(current_subtask,STS_processing);
                }
            }

 // will need a "clean up packet"

  void request_migration(uint current_subtask) {
  // create a packet of type P_migration
  // To = target SNId, from allocation logic
  // Return_to = current service
  // Return_as contains current subtask, for clean-up
  // Payload is the current subtask address, so actually no need?
// send packet
  }

  // on the receiving side
  // get P_migrate =>

demux_packets_by_type(Packet_t& packet) {
        Packet_type_t packet_type=getType(getHeader(packet));

         switch (packet_type) {
         case P_migrate :
         {
            migration_fifo.push_back(packet);
          break;
         }
            case P_clean_up :
         {
            clean_up_fifo.push_back(packet);
          break;
         }
migrate_subtask() {

// wait on migration_fifo;
// get the node id of the source and the subtask address
// access the source via the system pointer
 System& sba_system=*((System*)sba_system_ptr);
        Tile& src_tile=*(sba_system.nodes[source_address]);
		Subtask_List_Item src_record = src_tile.service_manager.subtask_list[src_subtask_address];
		// allocate a new subtask (pop from stack)
		// copy content of src_record into it
		// push on pending_subtasks_fifo
		// send clean-up request to source -> factor out
}
  * */
