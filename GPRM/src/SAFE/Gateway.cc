// Gateway.cc
//
// :title: Gannet Service-based SoC project - Gateway class
//
//  (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//


#include <fstream>
#include "GatewayTile.h"
#include "System.h"
#include "Gateway.h"
#include "Interface.h"
#include "Timings.h"

using namespace std;
using namespace SBA;

 void Gateway::run()  {
#ifdef VERBOSE
                		cout << "Gateway::run()\n";
#endif // VERBOSE

    GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
		if (gw_tile.init == true) {
	    	parse_task_description();
		}
        if (gw_tile.finished == false){
            receive_packets();
        }
    } // of run()

 void Gateway::parse_task_description()  {
    GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
#if VERBOSE==1
    cout << "Gateway: Interface receive()"<<endl;
#endif // VERBOSE
                    core_status = CS_busy;
                    Bytecode tdc=Interface::read_bytecode(task);
                    TaskDescription task_description(tdc);
                    Packet_List task_description_packet_list=task_description.Packets;
//                     Word nullw = (Word)0;
//                     gw_tile.result_store.mputWord(task_id,nullw);
#if QUIT==1
                        exit(1);
#endif // QUIT
//                        cout << "NPACKETS: "<<task_description_packet_list.size()<<"\n";
                    for(Packet_List::iterator iter_=task_description_packet_list.begin();iter_!=task_description_packet_list.end();iter_++) {
                    	Packet_t task_description_packet=*iter_;
                    	if ((getType(getHeader(task_description_packet))) == P_reference) {
#if VERBOSE==1
                        cout << "GATEWAY SENDS PACKET:"<<endl;
                        cout << ppPacket(task_description_packet)<<endl;
#endif // VERBOSE
                        transmit_packet(task_description_packet);
                    	} else if ((getType(getHeader(task_description_packet))) == P_code) {
                    		// It must be a code packet, store it
                    		store_code(task_description_packet);
                    		} // if

                    } // for
	gw_tile.init = false;
					
    } // of parse_task_description
 /*
 void Gateway::parse_task_description_OLD()  {
    GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
#if VERBOSE==1
    cout << "Gateway: Interface receive()"<<endl;
#endif // VERBOSE
					uint ntdcs=vmif.receive(core_status);
                if (ntdcs!=0 and tasks_stack.size()>0 ){
                    core_status = CS_busy;
                    uint task_id=tasks_stack.pop();
                    vmif.iodescs[task_id]=ntdcs;
                    Bytecode tdc= vmif.tdcs.front();vmif.tdcs.pop_front();
                    TaskDescription task_description(tdc,task_id);
                    Packet_List task_description_packet_list=task_description.Packets;
                     Word nullw = (Word)0;
                     gw_tile.result_store.mputWord(task_id,nullw);
#if QUIT==1
                        exit(1);
#endif // QUIT
                        cout << "NPACKETS: "<<task_description_packet_list.size()<<"\n";
                    for(Packet_List::iterator iter_=task_description_packet_list.begin();iter_!=task_description_packet_list.end();iter_++) {
                    	Packet_t task_description_packet=*iter_;
                    	if ((getType(getHeader(task_description_packet))) == P_reference) {
#if VERBOSE==1
                        cout << "GATEWAY SENDS PACKET:"<<endl;
                        cout << ppPacket(task_description_packet)<<endl;
#endif // VERBOSE
                        transmit_packet(task_description_packet);
                    	} else if ((getType(getHeader(task_description_packet))) == P_code) {
                    		// It must be a code packet, store it
                    		store_code(task_description_packet);
                    		} // if

                    } // for
                }

    } // of parse_task_description_OLD
*/
 void Gateway::store_code(Packet_t subtask_code_packet)  {
	    GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
#if VERBOSE==1
            cout << "\nGateway::store_code()"<<endl;
#endif // VERBOSE

#ifdef VERBOSE
                cout << ppPacket(subtask_code_packet)<<endl;
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
	bool removed_extsyms=false;
	for (Word_List::iterator _iter=subtask_code.begin(); _iter!=subtask_code.end();_iter++) {
		Word symbol = *_iter;
		if (extidx==0 && getExt(symbol)>0) { // OK, extended symbol
			removed_extsyms=true;
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

                cout << "Gateway::store_code(): subtask code:"<<endl;
                cout << ppPayload(subtask_code)<<endl;
    if (removed_extsyms) {
                cout << "Gateway::store_code(): subtask code after removal of extended symbol values:"<<endl;
                cout << ppPayload(n_subtask_code)<<endl;
	}
#endif // VERBOSE
            Word code_label= subtask_code_packet[2];
            CodeAddress code_address=getCodeAddress(code_label);
            gw_tile.code_store.mput(code_address,n_subtask_code);

#ifdef VERBOSE
                cout << "Gateway::store_code(): stored " <<ppSymbol(code_label)<< " at " <<code_address<< "" <<endl;
#endif // VERBOSE

    } // of store_code()

 void Gateway::receive_packets()  {
        GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
//#ifdef VERBOSE
//         uint print_ok=0;
//         if (gw_tile.transceiver.rx_fifo.status()==1         ){
//            print_ok=1;
//        }
//#endif // VERBOSE

#if USE_THREADS==1
         if (!gw_tile.transceiver.rx_fifo.has_packets()) gw_tile.transceiver.rx_fifo.wait_for_packets(address);
#endif
         while (gw_tile.transceiver.rx_fifo.has_packets() ) {
        	  Packet_t  rx_packet=  gw_tile.transceiver.rx_fifo.pop_front();

            if ( getType(getHeader(rx_packet)) == P_data){
                rx_fifo.push_back(rx_packet);
                store_result_packets();
                if (gw_tile.finished){
                  break;
                }
#if DATA==1
            } else if ( getType(getHeader(rx_packet)) == P_request){
                request_fifo.push_back(rx_packet);
#endif
            } else {

#ifdef VERBOSE
                cout << "Gateway::receive_packets:"<< endl;
                cout << "Gateway address = " << gw_tile.address << " OR "<< gw_tile.service << endl;
                cout << " received a "<< (int)getPacket_type_p(rx_packet)<<" packet from " << getReturn_to_p(rx_packet) << endl;
				cout << ppPacket(rx_packet)<<endl;
#endif // VERBOSE
                 cout << "ERROR: Gateway can only receive data/result" <<  getType(getHeader(rx_packet)) << endl;//.to_uint
//                 exit(0);
            }
        }
    } // of receive_packets


 void Gateway::store_result_packets()  {
    GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
    System& sba_system=*((System*)sba_system_ptr);
           while (rx_fifo.has_packets()){
            	Packet_t data_packet= rx_fifo.pop_front();
//                Word label= getReturn_as(getHeader(data_packet));
                Word_List result=getPayload(data_packet);
//                uint  task_id=getTask(label);
//                        vmif.send(result,task_id);
				cout <<endl;
				if (result.size()==0){
                     cout << "Return value: []"<<endl;
                     sba_system.result=nullptr;
				} else {
					Word w = result[0];
					uint val=getValue(w);
					if (getKind(w)==K_B) {
                     cout << "Return value:\t" <<val<< ""<<endl;
                     uint* val_ptr=&val;
                     sba_system.result=(void*)val_ptr;
					} else if (getKind(w)==K_P) {
						cout << "Return pointer:\t" <<std::hex <<getPointer(w)<< std::dec<<""<<endl;
						sba_system.result=getPointer(w);
					} else {
						cout << "Return ERROR!\n";
					}
				}

#if DISTR==1
                        exit(0);
#endif // DISTR
//                        tasks_stack.push_back(task_id);
//                        uint stack_full = MAX_NTASKS;
//                        if (tasks_stack.size()==stack_full){
//                            core_status = CS_idle;
							gw_tile.finished=true;
//                        }
            }
    } // of store_result_packets

 void Gateway::transmit_packet(Packet_t packet) {

        GatewayTile& gw_tile=*((GatewayTile*)sba_gwtile_ptr);
#ifdef VERBOSE
        if (tx_fifo.size()>0 ){
        }
#endif // VERBOSE
#if VERBOSE==1
                cout << " packet <"<<(int) getType(getHeader(packet)) <<"> <"<<getReturn_as(getHeader(packet))<<"> to TX for "<< (int)getTo(getHeader(packet))<<"\n";
#endif // VERBOSE
#if DISTR==1
                gw_tile.transceiver.transmit_packets();
#else // DISTR
                //Ashkan changed it to implement "all-services on every-tile" mechanism.
                if ((getType(getHeader(packet))) == P_reference) {
                    gw_tile.transceiver.tx_fifo.push_back(packet);
#ifdef VERBOSE
                    cout << "unicast for reference packet" << endl;
#endif
                    gw_tile.transceiver.run();
                } else if ((getType(getHeader(packet))) == P_code) {
                    gw_tile.transceiver.tx_fifo.push_back(packet);
#ifdef VERBOSE
                	cout << "multicast for code packets" << endl;
#endif
                	gw_tile.transceiver.multicast();
                } else {
#ifdef VERBOSE
                	cout << "GOT RUBBISH! IGNORE!\n" << endl;
#endif
                }

#endif // DISTR
    } // of transmit_packet

#if DATA==1
    unordered_map<uint,Word> Gateway::read_data(string data_file) { //H
          unordered_map<uint,Word> data;
          ifstream inFile;
          inFile.open(data_file.c_str());
          if (inFile.fail()) {
              cerr << "Can't open file "<<data_file<<" for reading." << endl;
              exit(1);
          }
          uint ndata,dkey;
          Word dvalue;
          inFile >>ndata;
          for (uint i=0;i<ndata;i++) {
                inFile >> dkey >> dvalue;
                data[dkey]=dvalue;
            }
            return data;
         }
#endif // DATA

