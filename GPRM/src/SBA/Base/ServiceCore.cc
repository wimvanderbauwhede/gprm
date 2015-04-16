// Base::ServiceCore.cc
//   
// :title: Gannet Service-based SoC project - Service Core Library 
//    
//
// *
// *  (c) 2004-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
//

#include <fstream>
#include <sstream>
#include "SBA/System.h" 
#include "SBA/Tile.h" 
#include "Base/ServiceCore.h"
#include "SystemConfiguration.h"
#include "Base/Let.h"

 using namespace std;


 using namespace SBA; 
#ifndef NO_SERVICES    

 double Base::ServiceCore::word2dbl(Word result) {
        u_int64_t* result_p=&result;
        void* tmpd=(void*)result_p;
        double* tmp_dbl_p = (double*) tmpd;
        double dbl_result=*tmp_dbl_p;
        return dbl_result;
    }

 Word Base::ServiceCore::dbl2word(double dbl) {
        double* dbl_p=&dbl;
        void* v_p=(void*)dbl_p;
        Word* w_p=(Word*)v_p;
        Word w= *w_p;
        return w;        
    }

 Word_List Base::ServiceCore::string2symbol(string str) {
        uint  npad=NBYTES - (str.size() % NBYTES);
        uint nwords=(str.size()+npad)/NBYTES;
         str.resize(NBYTES*nwords,0);
        Symbol_t sheader = mkSymbol(K_B,T_s,1,1,0,nwords,npad);
        Word_List sym;
        sym.push_back(sheader);
        for(uint i=0;i<nwords;i++) {        
            Word strword=0;
            for (uint j=0;j<NBYTES;j++) {
                strword+=(Word)str[NBYTES*i+j]<<8*(NBYTES-j-1);
            }
            cout << strword <<"\n";
            sym.push_back(strword);
        }        

    
        return sym;
    }
 string Base::ServiceCore::extsym2str(Word sym) {
	 Word filename_ptr=getExtValue(sym);
	 void* vp = (void*)filename_ptr;
	 // the format is nwords|npadbytes|content
	 Word* wp=(Word*)vp;
	 uint nwords = wp[0];
	 uint npadbytes = wp[1];
//	 unsigned char* bytes =(unsigned char*)wp;
	 string filename = "";
//	 for (uint idx=16;idx<16+nwords*8-npadbytes;idx++) {
//		 filename+=bytes[idx];
//	 }
//	 std::cout << "extsym2str(): nwords="<<nwords<<", npadbytes="<<npadbytes<<"\n";
	 for (uint idx=0; idx<nwords; idx++) {
		 Word w = wp[idx+2];
//		 std::cout << std::hex <<w <<"\n";
		 for (uint jdx=0;jdx<8;jdx++) {
			 Word byte = (w>>(8*(7-jdx))) & 0xFF;
			 unsigned char ch=(unsigned char)byte;
//			 std::cout << "("<<byte<<") "<<ch <<"\n";
			 if (idx==nwords-1 && jdx == (8-npadbytes)) {
//				 std::cout << "\n" ;
				 break;
			 }
			 filename+=ch;
		 }
	 }
	 return filename;
 }
 string Base::ServiceCore::sym2str(Word_List sym) {
         Word header=sym.front();sym.pop_front();
        uint nwords=getSubtask(header);
        uint padding=getName(header);

        string str;
        str.reserve(nwords*NBYTES-padding);
        for(uint i=0;i<nwords;i++) {    
            uint npad=(i==nwords-1)?padding:0;
            Word strword=sym.front();sym.pop_front();
            for (uint j=0;j<NBYTES-npad;j++) {
                char byte=(char)((strword>>8*(NBYTES-j-1))&255);
                str+=byte;
            }
        }       
        

    
        return str;
    }
    

 string Base::ServiceCore::wl2str(Word_List wl) {
        uint nwords=wl.size();
        uint padding=0;
        string str;
        str.reserve(nwords*NBYTES-padding);
        for(uint i=0;i<nwords;i++) {    
            uint npad=(i==nwords-1)?padding:0;
            Word strword=wl.front();wl.pop_front();
            for (uint j=0;j<NBYTES-npad;j++) {
                char byte=(char)((strword>>8*(NBYTES-j-1))&255);
                str+=byte;
            }
        }       
       
    
        return str;
    }
    

 Int Base::ServiceCore::sym2int(Word_List sym) {
        return getInt(sym);
    }
    

 Word Base::ServiceCore::sym2uint(Word_List sym) {
        return getUInt(sym);
    }
    

 bool Base::ServiceCore::sym2bool(Word_List sym) {
        return getUInt(sym)==1;
    }
                    

 float Base::ServiceCore::sym2flt(Word_List sym) {
         Word result=sym.front();sym.pop_front();
#if WORDSZ==64
             double flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
#else // WORDSZ==32
             float flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
#endif // WORDSZ
        return flt_result;
    }
#endif // NO_SERVICES        





#ifndef NO_SERVICES    


 Int Base::ServiceCore::div(Int m,Int n) {
        Int q=0;
        Int         sm=1;
        if (m<0){
            sm=-1;
        }        
        Int sn=1;
        if (n<0){
            sn=-1;
        }        
        Int           um=m*sm;
        Int     un=n*sn;
        Int modmn= um % un;
        q=(um-modmn)/un;
        if (2*modmn>un){
            q+=1;
        }  
        return q*sn*sm;
    }
    
    

 void Base::ServiceCore::ls_ALU() {
	 /*
        // Set up context
	ServiceCore* servicecore_ptr=this;
	ServiceCore& *servicecore_ptr;
//System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
*/
#ifdef VERBOSE
        cout << "ALU CORE: processing subtask " <<current_subtask()<< ""<<endl;
#endif // VERBOSE
         Word_List result_list;
        Uint operation=method();

#ifdef VERBOSE
        cout << "ALU (" <<service<< ") CORE: " <<nargs()<< " addresses"<<endl;
#endif // VERBOSE
        Word res_symbol=arg(0);
        Int        int_result=getInt(res_symbol);
        Word wresult=int_result;
#ifdef VERBOSE
        cout << "ALU CORE: arg 1: Found int " <<wresult<< " (" <<T_i<< ") @ " <<0<< ""<<endl;
#endif // VERBOSE
        uint n_args=nargs();
        if (operation==M_ServiceCore_ALU_not){
            wresult=1-wresult;
        } else {
            int             ii=0; 
            for(uint argn=0;argn<=n_args-1 ;argn++) {
                ii+=1;
                if (ii>1){
                Word tres_symbol=arg(argn);
                Int        int_tres=getInt(tres_symbol);
                 Word tres=int_tres;                    
#ifdef VERBOSE
                        cout << "ALU CORE: arg " <<ii<< ": Found int " <<tres<< " (" <<T_i<< ") @ " <<addr(argn)<< ""<<endl;
#endif // VERBOSE
                 switch (operation) {
                 case M_ServiceCore_ALU_plus :
                 {
                     int_result+=int_tres;                    
                  break;
                 }
                 case M_ServiceCore_ALU_minus :
                 {
                     int_result-=int_tres; 
                  break;
                 }
                 case M_ServiceCore_ALU_times   :
                 {
                     int_result*=int_tres;
                  break;
                 }
                 case M_ServiceCore_ALU_over :
                 {
                     int_result=div(int_result,int_tres);
                  break;
                 }
                 case M_ServiceCore_ALU_lt :
                 {
                     int_result=(int_result<int_tres)?1:0;
                  break;
                 }
                 case M_ServiceCore_ALU_gt :
                 {
                     int_result=(int_result>int_tres)?1:0;
                  break;
                 }
                 case M_ServiceCore_ALU_eq :
                 {
                     int_result=(int_result==int_tres)?1:0;
                     break;}
                 default:
                    cerr << "Unknown ALU CORE service: " <<operation<< "";
                    exit(1);
                       exit(0);
                } ;
            }
            }
        }
         wresult=(Uint)int_result;                
#ifdef VERBOSE
        cout << "ALU CORE RESULT: (uint" <<WORDSZ<< ") " <<wresult<< ""<<endl;
        cout << "ALU (" <<service<< ") CORE (" <<current_subtask()<< "):  result: " <<wresult<< ""<<endl;
#endif // VERBOSE
        Word one=1;
        if (wresult>((one<< FB_Value)-1)){ // too big to fit in a Word
            res_symbol=setExt(res_symbol,1);
            res_symbol=setNSymbols(res_symbol,1);
			Word* wp = new Word; // FIXME: malloc instead!
			void* vp=(void*)wp;
			Word wvp=(Word)vp;
			res_symbol=setValue(res_symbol,wvp);
//             result_list.push_back(res_symbol);result_list.push_back(wresult);
        } else {
            res_symbol=setExt(res_symbol,0);
            res_symbol=setValue(res_symbol,wresult);
//             result_list.push_back(res_symbol);
        }            
        result(res_symbol);
    } // of ALU
            
#if 0 // SKIP ls_FPU

void Base::ServiceCore::ls_FPU(Base::ServiceCore* ptr) {
    // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
    
#ifdef VERBOSE
        cout << "FPU CORE: processing subtask " <<current_subtask()<< ""<<endl;
#endif // VERBOSE
                    
     Word_List result_list;
    
        Word return_as=sba_tile.service_manager.subtask_list.return_as(current_subtask());
        Uint operation=opcode;
#ifdef VERBOSE
        cout << "FPU (" <<service<< ") CORE: " <<nargs()<< " addresses"<<endl;
#endif // VERBOSE
        
        Word_List words=arg(0);
        Word     res_symbol=words[0];
     result_list.push_back(res_symbol);
        double result=getFloat(words);
    
        if (operation==M_ServiceCore_FPU_not){
            result=1.0-result;
        } else {
            int ii=0; 
            uint n_args=nargs();
            for(uint argn=1;argn<=n_args-1 ;argn++) {
                ii+=1;
                if (ii>1){
                    Word_List twords=arg(argn);
                    double tres=getFloat(words);
                    
                     switch (operation) {
                     case M_ServiceCore_FPU_plus :
                     {
                         result+=tres;                    
                      break;
                     }
                     case M_ServiceCore_FPU_minus :
                     {
                         result-=tres; 
                      break;
                     }
                     case M_ServiceCore_FPU_times   :
                     {
                         result*=tres;
                      break;
                     }
                     case M_ServiceCore_FPU_over :
                     {
                         result=result/tres;
                      break;
                     }
                     case M_ServiceCore_FPU_lt :
                     {
                         result=(result<tres)?1:0;
                      break;
                     }
                     case M_ServiceCore_FPU_gt :
                     {
                         result=(result>tres)?1:0;
                      break;
                     }
                     case M_ServiceCore_FPU_eq :
                     {
                         result=(result==tres)?1:0;
                         break;}
                     default:
                        cerr << "Unknown FPU CORE service: " <<operation<< "";
                        exit(1);
                           exit(0);
                    } ;
                }
            }
        }
#ifdef VERBOSE
#endif // VERBOSE
                        
#if WORDSZ==64               
              Word resultword=(Word)&result;
             
#else // WORDSZ==32
             float flt_result=(float)result; Word resultword=(Word)&flt_result;
#endif // WORDSZ
#ifdef VERBOSE
    cout << "FPU CORE RESULT: (uint" <<WORDSZ<< ") " <<result<< ""<<endl;
    cout << "FPU (" <<service<< ") CORE (" <<current_subtask()<< "):  result: " <<result<< ""<<endl;
#endif // VERBOSE

     result_list.push_back(resultword);
    
    result(result_list);
        
} // of ls_FPU    
        
 

    
    
#endif // SKIP ls_FPU

 void Base::ServiceCore::ls_BEGIN() {
        Word res;
        uint n_args=nargs();
        res=arg(n_args-1);
#ifdef VERBOSE
         cout << service<< " CORE: Passing on result "<<res<<"\n";
#endif // VERBOSE
        result(res);
 } // of BEGIN

#include "ls_SEQ.cc"

 void Base::ServiceCore::ls_IF() {
	 uint  argidx=0;
	 uint operation =method();
	 /*
	if (waiting()) {
		// This means that a packet was dispatched
		// we now receive the value for the corresponding argument
		// As we are now called, it means that all args are present
		// So I need to run the logic again
		// But actually we don't need this here!
    } else {
    */
        if (operation==M_ServiceCore_IF_return or operation==M_ServiceCore_IF_returntc){
            argidx=0;
        } else {
            unsigned int condval= arg(0) & 0x1;
            argidx= 2-condval;
        }
        /*
    }
    */
// If it's quoted and K_R, it will be a request, dispatch it;
// otherwise, return it, it will be data, just return it.
	if (isQuoted(argidx) && isRef(argidx)) {
			dispatch(argidx);
	} else {
			result(arg(argidx));
	}
} // of ls_IF

#include "ls_LET.cc"

#if 0
 void Base::ServiceCore::ls_S_IF(Base::ServiceCore* ptr) {
        // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
        Word operation=opcode;
        MemAddresses& addresses=addresses();
        MemAddress  valaddress=0;
        if (operation==M_ServiceCore_S_IF_return){
            valaddress=addresses[0];
        } else {
            MemAddress condaddr=addresses[0];
            Word condval=sba_tile.data_store.mget(condaddr)[0]&1;
            if (condval!=0){
                valaddress=addresses[1];
            } else {
                valaddress=addresses[2];
            }           
        }
        
        Word_List result_list=sba_tile.data_store.mget(valaddress);
        Word result=result_list[0];

        if (getKind(result) == K_D){
            core_return_type= P_request;
        } else if (getKind(result) == K_B                         ){
            core_return_type=P_data;
        } else if (getKind(result) == K_R   ){
          sba_tile.service_manager.subtask_list.to(current_subtask(),getName(result));
            core_return_type= P_reference;
        }          
        
        result(result_list);
        
    } // of ls_S_IF        
#endif // SKIP ls_S_IF

#if 0 // SKIP ls_Math

 void Base::ServiceCore::ls_Math(Base::ServiceCore* ptr) {
        // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
        
        Word operation=opcode;
        MemAddresses& addresses=addresses();
    Word_List result_list;
        if (operation==M_ServiceCore_Math_rand        ){
        MemAddress address0=addresses[0];

        Word_List words=sba_tile.data_store.mget(address0);
        Int min_val=getInt(words);
        
        MemAddress address1=addresses[1];
        words=sba_tile.data_store.mget(address1);
        Int max_val=getInt(words);
         const Word res_symbol = EXTSYM;
         result_list.push_back(res_symbol);
         srandom(state_register[0]);      
         Word rnd_val = random();
         Word res_val=min_val + max_val + (rnd_val % max_val);  
         result_list.push_back(res_val);
#ifdef VERBOSE
        cout << "RAND CORE RESULT: " <<res_val<< ""<<endl;
#endif // VERBOSE
        } else {
            cerr << "Math: Operation " <<operation<< " not supported";
            exit(1);
        }
        result(result_list);
        
    } // of ls_Math     



 
#endif // SKIP ls_Math
#if 0
 void Base::ServiceCore::ls_LET() {
        // Set up context
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
	 SBA::Tile& sba_tile=*(sba_system.nodes[this->address]);

        MemAddresses& addresses=addresses();
        Word service_word=sba_tile.service_manager.subtask_list.called_as(current_subtask());
        
		Name_t service=getName(service_word);
        core_return_type=P_data;
#ifdef VERBOSE
         int ppservice=(int)service;
        cout << "LET (" <<service<< ") CORE: " <<current_subtask()<< ": (" <<ppservice<< "<>" <<SC_Base::ServiceCore_LET<< "\n";
#endif // VERBOSE
        uint method=method();
        if (method==M_ServiceCore_LET_let or method==M_ServiceCore_LET_lettc){
#ifdef VERBOSE
	        cout << "LET (" <<service<< ") CORE: " << current_subtask() << "\n";

            cout << "LET (" <<service<< ") CORE: " <<  "TO: " <<  sba_tile.service_manager.subtask_list.to(current_subtask()) << "\n";
            cout << "LET (" <<service<< ") CORE: " <<  "RETURN TO: " <<  sba_tile.service_manager.subtask_list.return_to(current_subtask()) << "\n";
            cout << "LET (" <<service<< ") CORE: " <<  "RETURN AS: " <<  sba_tile.service_manager.subtask_list.return_as(current_subtask()) << "\n";
            cout << "LET (" <<service<< ") CORE: " <<  "CALLED AS: " <<  sba_tile.service_manager.subtask_list.called_as(current_subtask()) << "\n";
#endif // VERBOSE
            bool last=false;
            uint         nargs=n_args;
            uint argct=nargs;
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
                argct-=1;
                last=(argct==0);
                Word      label=sba_tile.service_manager.symbol_table[address];
#ifdef VERBOSE
				    string yn= ( last ? "Y" : "N" );
                cout << "LET (" <<service<< ") CORE: " <<  "LABEL:" <<  label << " last? " << yn << "\n";
#endif // VERBOSE
                
                if (getQuoted(label)==1){
                    label=setQuoted(label,0);
                    Word numval=sba_tile.data_store.mget(address)[0];
                    label=setStatus(label,DS_requested);
                    sba_tile.service_manager.subtask_list.incr_nargs_absent(current_subtask());
                    sba_tile.service_manager.symbol_table[address]=label;
                    if (getKind(numval)!=K_R                  ){
                    }
#ifdef VERBOSE
                        cout << "LET (" <<service<< ") CORE: BLOCKED " <<current_subtask()<< ""<<endl;
#endif // VERBOSE
                    sba_tile.service_manager.subtask_list.status(current_subtask(),STS_blocked);
                    To_t to=getSNId(numval);
                    Return_to_t return_to=S_ServiceCore_LET;
                    Word var_label = setSubtask(label,address);
					var_label = setName(var_label,S_ServiceCore_LET);
                    Word return_as=var_label;
                    Word ack_to=0;
                    Packet_Type packet_type=P_reference;
                    Ctrl_t prio=0;
                    Redir_t redir=0;
                    Word_List reslist;
                    reslist.push_back(numval);
                    Length_t payload_length=1;

                bool use_redir=false;
                    use_redir=true;

                    if ((last and use_redir)      ){
                        core_status=CS_managed;
                        Word send_ack_to=setName(label,S_ServiceCore_LET);
                        send_ack_to=setSubtask(send_ack_to,address);
                        ack_ok=0;
                        if (method!=M_ServiceCore_LET_lettc                       ){
                            sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask(),1);
                        }
                        return_as=sba_tile.service_manager.subtask_list.return_as(current_subtask());
                        return_to=sba_tile.service_manager.subtask_list.return_to(current_subtask());
                        Header_t ref_packet_header = mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as);
                        Word_List  ref_packet_payload=reslist;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
                        if (to!=S_ServiceCore_LET){

                            sba_tile.transceiver->tx_fifo.push_back(ref_packet);
                        } else {
#if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
#else // SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet);
#endif // SEQVM
                        }
                    } else {
#if VERBOSE==1                        
                        cout << "NOT LAST arg, sequencing"<<endl;
#endif // VERBOSE 
                        core_status=CS_managed;
                        Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,ack_to,return_as);
                        Word_List  ref_packet_payload=reslist;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
#if VERBOSE==1
                        cout << ppPacket(ref_packet)<<endl;
#endif // VERBOSE 
                        if (to!=S_ServiceCore_LET){
                            sba_tile.transceiver->tx_fifo.push_back(ref_packet);
                        } else {
#if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
#else // SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet);
#endif // SEQVM                            
                        }
                    } // of if last
                    if ( service!=M_ServiceCore_LET_lettc               ){
                        result( reslist );
                    } else { 
                        break;
                    }    
                } // of if quoted
            } // of for
                    
            MemAddress labeladdr=addresses[nargs-1];
            Word_List result=sba_tile.data_store.mget(labeladdr);
            core_return_type=P_data;
            sba_tile.service_manager.subtask_list.status(current_subtask(),STS_processing);
            if (sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask())==1){
#ifdef VERBOSE
                cout << "LET CORE: ACK REDIR: " <<sba_tile.service_manager.subtask_list.redir(current_subtask())<< ""<<endl;
#endif // VERBOSE
                ack_ok=1;
                core_status=CS_managed;
                sba_tile.service_manager.subtask_list.status(current_subtask(),STS_processed);
                sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask(),0);
            }

            
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress labeladdr=*iter_;
#ifdef VERBOSE
                cout << "LET-ASSIGN\tFound " <<labeladdr<< ""<<endl;
#endif // VERBOSE
                Word_List var_label_l=sba_tile.data_store.mget(labeladdr);
                    if (var_label_l.size()>0        ){
                    Word     var_label=var_label_l[0];
                    if (getKind(var_label)==K_L){
                        MemAddress  var_address=getSubtask(var_label);
                        Name_t  var_name=getName(var_label);
                    
                        bool has_label=false;
                        
                        if (sba_tile.lookup_table.count(var_name)==1){
                            Word word=sba_tile.lookup_table.read(var_name);
                            if (getSubtask(word)==var_address){
                                has_label=true;
                                sba_tile.lookup_table.erase(var_name);
                            }
                        }
                        
                        if (has_label==true){
                            sba_tile.service_manager.data_address_stack.push_back(var_address);
                        }                                        
                    }
                    }                        
                    
            }
            result( result );
        } else {
            Word result=0;
             #ifndef STATIC_ALLOC
            List<Word_List>             value_list;
             #else
             List<Word_List,MAX_NARGS> value_list;
             #endif
            for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
            	MemAddress address=*iter_;
#ifdef VERBOSE
                    Word_List try_array=sba_tile.data_store.mget(address);
                    cout << "LET (" <<service<< ") CORE: " <<address<< "\t" <<ppPayload(try_array)<< "\n";
#endif // VERBOSE
                    value_list.push_back(sba_tile.data_store.mget(address));
            }  
            Word_List  var=value_list[0];
            Word var_name=var[0];
            result=var_name;
             switch (method) {
             case M_ServiceCore_LET_assign                  :
             {
                Word sym=arg(0);
                Word val=arg(1);
                cout << "ASSIGN: " <<ppSymbol(sym)<< " => " <<ppSymbol(val)<< ""<<endl;
                
                Word var_label=sym;
                Name_t var_name=getName(var_label);
                bool has_label=false;
                MemAddress var_address=0;
                
                if (sba_tile.lookup_table.count(var_name)==1){
                    Word word = sba_tile.lookup_table.read(var_name);
                    has_label=true;
                    var_address= getSubtask(word);
                }                    
                if (has_label==false){
                    var_address=sba_tile.service_manager.data_address_stack.pop();
                    Word word=setSubtask(var_label,var_address);
                    word=setStatus(word,DS_present);
#ifdef VERBOSE
                    cout << "ASSIGN: STS=" <<sba_tile.service_manager.subtask_list.status(current_subtask())<< ""<<endl;
#endif // VERBOSE
                    sba_tile.lookup_table.write(getName(word),word);
                     Word var_value=arg(1);
#ifdef VERBOSE
                    cout << "ASSIGN: storing" << "\n" << ppPayload(var_value) << "\n@ " <<var_address<< ""<<endl;
#endif // VERBOSE
                    sba_tile.data_store.mputWord(var_address,var_value);
                    result=word;
#ifdef VERBOSE
                } else {

                    cout << " [WARNING: overwriting <" <<var_label<< "> (" << "\n" << ppPayload(sba_tile.data_store.mget(var_address)) << "\n)] "<<endl;
#endif // VERBOSE
                }
                 Word_List result_list; result_list.push_back(result); result(result_list);
              break;
             }
             case M_ServiceCore_LET_update :
             {

                MemAddress sym_address=addresses[0];
                Word var_label=sba_tile.service_manager.symbol_table[sym_address];
                var_name=getName(var_label);
                bool has_label=false;
                MemAddress var_address=0;
                if (sba_tile.lookup_table.count(var_name)==1){
                    Word word=sba_tile.lookup_table.read(var_name);
                    has_label=true;
                    var_address= getSubtask(word);
                }                
                Word_List result_list;
                if (has_label==true){
                    MemAddress newval_address=addresses[1];
                    Word_List newval= sba_tile.data_store.mget(newval_address);
                    sba_tile.data_store.mput(var_address,newval);
                    result_list=newval;
                } else {
                    sba_tile.service_manager.subtask_list.status(current_subtask,STS_pending);
                    core_status=CS_managed;
                }
                result(result_list);
              break;
             }
             case M_ServiceCore_LET_read :
             {

                MemAddress sym_address=addresses[0];
                Word var_label=sba_tile.service_manager.symbol_table[sym_address];
                var_name=getName(var_label);
                bool has_label=false;
                MemAddress var_address=0;
                
                if (sba_tile.lookup_table.count(var_name)==1){
                    Word word=sba_tile.lookup_table.read(var_name);
                    has_label=true;
                    var_address= getSubtask(word);
                }                              
                Word_List   result_list;
                if (has_label==true){
                    Word_List var_value=sba_tile.data_store.mget(var_address);
                    result_list=var_value;
#ifdef VERBOSE
					cout << "READ: returning" << "\n" << ppPayload(result_list)<<endl;
#endif // VERBOSE
                } else {
                    sba_tile.service_manager.subtask_list.status(current_subtask,STS_pending);
                    core_status=CS_managed;
                }                
                result(result_list);
        break;}
        default: std::cerr<< "Service "<<service<< " has no implementation\n";exit(0);
            } // of non-LET services case block
           
#ifdef VERBOSE
            cout <<  ") => ";
            cout << ppSymbol(result) << "\n";
#endif // VERBOSE
            core_return_type=P_data;
            sba_tile.service_manager.subtask_list.status(current_subtask,STS_cleanup);
              Word_List result_list; result_list.push_back(result); result(result_list);
 } // of not M_ServiceCore_LET_let
    } // of ls_LET        
#endif // 0
#if 0 // SKIP ls_SEQ

void Base::ServiceCore::ls_SEQ(Base::ServiceCore* ptr) {
    // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);

    MemAddresses& addresses=addresses();
    Word     service_word=sba_tile.service_manager.subtask_list.called_as(current_subtask());
    Name_t service=getName(service_word);
    core_return_type=P_data;
#ifdef VERBOSE
     int ppservice=(int)service;
    cout << "SEQ (" <<service<< ") CORE: " <<current_subtask<< ": (" <<ppservice<< "<>" <<M_ServiceCore_SEQ_seq<< "\n";
    cout << "SEQ (" <<service<< ") CORE: " <<  "TO: " <<  sba_tile.service_manager.subtask_list.to(current_subtask()) << "\n";
    cout << "SEQ (" <<service<< ") CORE: " <<  "RETURN TO: " <<  sba_tile.service_manager.subtask_list.return_to(current_subtask()) << "\n";
    cout << "SEQ (" <<service<< ") CORE: " <<  "RETURN AS: " <<  sba_tile.service_manager.subtask_list.return_as(current_subtask()) << "\n";
    cout << "SEQ (" <<service<< ") CORE: " <<  "CALLED AS: " <<  sba_tile.service_manager.subtask_list.called_as(current_subtask()) << "\n";
#endif // VERBOSE
        bool last=false;
        uint         nargs=n_args;
        uint argct=nargs;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
            argct-=1;
            last=(argct==0);
            Word      label=sba_tile.service_manager.symbol_table[address];
#ifdef VERBOSE
            cout << "SEQ (" <<service<< ") CORE: " <<  "LABEL:" <<  label << " last?" << last << "\n";
#endif // VERBOSE
            if (getQuoted(label)==1){
                label=setQuoted(label,0);
                Word numval=sba_tile.data_store.mget(address)[0];
                label=setStatus(label,DS_requested);
                sba_tile.service_manager.subtask_list.incr_nargs_absent(current_subtask());
                sba_tile.service_manager.symbol_table[address]=label;
                if (getKind(numval)!=K_R                  ){
                }
#ifdef VERBOSE
                    cout << "SEQ (" <<service<< ") CORE: BLOCKED " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                sba_tile.service_manager.subtask_list.status(current_subtask,STS_blocked);
                To_t to=getName(numval);
                Return_to_t return_to=S_ServiceCore_SEQ;
                Word var_label = setSubtask(label,address);
                var_label = setName(var_label,S_ServiceCore_SEQ);
                Word return_as=var_label;
                Word ack_to=0;
                Packet_Type packet_type=P_reference;
                Ctrl_t prio=0;
                Redir_t redir=0;
                Word_List reslist;
                reslist.push_back(numval);
                Length_t payload_length=1;
                bool use_redir=true;
                if ((last and use_redir)      ){
                    core_status=CS_managed;
                    Word send_ack_to=setName(label,S_ServiceCore_SEQ);
                    send_ack_to=setSubtask(send_ack_to,address);
                    ack_ok=0;
                    if (service!=M_ServiceCore_SEQ_seqtc                        ){
                        sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask,1);
                    }
                    return_as=sba_tile.service_manager.subtask_list.return_as(current_subtask());
                    return_to=sba_tile.service_manager.subtask_list.return_to(current_subtask());
                    Header_t ref_packet_header = mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as);
                    Word_List  ref_packet_payload=reslist;
                    Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
                    if (to!=S_ServiceCore_SEQ){
                        sba_tile.transceiver.tx_fifo.push_back(ref_packet);
                    } else {
#if SEQVM==0
                        sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
#else // SEQVM==1
                        sba_tile.service_manager.activate_subtask(ref_packet);
#endif // SEQVM
                    }
                } else {
                    core_status=CS_managed;
                    Header_t ref_packet_header = mkHeader(packet_type,prio,redir,payload_length,to,return_to,ack_to,return_as);
                    Word_List  ref_packet_payload=reslist;
                    Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
                    if (to!=S_ServiceCore_SEQ){
                        sba_tile.transceiver.tx_fifo.push_back(ref_packet);
                    } else {
#if SEQVM==0
                        sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
#else // SEQVM==1
                        sba_tile.service_manager.activate_subtask(ref_packet);
#endif // SEQVM                            
                    }
                } // of if last
                if ( service!=M_ServiceCore_SEQ_seqtc                    ){
                    result(reslist);
                } else { 
                    break;
                }    
            } // of if quoted
        } // of for
                
        MemAddress labeladdr=addresses[nargs-1];
        Word_List result=sba_tile.data_store.mget(labeladdr);
        core_return_type=P_data;
        sba_tile.service_manager.subtask_list.status(current_subtask,STS_processing);
        if (sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask())==1){
#ifdef VERBOSE
            cout << "SEQ CORE: ACK REDIR: " <<sba_tile.service_manager.subtask_list.redir(current_subtask())<< ""<<endl;
#endif // VERBOSE
            ack_ok=1;
            core_status=CS_managed;
            sba_tile.service_manager.subtask_list.status(current_subtask,STS_processed);
            sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask,0);
        }
        
#ifdef VERBOSE
        cout <<  ") => ";
        cout << ppSymbol(result[0]) << "\n";
#endif // VERBOSE
        core_return_type=P_data;
        sba_tile.service_manager.subtask_list.status(current_subtask,STS_cleanup);
        result(result);
} // of ls_SEQ       
#endif // SKIP ls_SEQ

#if 0 // SKIP ls_LAMBDA

 void Base::ServiceCore::ls_LAMBDA(Base::ServiceCore* ptr) {

        // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
        MemAddresses&  addresses=addresses();
#ifdef VERBOSE
        cout << "LAMBDA CORE (" <<current_subtask<< "): \n";
#endif // VERBOSE
        Word_List result;
        Word_List value_list;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
                Word tval=sba_tile.data_store.mget(address)[0];
                value_list.push_back(tval);
        }
        for(Word_List::iterator iter_=value_list.begin();iter_!=value_list.end();iter_++) {
        	Word value=*iter_;
            result.push_back(value);
        }
#ifdef VERBOSE
        cout << "\nLAMBDA CORE (" <<current_subtask<< "): result:" << "\n" << ppPayload(result)<<endl;
#endif // VERBOSE
        result(result);
    } // of LAMBDA

#endif // SKIP ls_LAMBDA

#if 0 // SKIP ls_NEW_LAMBDA

 void Base::ServiceCore::ls_NEW_LAMBDA(Base::ServiceCore* ptr) {

        // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
        MemAddresses&  addresses=addresses();
#ifdef VERBOSE
        cout << "LAMBDA CORE (" <<current_subtask<< "): \n";
#endif // VERBOSE
        Word_List result;
        Word_List value_list;
        for(MemAddresses::iterator iter_=addresses.begin();iter_!=addresses.end();iter_++) {
        	MemAddress address=*iter_;
                Word tval=sba_tile.data_store.mget(address)[0];
                value_list.push_back(tval);
        }
        for(Word_List::iterator iter_=value_list.begin();iter_!=value_list.end();iter_++) {
        	Word value=*iter_;
            result.push_back(value);
        }
#ifdef VERBOSE
        cout << "\nLAMBDA CORE (" <<current_subtask<< "): result:" << "\n" << ppPayload(result)<<endl;
#endif // VERBOSE
        result(result);
    } // of ls_NEW_LAMBDA




#endif // SKIP ls_NEW_LAMBDA

#if 0 // SKIP ls_APPLY

 void Base::ServiceCore::ls_APPLY(Base::ServiceCore* ptr) {

            // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
        MemAddresses& addresses=addresses();
    	Uint   method=opcode;
    	 bool use_redir; bool use_unique;
        if (method==M_ServiceCore_APPLY_apply){
#ifdef VERBOSE
            cout << "APPLY CORE (" <<current_subtask<< "): \n";
#endif // VERBOSE
            use_redir=true;
#if SEQVM==0            
            use_unique=true;
#else // SEQVM==1
              use_unique=false;
#endif // SEQVM
        } else if (method==M_ServiceCore_APPLY_applytc){
#ifdef VERBOSE
            cout << "APPLY TAILCALL"<<endl;
#endif // VERBOSE
            use_redir=false;
            use_unique=false;
        } else {
            cerr << "CORE APPLY: no such method " <<method<< "";
            exit(1);
        }
         if (sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask())==1){
            if (lookup_table.count(current_subtask()) ){
                Word lref=lookup_table.read(current_subtask());
#ifdef VERBOSE
                cout << "APPLY: remove " <<lref<< " for " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                lookup_table.erase(current_subtask());
                lookup_table.erase(lref);
            }
            ack_ok=1;
            core_status=CS_managed;
#ifdef VERBOSE
            cout << "WARNING: core_status=CS_managed; returning empty list"<<endl;
#endif // VERBOSE
             Word_List empty_list; result(empty_list);
        } else {
            bool unique=true;
            Word result=0;
            Word_List  lambda_function_args;
            MemAddress lambda_def_address=addresses[0];
            Word lambda_label=sba_tile.service_manager.symbol_table[lambda_def_address];
#ifdef VERBOSE
            Kind_t data_kind=getKind(lambda_label);
            Task_t  data_status=getTask(lambda_label);
#endif // VERBOSE
            Word_List    lambda_function=sba_tile.data_store.mget(lambda_def_address);
#ifdef VERBOSE
            cout << "LAMBDA FUNCTION:" << "\n" << ppPayload(lambda_function)<<endl;
                cout << "LEN: " <<lambda_function.size()<< ""<<endl;
#endif // VERBOSE
            if (lambda_function.size()>1   ){
                uint ext=0;
                for(Word_List::iterator iter_=lambda_function.begin();iter_!=lambda_function.end();iter_++) {
                	Word itemw=*iter_;
                     if (ext==0) {
                    ext=getExt(itemw);
                    if (getKind(itemw) == K_S ){
                        cerr << "APPLY CORE: ERROR: LAMBDA definition list args can only be U or R";
                        exit(1);
                    }
                    if (getKind(itemw) == K_A){
#ifdef VERBOSE
                        cout << "ARG: " <<ppSymbol(itemw)<< ""<<endl;
#endif // VERBOSE
                        lambda_function_args.push_back(itemw);
                    } else if (getKind(itemw)==K_R){
#ifdef VERBOSE
                            cout << "REF: " <<ppSymbol(itemw)<< ""<<endl;
#endif // VERBOSE
                        ext=0;
                        if (lookup_table.count(itemw)==0){

#ifdef VERBOSE
                            cout << "APPLY: activating code " <<ppSymbol(itemw)<< " for " <<current_subtask<< ""<<endl;
#endif // VERBOSE
                            lookup_table.write(current_subtask,itemw);
                            lookup_table.write(itemw,current_subtask);
                        } else {
                            unique=false;
                        }
                        break;
                    }
                     } // do "next"
                }
            }                      
            
            if (use_unique and not unique ){
                Word_List   result_list;
#ifdef VERBOSE
                cout << "APPLY CORE: DEFER: task is not unique, setting subtask <" <<current_subtask<< "> to " <<STS_pending<< ""<<endl;
#endif // VERBOSE
                sba_tile.service_manager.subtask_list.status(current_subtask,STS_pending);
                core_status=CS_managed;
                result(result_list);
            } 
#ifdef VERBOSE
         int pp_data_kind=(int)data_kind;
         int pp_data_status=(int)data_status;
        cout <<  "APPLY CORE: " <<current_subtask<< ": CALLER ARG: (" <<pp_data_kind<< ") " <<ppSymbol(lambda_label)<< "=>" <<lambda_def_address<< "=> <" <<pp_data_status<< ">\n";
        cout <<  "APPLY CORE: " <<current_subtask<< ": CALLER VAL: " << ppPayload(sba_tile.data_store.mget(lambda_def_address)) << "\n";
#endif // VERBOSE
#ifdef VERBOSE
    cout << "APPLY CORE: " << "\nREWRITING R-code\n";
#endif // VERBOSE
    uint root_ref=1;
    for(Word_List::iterator iter_=lambda_function.begin();iter_!=lambda_function.end();iter_++) {
    	Word ref_symbol_word=*iter_;
         if(getKind(ref_symbol_word)==K_R) {                

        if (root_ref==1 ){
            result=ref_symbol_word;
            root_ref=0;
        }
#ifdef VERBOSE
        cout << "APPLY CORE: code for " <<ppSymbol(ref_symbol_word)<< ""<<endl;
#endif // VERBOSE
        CodeAddress lambda_function_definition_address=getCodeAddress(ref_symbol_word);
#ifdef VERBOSE
        cout << "APPLY CORE: CODE ADDRESS: " <<lambda_function_definition_address<< ""<<endl;
#endif // VERBOSE
        Word_List lambda_function_definition=sba_tile.code_store.mget(lambda_function_definition_address);
#ifdef VERBOSE
        cout << "LAMBDA FUNCTION PACKET PAYLOAD:" << "\n" << ppPayload(lambda_function_definition)<<endl;
#endif // VERBOSE
        Word_List  appl_function;
        uint ext=0;
        uint ii=0;
#ifdef VERBOSE
            cout << "LAMBDA FUNCTION LEN: " <<lambda_function_definition.size()<< ""<<endl;
#endif // VERBOSE
        for(uint i=0;i<=lambda_function_definition.size()-1 ;i++) {
            Word symbol_word=lambda_function_definition[i];
            if (ext==0){
                if ( getKind(symbol_word)!=K_A){
                    appl_function.push_back(symbol_word);
#ifdef VERBOSE
                    cout << "X:appl_function[" <<ii<< "]: " <<ppSymbol(symbol_word)<< ""<<endl;
#endif // VERBOSE
                } else {      
                    for(uint j=0;j<=lambda_function_args.size()-1 ;j++) {
                        if (setQuoted(symbol_word,0) == setQuoted(lambda_function_args[j],0)){
                            uint first=1;
                            MemAddress data_address=addresses[j+1];
                            Word_List                                     symbol_word_list=sba_tile.data_store.mget(data_address);
                            for(Word_List::iterator iter_=symbol_word_list.begin();iter_!=symbol_word_list.end();iter_++) {
                            	Word val_word=*iter_;
                                if (first==1){
                                    Word newsym=setQuoted(val_word,getQuoted(symbol_word)|getQuoted(val_word));
                                    appl_function.push_back(newsym);
                                    first=0;
                                } else {
                                    appl_function.push_back(val_word);
                                }                             
#ifdef VERBOSE
                                cout << "U:appl_function[" <<ii<< "]: " <<appl_function[appl_function.size()-1]<< ""<<endl;
#endif // VERBOSE
                                ii=ii+1;
                            }
                            ii=ii-1;
                        } 
                    }
                }                                   
                if (getExt(symbol_word)==1 and (getKind(symbol_word)==K_B ) ){
                    ext=getSubtask(symbol_word);
#ifdef VERBOSE
                    cout << "EXT: " <<ppSymbol(symbol_word)<< " => ext=" <<ext<< ""<<endl;
#endif // VERBOSE
                     ii=ii-1;
                }      
            } else {
                ext-=1;
                ii=ii+1;
                appl_function.push_back(lambda_function_definition[i]);
#ifdef VERBOSE
                cout << "E(" <<ext<< "):appl_function[" <<ii<< "]: " <<ppSymbol(appl_function[appl_function.size()-1])<< ""<<endl;
#endif // VERBOSE
            }                      
            ii=ii+1;
        } 
        uint plength=appl_function.size();
        Header_t code_packet_header = mkHeader(P_code,0,0,plength,0,0,0,0);
        code_packet_header=setTo(code_packet_header,getName(ref_symbol_word));
        code_packet_header=setReturn_as(code_packet_header,ref_symbol_word);
        Word_List code_packet_payload=appl_function;
        Packet_t code_packet = mkPacket(code_packet_header,code_packet_payload);
#ifdef VERBOSE
        cout << "APPLY CORE: CODE PACKET" << "\n" << ppPacket(code_packet)<<endl;
#endif // VERBOSE
        sba_tile.transceiver->tx_fifo.push_back(code_packet);
         } // to emulate next
    }  // of for               
    Symbol_t result_symbol = mkSymbol(K_B,0,0,1,0,0,0);
            
            if (use_redir){
#ifdef VERBOSE
                        cout << "APPLY CORE REDIR/wait for ACK"<<endl;
#endif // VERBOSE
                        Word send_ack_to=setName(lambda_label,S_APPLY);
                        send_ack_to=setSubtask(send_ack_to,lambda_def_address);
                        ack_ok=0;
                        sba_tile.service_manager.symbol_table[lambda_def_address]=setStatus(lambda_label,DS_requested);
                        sba_tile.service_manager.subtask_list.status(current_subtask,STS_blocked);
                        sba_tile.service_manager.subtask_list.incr_nargs_absent(current_subtask());
                        sba_tile.service_manager.subtask_list.waiting_for_ack(current_subtask,1);
                        core_status=CS_managed;
                        Packet_Type packet_type=P_reference;
                        Ctrl_t prio=0;
                        Length_t payload_length=1;
                         Word_List result_list;result_list.push_back(result);
                        Name_t to=getName(result);
                        Return_to_t return_to=sba_tile.service_manager.subtask_list.return_to(current_subtask());
                        Word return_as=sba_tile.service_manager.subtask_list.return_as(current_subtask());
                        send_ack_to=setName(lambda_label,S_APPLY);
                        send_ack_to=setSubtask(send_ack_to,lambda_def_address);
                        Header_t ref_packet_header = mkHeader(packet_type,prio,1,payload_length,to,return_to,send_ack_to,return_as);
                        Word_List  ref_packet_payload=result_list;
                        Packet_t ref_packet = mkPacket(ref_packet_header,ref_packet_payload);
#ifdef VERBOSE
                        cout << ppPacket(ref_packet)<<endl;
#endif // VERBOSE
                        if (to!=S_APPLY){
                            sba_tile.transceiver->tx_fifo.push_back(ref_packet);
                        } else {
#if SEQVM==0
                            sba_tile.service_manager.subtask_reference_fifo.push_back(ref_packet);
#else // SEQVM==1
                            sba_tile.service_manager.activate_subtask(ref_packet);
#endif // SEQVM
                        }
                        result_symbol=setSubtask(result_symbol,1);
            }            
            if (not use_redir                                                 ){
                core_return_type= P_reference;
                result_symbol=result;
                sba_tile.service_manager.subtask_list.to(current_subtask,getName(result_symbol));

            }   
             Word_List result_symbol_l;result_symbol_l.push_back(result_symbol);result(result_symbol_l);
    } // of ls_APPLY
#endif // SKIP ls_APPLY

#if 0 // SKIP ls_NEW_APPLY

 void Base::ServiceCore::ls_NEW_APPLY(Base::ServiceCore* ptr) {
            // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
            Word result=0;
            Word result_symbol=NULL;
			if (nargs() == 1){
				result_symbol=arg(0);
			} else {

            Word_List lambda_function_args;
                 

            Word lambda_ref=arg(0);
			CodeAddress lambda_code_address=getCodeAddress(lambda_ref);
	        Word_List lambda_code=sba_tile.code_store.mget(lambda_code_address);
			
            if (lambda_code.size()>2   ){
                for(Word_List::iterator iter_=lambda_function.begin();iter_!=lambda_function.end();iter_++) {
                	Word itemw=*iter_;
                    if (getKind(itemw) == K_A){
                        lambda_function_args.push_back(itemw);
                    } else if (getKind(itemw)==K_R){
                        break;
                    }
                     } // do "next"
                }
			} else if (lambda_code.size()>1){
			} else {
            }                      
            
     
    uint root_ref=1;
    for(Word_List::iterator iter_=lambda_function.begin();iter_!=lambda_function.end();iter_++) {
    	Word ref_symbol_word=*iter_;
         if(getKind(ref_symbol_word)==K_R) {                

        if (root_ref==1 ){
            result=ref_symbol_word;
            root_ref=0;
        }
        CodeAddress lambda_function_definition_address=getCodeAddress(ref_symbol_word);
        Word_List lambda_function_definition=sba_tile.code_store.mget(lambda_function_definition_address);
        Word_List  appl_function;
        uint ii=0;
        for(uint i=0;i<=lambda_function_definition.size()-1 ;i++) {
            Word symbol_word=lambda_function_definition[i];
                if ( getKind(symbol_word)!=K_A){
                    appl_function.push_back(symbol_word);
                } else {      
                    for(uint j=0;j<=lambda_function_args.size()-1 ;j++) {
                        if (setQuoted(symbol_word,0) == setQuoted(lambda_function_args[j],0)){
                            uint                             first=1;
                            Word_List                                     symbol_word_list=arg(j+1);
                            for(Word_List::iterator iter_=symbol_word_list.begin();iter_!=symbol_word_list.end();iter_++) {
                            	Word val_word=*iter_;
                                if (first==1){
                                    Word newsym=setQuoted(val_word,getQuoted(symbol_word)|getQuoted(val_word));
                                    appl_function.push_back(newsym);
                                    first=0;
                                } else {
                                    appl_function.push_back(val_word);
                                }                             
                                ii=ii+1;
                            }
                            ii=ii-1;
                        } 
                    }
                }                                   
            }                      
            ii=ii+1;
         
        uint plength=appl_function.size();
        Header_t code_packet_header = mkHeader(P_code,0,0,plength,0,0,0,0);
        code_packet_header=setTo(code_packet_header,getName(ref_symbol_word));
        code_packet_header=setReturn_as(code_packet_header,ref_symbol_word);
        Word_List code_packet_payload=appl_function;
        Packet_t code_packet = mkPacket(code_packet_header,code_packet_payload);
        sba_tile.transceiver.tx_fifo.push_back(code_packet);
         } // to emulate next
    }  // of for               
        core_return_type= P_reference;
        result_symbol=result;
        sba_tile.service_manager.subtask_list.to(current_subtask,getName(result_symbol));
		}
         Word_List result_symbol_l;result_symbol_l.push_back(result_symbol);result(result_symbol_l);   
    } // of ls_NEW_APPLY
#endif // SKIP ls_NEW_APPLY

#if 0
 void Base::ServiceCore::ls_IO() {

        // Set up context
ServiceCore* servicecore_ptr=(ServiceCore*)ptr;
ServiceCore& *servicecore_ptr;
System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
System& sba_system=*sba_system_ptr;
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);

        core_return_type=P_data;
        Word port_symbol=arg(0);
        Word builtin_symbol=EXTSYM;
        Word port = port_symbol & 255;
         Word_List eof; eof.push_back(builtin_symbol); eof.push_back((Word)(0));      
         Word_List pass; pass.push_back(builtin_symbol); pass.push_back((Word)(1));
#if WORDSZ==64
         Word min1=(Uint64)(-1);        
#else // WORDSZ==32
         Word min1=(Uint32)(-1);
#endif // WORDSZ 
    Word_List fail; fail.push_back(builtin_symbol); fail.push_back(min1);
    FILE* fd;
        uint method=method();
        if (method==M_ServiceCore_IO_open){
            Word filename_bytes=arg(1);
			Word_List filename_bytes_wl;
			// FIXME! THIS IS BROKEN! strings should be handled using pointers!
			filename_bytes_wl.push_back(filename_bytes);
			string filename=sym2str(filename_bytes_wl);
#ifdef VERBOSE
            cout << "CORE FOPEN: HANDLE: " <<port<< " FILENAME: " <<filename<< ""<<endl;
#endif // VERBOSE
            if (nargs()==3){
                Word flag_bytes=arg(2);

                string flag="";//sym2str(flag_bytes); // FIXME!
                 fd=fopen(filename.c_str(),flag.c_str());
            } else {
                 fd=fopen(filename.c_str(),"r");
            }
             lookup_table.write(port,(Uint64)fd);
            resultWord(port_symbol);
        } else if (method==M_ServiceCore_IO_close){
            if (lookup_table.count(port)){
            	 fd=(FILE*)(lookup_table.read(port));
            	 fclose(fd);
            	result(pass);
            } else {
            	result(fail);
            }        
        } else if (method==M_ServiceCore_IO_readline ){
            if (nargs() == 2){
                Word nbytes=arg(1);
//                Word nbytes=getWord(nbytes_wl); // FIXME: CHECK IF THIS IS OK!
                if (nbytes!=0 ){
                    std::cerr <<  "CORE IOREAD: reading chunks of " <<nbytes<< " bytes is not yet implemented";
                } 
            }
             char* nil=NULL;
             char inp[255]; // the 255 is very ad-hoc
            if ((not lookup_table.count(port))){
            	 std::cerr << port << ": ";
                 fgets(inp,255,stdin);
            } else {
                 fd=(FILE*)(lookup_table.read(port));
                if (not feof(fd) ){
                     fgets(inp,255,fd);
                }
            }
             
            if (inp!=nil){
                Word_List  inp_sym = string2symbol(inp);
                result(inp_sym);
            } else {
                 Word_List emptysymbol;emptysymbol.push_back(NIHIL);
                result(emptysymbol);
            }
        } else if (method==M_ServiceCore_IO_write                   ){
            Word data_symbol = arg(1);
             string data;            
            Kind_t kind=getKind(data_symbol);
            Datatype_t datatype=getDatatype(data_symbol);
			uint nwords =  getSubtask(data_symbol);
			uint ext =  getExt(data_symbol);
            if ((kind==K_B and ext==1 and nwords==0) ){
                result(eof);
            } else {
                 if ((not lookup_table.count(port))){
                     result(fail);
                 } else {            
                      FILE* fd=(FILE*)(lookup_table.read(port));
					if (kind==K_B){
                    if (datatype==T_s or (WORDSZ==32 and nwords>1) ){
                        string data="";// FIXME!!! sym2str(data_symbol);
                         fprintf(fd,"%s",data.c_str());
                    } else if (datatype==T_i){
                        Int            data=0;// FIXME!!! sym2int(data_symbol);
                         fprintf(fd,"%d",(int)data);
                    } else if ( datatype==T_f){
                        Float             data=0.0;// FIXME!!! sym2flt(data_symbol);
                         fprintf(fd,"%f",data);
                    } else {
                        cerr << "CORE WRITE: datatype " <<datatype<< "";
                        exit(1);
                    }          
					} else {
					}
                     result(pass);
                 }
             }
        } else if (method==M_ServiceCore_IO_eof){
             Word_List res;
            if (feof(fd) ){
                 res.push_back(ONE); 
            } else {
                 res.push_back(ZERO);
            }
            result( res );
        } else if (method==M_ServiceCore_IO_display){

            string result="";
            for(uint argn=0;argn<=nargs()-1 ;argn++) {
				Word data=arg(argn);
                if (1) { // FIXME!!! (data.size()==1 || data.size()==2) {
                switch (getDatatype(data)) {
    				case T_i:
                    #ifdef VERBOSE
                        std::cout << ">>>Int: ";
    				#endif
    				    std::cout << getInt(data) << std::endl;
    				    break;
				    case T_f:
#ifdef VERBOSE
                        std::cout << ">>>Float: ";
#endif
                        std::cout << getFloat(data) << std::endl;
                        break;
                    case T_c:
#ifdef VERBOSE
                        std::cout << ">>>Char: ";
#endif
// FIXME!!!                        std::cout << getChar(data) << std::endl;
                        break;
                    case T_s:
#ifdef VERBOSE
                        std::cout << ">>>String: ";
#endif
// FIXME!!!                        std::cout << getString(data) << std::endl;
                        break;				
                    default:
						std::cout << data << std::endl;
/* FIXME!!                        std::cout << data[0] << std::endl;
                        if (data.size()==2) {
                            std::cout << data[1] << std::endl;
                            }
*/							
                    }
                } else {
/* FIXME!!
#ifndef STATIC_ALLOC
                 	for(Word_List::iterator ai=data.begin(); ai!=data.end(); ++ai) {
                 		std::cout <<">>>"<< *ai << "\n";
#else
                 	for(unsigned int ai=0;ai<data.size(); ++ai) {
               	std::cout <<">>>"<< data.back() << "\n";
                 	data.pop_back();
#endif                 		
                 		}
*/
                 	std::cout << "----------------\n";
				}

            }    			
			
            result( pass );
        } else {
            cerr << "CORE IO does not support method " <<method<< "";
            exit(1);
        }

    } // of IO
#endif // 0 for skipping
#include "ls_IO.cc"
// This is the subroutine to run a certain task in a given thread
void Base::ServiceCore::ls_RUN() {
}   

void Base::ServiceCore::none() {
    // Set up context
     Result res; res.push_back((Word)0); result(res);
}        
#endif // NO_SERVICES

// This one is generated by the build system in the inherited class
void Base::ServiceCore::select_wrapper(unsigned int code) {
	switch (code) {
		default:
			none();
	};
}
