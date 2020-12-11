 void Base::CoreServices::ls_IO() {
     // Set up context
//     CoreServices* servicecore_ptr=(CoreServices*)ptr;
//     CoreServices& *servicecore_ptr;
//     System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
//     System& sba_system=*sba_system_ptr;
//     Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);

     core_return_type=P_data;
     Word port_symbol=arg(0);
//     Word builtin_symbol=EXTSYM;
     Word port = port_symbol & 255;
//     Word_List eof; eof.push_back(builtin_symbol); eof.push_back((Word)(0));
     Word eof = ZERO;
//     Word_List pass; pass.push_back(builtin_symbol); pass.push_back((Word)(1));
     Word pass = ONE;

//     Word min1=(Uint64)(-1);
//     Word_List fail; fail.push_back(builtin_symbol); fail.push_back(min1);
     Word fail = NIHIL;
#ifdef USE_TILERA
     FILE* fd = NULL; //TODO: Ashkan: not sure if NULL is a good replacement!
#else
     FILE* fd = nullptr;
#endif
     uint method_code=method();
     if (method_code==M_CoreServices_IO_open){
         string filename=extsym2str(arg(1));
//         Word_List filename_bytes_wl;
//         // FIXME! THIS IS BROKEN! strings should be handled using pointers!
//         filename_bytes_wl.push_back(filename_bytes);
//         string filename=sym2str(filename_bytes_wl);
#ifdef VERBOSE
            cout << "CORE FOPEN: HANDLE: " <<port<< " FILENAME: " <<filename<< ""<<endl;
#endif // VERBOSE
            if (nargs()==3){
                Word flag_bytes=arg(2);
                string flag=extsym2str(flag_bytes); // FIXME!
                 fd=fopen(filename.c_str(),flag.c_str());
            } else {
                 fd=fopen(filename.c_str(),"r");
            }
             lookup_table.write(port,(Uint64)fd);
            resultWord(port_symbol);
        } else if (method_code==M_CoreServices_IO_close){
            if (lookup_table.count(port)){
            	 fd=(FILE*)(lookup_table.read(port));
            	 fclose(fd);
            	result(pass);
            } else {
            	result(fail);
            }
        } else if (method_code==M_CoreServices_IO_readline ){
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
                 char* res = fgets(inp,255,stdin);
            } else {
                 fd=(FILE*)(lookup_table.read(port));
                if (not feof(fd) ){
                     char* res = fgets(inp,255,fd);
                }
            }

            if (inp!=nil){
                Word_List  inp_sym = string2symbol(inp);
                result(inp_sym);
            } else {
                 Word_List emptysymbol;emptysymbol.push_back(NIHIL);
                result(emptysymbol);
            }
        } else if (method_code==M_CoreServices_IO_write                   ){
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
        } else if (method_code==M_CoreServices_IO_eof){
             Word res;
            if (feof(fd) ){
                 res=ONE;
            } else {
                 res=ZERO;
            }
            result( res );
        } else if (method_code==M_CoreServices_IO_display){
//            string result="";
        	std::ostringstream outs;
            for(uint argn=0;argn<=nargs()-1 ;argn++) {
				Word data=arg(argn);
                switch (getDatatype(data)) {
    				case T_i:
                    #ifdef VERBOSE
                        std::cout << ">>>Int: "<<getInt(data)<<"\n";
    				#endif
                        outs<< getInt(data) ;
    				    break;
				    case T_f:
#ifdef VERBOSE
                        std::cout << ">>>Float: "<<getFloat(data) <<"\n";
#endif
                        outs << getFloat(data) ;
                        break;
                    case T_c:
#ifdef VERBOSE
                        std::cout << ">>>Char: \n";
#endif
// FIXME!!!                        std::cout << getChar(data) << std::endl;
                        break;
                    case T_s:
#ifdef VERBOSE
                        std::cout << ">>>String: "<< extsym2str(data)<< "\n";
#endif
                        outs << extsym2str(data);
                        break;
                    default:
#ifdef VERBOSE
                        std::cout << ">>>Default: "<< data<<"\n";
#endif
						outs << data ;
                    }
            } // loop over args
#ifdef VERBOSE
    std::cout << ">>> ";
#endif

			std::cout << outs.str() << std::endl;
            result( pass );
        } else {
            cerr << "CORE IO does not support method " <<method_code<< "";
            exit(1);
        }
    } // of IO
