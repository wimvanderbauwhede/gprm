// Interface.rb
//
// :title: Gannet Service-based SoC project - Interface class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>

#include <queue>
#include "System.h"
#include "Interface.h"
#include "Timings.h"

using namespace std;
using namespace SBA;

 Bytecode Interface::read_bytecode(string tdc_file) {
#ifdef VERBOSE
	 cout << "read_bytecode("<<tdc_file<<")\n";
#endif
      if(   FILE * fd=fopen(tdc_file.c_str(),"r") ) {

         Bytecode bytewords;
        Word byteword=0;
        uint hwb=0;
         Word byte=0;
         while((Int)byte!=EOF) {
         byte=(Word)fgetc(fd);
            byteword+=(byte<<(8*(NBYTES-1-hwb)));
            hwb=hwb+1;
            if (hwb==NBYTES){
                hwb=0;
                bytewords.push_back(byteword);
                byteword=0;
            }
         }
        return bytewords;
      } else {
    	  cout << "Could not open file "<<tdc_file<<"\n";
    	  exit(-1);
      }
    }
/*
 Bytecode Interface::read_bytecode(uint status){ //H
     Bytecode bytewords;
     return bytewords;
  }

 uint Interface::receive(uint core_status) {
	 System& sba_system=*((System*)sba_system_ptr);
	 Bytecode bycl=read_bytecode(sba_system.task_description);
	 if (sba_system.task_descriptions.size()==1){
		 StringPair tdc_file=sba_system.task_descriptions.front();sba_system.task_descriptions.pop_front();
		 sba_system.task_data=tdc_file.datafile;
		 Bytecode bycl=read_bytecode(tdc_file.taskfile);

		 Bytecode bycl=read_bytecode(sba_system.task_description);
		 tdcs.push(bycl);
		return 1;
	 } else {
		return 0;
	 }
 } // of receive()

 void Interface::send(Word_List& result,uint taskid) {
     System& sba_system=*((System*)sba_system_ptr);

        if (sba_system.io_mech==0 ){
        } else {
                cout <<endl;
                if (result.size()==0){
                    cout << "Return value: []"<<endl;
                } else {
                    cout << "Return value:\t" <<getValue(result[0])<< ""<<endl;
                }
        }
    } // of send()
*/
