
#ifndef BASE_SERVICECORE_H_
#define BASE_SERVICECORE_H_

// SBACore.rb
//   
// :title: Gannet Service-based SoC project - Service Core Library 
//    
//
// *
// *  (c) 2004-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
//

#include <dlfcn.h>
#include <fstream>
#include <sstream>
#include "Types.h" 
#include "Packet.h" 
#include "Base/ServiceCoreControl.h" 
//#include "SBACore.h"

 using namespace std;

 namespace SBA {
 namespace Base {
 class ServiceCore : public Base::ServiceCoreControl {
	 public:		 

	ServiceCore() {};
	ServiceCore(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) : Base::ServiceCoreControl(sba_s_,sba_t_,s_,addr_,tid_) {};		 
void select_wrapper(unsigned int code);
	
	double word2dbl(Word);
 Word dbl2word(double);

 Word_List string2symbol(string);
 string extsym2str(Word);
 string sym2str(Word_List);
 string wl2str(Word_List);
 Int sym2int(Word_List);
 Word sym2uint(Word_List);
 bool sym2bool(Word_List);
 float sym2flt(Word_List);

 Int div(Int,Int);
    
 void ls_RUN();
void ls_ALU();
 
 void ls_BEGIN();
 void ls_SEQ();
 void ls_IF();
// void ls_S_IF();
 void ls_LET();

#if 0 // SKIP ls_LAMBDA
 void ls_LAMBDA(Base::ServiceCore*);
#endif // SKIP ls_LAMBDA

#if 0 // SKIP ls_NEW_LAMBDA
 void ls_NEW_LAMBDA(Base::ServiceCore*);
#endif // SKIP ls_NEW_LAMBDA

#if 0 // SKIP ls_APPLY
 void ls_APPLY(Base::ServiceCore*);
#endif // SKIP ls_APPLY

#if 0 // SKIP ls_NEW_APPLY
 void ls_NEW_APPLY(Base::ServiceCore*);
#endif // SKIP ls_NEW_APPLY
void ls_IO();

void none();
 }; // class
 
 } } // namespaces       

#endif // SBACORE_H_
