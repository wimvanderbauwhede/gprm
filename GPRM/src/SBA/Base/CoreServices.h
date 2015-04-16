
#ifndef BASE_CORESERVICES_H_
#define BASE_CORESERVICES_H_

// CoreServices.h
//
// :title: Gannet Service-based SoC project - Core Services Library
//
//
// *
// *  (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//

//#include <dlfcn.h>
//#include <fstream>
//#include <sstream>
#include "Types.h"
#include "Packet.h"
#include "Base/ServiceCoreControl.h"

 using namespace std;

 namespace SBA {
 namespace Base {
 class CoreServices : public Base::ServiceCoreControl {
	 public:

	CoreServices() {printf("CoreServices Constructor: %d\n",service);};
	CoreServices(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) : Base::ServiceCoreControl(sba_s_,sba_t_,s_,addr_,tid_) {};
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

 void ls_CTRL();
 void ls_ALU();
 void ls_Math();

 void ls_BEGIN();
 void ls_SEQ();
 void ls_IF();
 void ls_LET();
 void ls_APPLY();
 void ls_IO();
 void ls_REG();
 void ls_ARG();

 void none();
 }; // class

 } } // namespaces

#endif // BASE_CORESERVICES_H_
