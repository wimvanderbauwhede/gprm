
#ifndef SERVICECORE_H_
#define SERVICECORE_H_

// ServiceCore.h
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
#include "Base/System.h"
#include "Base/Tile.h"
#include "Base/ServiceCore.h" 

 using namespace std;
 using namespace SBA;

 namespace SBA {
 class ServiceCore : public Base::ServiceCore {
	 public:
ServiceCore() {};
ServiceCore(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) : Base::ServiceCore(sba_s_,sba_t_,s_,addr_,tid_) {};
void select_wrapper(unsigned int code); 
 }; // class
 
 } // namespaces       

#endif // SERVICECORE_H_
