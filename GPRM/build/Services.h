// FIXME: This should now be gensrc. So I can actually replace the #include by the proper call right away ...
#ifndef SERVICES_H_
#define SERVICES_H_
#define __KERNEL_WRAPPER__
// Services.h
//   
// :title: Gannet Service-based SoC project - Application-Specific Services Library
//    
//
// *
// *  (c) 2004-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
//

#include "Types.h" 
#include "Packet.h" 
#include "Base/System.h"
#include "Base/Tile.h"
#include "Base/CoreServices.h"

using namespace std;
using namespace SBA;

namespace SBA {
    class Services : public Base::CoreServices {
        public:
            Services() {};
            Services(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) : Base::CoreServices(sba_s_,sba_t_,s_,addr_,tid_) {};
//     using Base::CoreServices::CoreServices; // C++11 constructor inheritance   
            __KERNEL_WRAPPER__
            void select_wrapper(unsigned int code); 
    }; // class
} // namespaces       

#endif // SERVICES_H_
