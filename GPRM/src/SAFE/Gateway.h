
#ifndef GATEWAY_H_
#define GATEWAY_H_

// Gateway.h
// 
// :title: Gannet Service-based SoC project - Gateway class
//
//  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: Gateway.rb 2557 2009-05-13 13:05:38Z socgroup $

#include "Base/Tile.h" 
#include "Base/System.h" 
#include "ServiceManagerObjects.h" 
#include "TaskDescription.h" 
#include "ServiceConfiguration.h" 
#include "Interface.h"

using namespace std;

namespace SBA {
class Gateway  {
	public:
    
        	Base::System* sba_system_ptr;
        	Base::Tile* sba_gwtile_ptr;    
    
	Service service;
	ServiceAddress address;
	Interface     vmif;
	Stack<MAX_NTASKS>     tasks_stack ;
	Core_Status core_status;
	TaskDescList tasks;
	std::string task;
	Packet_Fifo     rx_fifo; // This is different from its transceiver's rx_fifo! It should be Packet_Fifo, otherwise we would have 2 lock for one thread resulting in smashing stack error.
	Packet_Fifo request_fifo;
	Packet_Fifo tx_fifo ;

 Gateway(Base::System* sba_s_, Base::Tile* sba_t_,Service s_,ServiceAddress addr_,TaskDescList& td_) 
		:  sba_system_ptr(sba_s_), sba_gwtile_ptr(sba_t_),
		 service(s_),
        address(addr_),
        vmif(sba_s_, sba_t_),
        tasks_stack(0),
//		 task_counter(0),
		 core_status(CS_idle),
		 tasks(td_)
		  {		    
		  };

 Gateway(Base::System* sba_s_, Base::Tile* sba_t_,Service s_,ServiceAddress addr_,std::string td_)
 		:  sba_system_ptr(sba_s_), sba_gwtile_ptr(sba_t_),
 		 service(s_),
         address(addr_),
         vmif(sba_s_, sba_t_),
         tasks_stack(0),
 //		 task_counter(0),
 		 core_status(CS_idle),
 		 task(td_)
 		  {
 		  };


 void run();

 void parse_task_description();

 void receive_packets();

 void store_result_packets();
 void transmit_packet(Packet_t);
 void store_code(SBA::Packet_t);

}; // Gateway
} // namespace SBA

#endif // GATEWAY_H_
