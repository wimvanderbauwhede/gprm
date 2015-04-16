
#ifndef GATEWAYTILE_H_
#define GATEWAYTILE_H_

// Tile.rb
//
// :title: Gannet Service-based SoC project - SBA Tile class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: GatewayTile.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/GatewayTile.rb by ../../util/r2n.pl on Sun Apr 22 17:13:59 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#ifdef WV_SYSTEMC
#include <systemc.h>
#endif

#include "./ServiceConfiguration.h"
#include "Types.h"
#include "TaskDescription.h"
#include "Gateway.h"
#ifndef STATIC_ALLOC
#include "Memory.h"
#endif
#include "Transceiver.h"
#include "Base/Tile.h"
#include "Base/System.h"

// B ashkan
#ifdef USE_TILERA
#include "sys/types.h"
#include "tmc/cpus.h"
#include "tmc/task.h"
#include "tmc/perf.h"
#include "tmc/alloc.h"
#endif
// E ashkan

using namespace std;
/**
 * Ideally, each service has its own configuration.
 * Therefore, we should store the per-service confgiration in an array.
 * But for now, we use a common configuration file
 */
namespace SBA {

#if USE_THREADS==1
    void *run_gwtile_loop(void*);
#endif

class GatewayTile : public Base::Tile {
		public:
		Service service;
		ServiceAddress address;
		int cpu; // For Tilera
		Base::System* sba_system_ptr;
		Gateway gateway;
#if DATA==1
		Memory data_packet_store;
#endif // DATA
#ifndef STATIC_ALLOC
		Memory result_store;
		Memory code_store;
#else
	    Store<DATA_SZ> result_store; // not DATA_SZ: 1 packet per task
#endif
		Transceiver transceiver;
		TaskDescList task_descriptions;
		std::string task_description;
		bool init;
		bool finished;
	GatewayTile(Base::System* sba_s_, Service s_, ServiceAddress addr_,TaskDescList& tds_)
	: service(s_), address(addr_),
	sba_system_ptr(sba_s_),
	gateway(sba_system_ptr,this,s_,addr_,tds_),
	transceiver(sba_s_,this,s_,addr_),
	task_descriptions(tds_), init(true), finished(false)
	 {};	// END of constructor

	GatewayTile(Base::System* sba_s_, Service s_, ServiceAddress addr_,std::string td_)
	: service(s_), address(addr_),
	sba_system_ptr(sba_s_),
	gateway(sba_system_ptr,this,s_,addr_,td_),
	transceiver(sba_s_,this,s_,addr_),
	task_description(td_), init(true), finished(false)
	 {};	// END of constructor

/*
 Main methods
*/
	void run();
#if USE_THREADS==1
    pthread_t th;
    pthread_attr_t attr;
    void* tstatus;
    void run_th ();
#endif
}; // end

} // namespace SBA

#endif // GATEWAYTILE_H_
