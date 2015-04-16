
#ifndef TILE_H_
#define TILE_H_

// Tile.rb
//
// :title: Service-based SoC project - SBA Tile class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//
// $Id: Tile.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/Tile.rb by ../../util/r2n.pl on Sun Apr 22 17:13:59 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#include "./ServiceConfiguration.h"
#include "./Transceiver.h"
#include "./ServiceManager.h"
#ifndef STATIC_ALLOC
#include "./Memory.h"
#endif
#include "./LookupTable.h"
#include "Services.h"
#include "Base/Tile.h"
#include "Base/System.h"

using namespace std;
using namespace SBA;
/**
 * Ideally, each service has its own configuration.
 * Therefore, we should store the per-service confgiration in an array.
 * But for now, we use a common configuration file
 */
namespace SBA {
#if USE_THREADS==1
	//unordered_map <long int, ServiceAddress> thread_map;
    void *run_tile_loop(void*);
#endif
class Tile : public Base::Tile {
		public:
		Base::System* sba_system_ptr;
		Service service;
		ServiceAddress address;
		bool status;
		Transceiver transceiver;
		ServiceManager service_manager;
#ifndef STATIC_ALLOC
		Memory data_store;
		Memory code_store;
#else
        Store<DATA_SZ> data_store;
        Store<CODE_SZ> code_store;
#endif
        LookupTable lookup_table;

		Services service_core; //APIC api;
		bool finished;
		int core; //Ashkan (actua core id to which the Tile is mapped
	Tile(Base::System* sba_s_, Service& s_, ServiceAddress addr_)
	: sba_system_ptr(sba_s_), service(s_), address(addr_),
	status(false),
	transceiver(sba_s_,this,s_,addr_),
	service_manager(sba_s_,this,s_,addr_),
	service_core(sba_s_,this,s_,addr_,0), //api(sba_s_,this,s_,addr_,0),
	finished(false)
	 {
	 };	// END of constructor

	Tile()
	: sba_system_ptr(NULL), service(0), address(0), status(false),
	transceiver(sba_system_ptr,this,service,address),
	service_manager(sba_system_ptr,this,service,address),
	service_core(sba_system_ptr,this,service,address,0),
	finished(false)
	{
	};
/*
 Main methods
*/
	void run();

#if USE_THREADS==1
    pthread_t tid;
    pthread_attr_t attr;
    //void* tstatus;
    void run_th ();
#endif
#if DISTR==1
    void run_proc();
#endif

}; // end

} // namespace SBA


#endif // TILE_H_
