
#ifndef SYSTEM_H_
#define SYSTEM_H_

//
// Gannet Service-based SoC project - SBA System (toplevel) class
//
// (c) 2004-2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#include <unordered_map>
#include <vector>
#include "Types.h"
#include "Packet.h"
#include "Base/System.h"
#if SEQVM==0
#include "Tile.h"
#else
#include "TileVM.h"
#endif
#if DISTR==0
#include "GatewayTile.h"
#endif
#include "SystemConfiguration.h"

using namespace std;

namespace SBA {
class System : public Base::System {
	public:

//    Config cfg; ///< System service configuration, a list defining which services are where.

	ServiceAddress gw_address;
	ServiceMap servicenodes;
//	Configurations configurations;
	uint nservicenodes;
	uint finished;
	string task_data;
	string task_description;
	TaskDescList     task_descriptions;
	Bytecode     bytecode;
//	Word_List results;
	vector<void*> args;
	vector<Word> regs;
	void* result;
	uint io_mech;
	uint multi_ip;
#if DISTR==0
	GatewayTile     gw_instance ;
#endif // DISTR
//    Tile* nodes[NSERVICES]; // This had better be a map
    unordered_map<Service,Tile*> nodes;

	System(TaskDescList& tds_) : gw_address(0),
	finished(false), task_descriptions(tds_)
	,io_mech(1)
#if DISTR==0
	,gw_instance(this,0,0,tds_)
#endif // DISTR
    {
		// allocate some space for args and regs
		for (int regno = 0; regno< MAX_REGISTERFILE_SZ; regno++) {
			regs.push_back((Word)0);
		}

		//Services
		nservicenodes=NSERVICES;
    for (Service node_id_=1;node_id_<=NSERVICES;node_id_++) {
        ServiceAddress service_address=node_id_;//SERVICE_ADDRESSES[node_id_-1];
        Service node_id = node_id_;//service_address;
        if  (service_address != 0) {
#ifdef USE_TILERA
          tmc_alloc_t alloc = TMC_ALLOC_INIT;
          int mem_num = tmc_perf_num_numa_nodes();
          //printf ("number of memory controllers visible to Linux: %d\n", mem_num);
          //tmc_alloc_set_home(&alloc, 46);
          //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_TASK);
          //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_HASH);
          tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_NONE);
          tmc_alloc_set_node_preferred(&alloc, 3);
          void* raw_tile = tmc_alloc_map(&alloc, 12345);
	  //void* raw_tile = tmc_alloc_map(&alloc, getpagesize());
          //printf ("inja, this it the sizeof raw_tile: %d\n", sizeof(raw_tile));
	  // The next line will give an error. *raw_tile is *void
	  // printf ("inja, this it the sizeof *raw_tile: %d\n", sizeof(*raw_tile));
          if (raw_tile == NULL)
              tmc_task_die("Failed to allocate memory for raw_tile.");
	  //cout << "service_address" << service_address << endl;
          if (nodes.count(node_id)==0)
              nodes[node_id] = new(raw_tile) Tile(this,node_id,service_address);
	  //printf ("this it the sizeof nodes[%d]: %d\n", service_address, sizeof(nodes[service_address]));
          //printf ("this it the sizeof *nodes[%d]: %d\n", service_address, sizeof(*nodes[service_address]));
#else
          // We can have duplicate node IDs, no need to create the node twice!
          if (nodes.count(node_id)==0)
              nodes[node_id]=new Tile(this,node_id,service_address);
          //printf ("this it the sizeof nodes[%d]: %d\n", service_address, sizeof(nodes[service_address]));
                // printf ("this it the sizeof *nodes[%d]: %d\n", service_address, sizeof(*nodes[service_address]));
#endif
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE
          }
    }

	};

	// --------------------------------------------------------------
	System(std::string td_) : gw_address(0),
	finished(false), task_description(td_), result(nullptr)
	,gw_instance(this,0,0,td_)
    {
		// allocate some space for args and regs
		for (int regno = 0; regno< MAX_REGISTERFILE_SZ; regno++) {
			regs.push_back((Word)0);
		}
		
		//Services
		nservicenodes=NSERVICES;
    for (Service node_id_=1;node_id_<=NSERVICES;node_id_++) {
        ServiceAddress service_address=node_id_;//SERVICE_ADDRESSES[node_id_-1];
        Service node_id = node_id_;//service_address;
        if  (service_address != 0) {
#ifdef USE_TILERA
          tmc_alloc_t alloc = TMC_ALLOC_INIT;
          int mem_num = tmc_perf_num_numa_nodes();
          //tmc_alloc_set_home(&alloc, 46);
          //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_TASK);
          //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_HASH);
          tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_NONE);
          tmc_alloc_set_node_preferred(&alloc, 3);
          void* raw_tile = tmc_alloc_map(&alloc, 12345);
          if (raw_tile == nullptr)
              tmc_task_die("Failed to allocate memory for raw_tile.");
          if (nodes.count(node_id)==0)
              nodes[node_id] = new(raw_tile) Tile(this,node_id,service_address);
#else
          // This check is not needed if we create contiguous instances
          // but if we use SERVICE_ADDRESSES it would be required
          if (nodes.count(node_id)==0)
              nodes[node_id]=new Tile(this,node_id,service_address);
#endif
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE
          }
    }

	};
/*
#if DISTR==0
	// --------------------------------------------------------------
	System(Bytecode& byc_) : gw_address(0),
	finished(false), bytecode(byc_), io_mech(1)
    	,gw_instance(this,0,0,task_descriptions)
    	{
	 //Services
	 nservicenodes=NSERVICES;
    	 for (Service node_id_=1;node_id_<=NSERVICES;node_id_++) {
        ServiceAddress service_address=node_id_;//SERVICE_ADDRESSES[node_id_-1];
        Service node_id = node_id_;//service_address;
        //       ServiceAddress service_address=node_id;
               if  (service_address != 0 and service_address != MAX_NSERVICES) {
#ifdef USE_TILERA
            	 tmc_alloc_t alloc = TMC_ALLOC_INIT;
            	 //tmc_alloc_set_home(&alloc, 46);
            	 //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_TASK);
            	 //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_HASH);
            	 //tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_NONE);
            	 //tmc_alloc_set_node_preferred(&alloc, 1);
            	 //void* raw_tile = tmc_alloc_map(&alloc, sizeof(*nodes[service_address]));
            	 void* raw_tile = tmc_alloc_map(&alloc, getpagesize());
		 printf ("WE ARE HERE, this is the sizeof raw_tile: %d\n", sizeof(raw_tile));
		 if (raw_tile == NULL)
            	   tmc_task_die("Failed to allocate memory for raw_tile.");
            	 nodes[node_id] = new(raw_tile) Tile(this,node_id,service_address);
#else
            	 nodes[node_id]=new Tile(this,node_id,service_address);
 		 //printf ("this it the sizeof nodes[%d]: %d\n", service_address, sizeof(nodes[service_address]));
           //      printf ("this it the sizeof *nodes[%d]: %d\n", service_address, sizeof(*nodes[service_address]));

#endif
   #ifdef VERBOSE
                       cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
   #endif // VERBOSE
                 }
           }
	};
#else // DISTR
    // --------------------------------------------------------------
    System(int node_id,int multi_ip_) : gw_address(node_id),multi_ip(multi_ip_),
    finished(false), io_mech(1)
    {
        nodes[0]=new Tile(this,node_id,multi_ip); //WV: we use the service_address as multi_ip! Dangerous!
    };
#endif // DISTR
*/
#if DISTR==0
    Service service_by_address(ServiceAddress address);
#endif // DISTR
 void run();

#if USE_THREADS==1
 void run_th();
#endif // USE_THREADS
#if DISTR==1
 void run_proc();
#endif // DISTR

}; // System
} // namespace SBA

#endif // SYSTEM_H_
