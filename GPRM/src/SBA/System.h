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

	const static System* APIsys; //Ashkan: To be used by APIs
	ServiceAddress gw_address;
	ServiceMap servicenodes;
	uint nservicenodes;
	bool reuse;
	uint finished;
	string task_data;
	string task_description;
//	TaskDescList     task_descriptions;
	Bytecode     bytecode;
	std::vector<void*> args;
	std::vector<Word> regs;
	void* result;
	uint io_mech;
	uint multi_ip;
#if DISTR==0
	GatewayTile     gw_instance ;
#endif // DISTR
    std::unordered_map<Service,Tile*> nodes;
//    std::vector<Tile*>* nodes_ptr;
	// --------------------------------------------------------------
#ifdef USE_TILERA
	System(std::string td_, int nth_) : gw_address(0), nservicenodes(nth_), reuse(false),
	finished(false), task_description(td_), result(NULL) //TODO: Ashkan: not sure if NULL is a good replacement!
	,gw_instance(this,0,0,td_)
#else
	System(std::string td_, int nth_) : gw_address(0), nservicenodes(nth_), reuse(false),
	finished(false), task_description(td_), result(nullptr)
	,gw_instance(this,0,0,td_)
#endif
    {
		// allocate some space for args and regs
		for (int regno = 0; regno< MAX_REGISTERFILE_SZ; regno++) {
			regs.push_back((Word)0);
		}
//		nodes_ptr = new std::vector<Tile*> ;
//		std::vector<Tile*>& nodes = *nodes_ptr;
		//Services
		nservicenodes=NSERVICES;
//		nodes.push_back(nullptr); // idx==0
    for (Service node_id_=1;node_id_<=NSERVICES;node_id_++) {
#ifdef VERBOSE
    	cout << "Creating node " << node_id_ << " of "<<NSERVICES <<"\n";
#endif
        ServiceAddress service_address=node_id_;//SERVICE_ADDRESSES[node_id_-1];
        Service node_id = node_id_;//service_address;
        if  (service_address != 0) {
          // This check is not needed if we create contiguous instances
          // but if we use SERVICE_ADDRESSES it would be required
//          if (nodes.count(node_id)==0) {
//        	  cout << "\tNew node "<<node_id_<< "\n";
              nodes[node_id]=new Tile(this,node_id,service_address);
//              nodes.push_back( new Tile(this,node_id,service_address) );
//              cout << "\tNew node size "<<nodes.count(node_id_)<< "\n";
//          } else {
//        	  cout << "\tNode "<<node_id_<< " already existed\n";
//          }
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE
        }
    }

	};
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
