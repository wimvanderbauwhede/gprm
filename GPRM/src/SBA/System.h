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
#include "Tile.h"
#include "GatewayTile.h"
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
#ifdef KERNEL_HAS_STATE
	std::unordered_map<unsigned int,std::pair<unsigned int,void*> > kernels;
#endif
	void* result;
	uint io_mech;
	uint multi_ip;
	GatewayTile     gw_instance ;
    std::unordered_map<Service,Tile*> nodes;
	// --------------------------------------------------------------
	System(std::string td_, int nth_) : gw_address(0), nservicenodes(nth_), reuse(false),
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
#ifdef VERBOSE
    	cout << "Creating node " << node_id_ << " of "<<NSERVICES <<"\n";
#endif
        ServiceAddress service_address=node_id_;
        Service node_id = node_id_;
        if  (service_address != 0) {
          // This check is not needed if we create contiguous instances
          // but if we use SERVICE_ADDRESSES it would be required
              nodes[node_id]=new Tile(this,node_id,service_address);
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE
        }
    }

	};
    Service service_by_address(ServiceAddress address);
 void run();


}; // System
} // namespace SBA

#endif // SYSTEM_H_
