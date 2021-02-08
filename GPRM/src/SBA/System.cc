//
// Gannet Service-based SoC project - SBA System (toplevel) class
//
// (c) 2004-2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#include "System.h"

using namespace std;
using namespace SBA;

const System* System::APIsys; //for API use

    Service System::service_by_address(ServiceAddress address) { //H
        Service node_id=address;
        return node_id;
    }


 void System::run()  {
        gw_instance.run();
        if (gw_instance.finished==false){
            for (uint i=1;i<=NSERVICES;i++) {
                Tile& service_node=*nodes[i];
                if (service_node.status or service_node.transceiver.rx_fifo.status()){
                    service_node.run();
                }
            }
        } else {
            finished=1;
        }
    } // of run()




