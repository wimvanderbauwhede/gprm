//
// Gannet Service-based SoC project - SBA System (toplevel) class
//
// (c) 2004-2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#include "System.h"

using namespace std;
using namespace SBA;

#if DISTR==0
    Service System::service_by_address(ServiceAddress address) { //H
        Service node_id=address;
        return node_id;
    }

#endif // DISTR

 void System::run()  {
#if DISTR==0
        gw_instance.run();
        if ((gw_instance.finished==false) ){
            for (uint i=1;i<=NSERVICES;i++) {
                 Tile& service_node=*nodes[i];
            //for (map<ServiceAddress,Tile*>::iterator iter_=nodes.begin();iter_!=nodes.end();iter_++) {
              //  Tile& service_node= *(iter_->second);
                //if (true and (service_node.status or service_node.transceiver.rx_fifo.status())){
                if (service_node.status or service_node.transceiver->rx_fifo.status()){
                    service_node.run();
                }
#ifdef VERBOSE
//                		cout << "Tile::run() "<< i <<" from "<<NSERVICES<<" done\n";
#endif // VERBOSE

            }
        } else {
            finished=1;
        }
#endif // DISTR
    } // of run()

#if USE_THREADS==1
 void System::run_th()  {
        gw_instance.run_th();

#ifdef VERBOSE
        cout << "Running Tiles with Threads\n";
#endif // VERBOSE
            //for (map<ServiceAddress,Tile*>::iterator iter_=nodes.begin();iter_!=nodes.end();iter_++) {
              //  Tile& service_node= *(iter_->second);
            for (uint i=1;i<=NSERVICES;i++) {
                 Tile& service_node=*nodes[i];
                    service_node.run_th();
            }
#ifdef VERBOSE
        cout << "\n";
#endif // VERBOSE
         pthread_join(gw_instance.th,&gw_instance.tstatus);
    } // of run_th()
#endif // USE_THREADS

#if DISTR==1
 void System::run_proc()  {
         Tile& service_node=*nodes[0];
        service_node.run_proc();
    }
#endif // DISTR


