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

#include "System.h"
#include "Tile.h"
// The SBA Tile is the interface between the Service Manager and the Network.
// It transfers data from the Network tx_fifo to a local rx_fifo
// and from the local tx_fifo to the Network rx_fifo

#include "Schedule.h"
    //-- ----------------------------------------------------------------------------
    //
    // Main methods
    //

using namespace std;
using namespace SBA;


void Tile::run() {
	// transceiver.rx_fifo.wait_for_packets(address);
#ifdef VERBOSE
	cout << "Tile::run() start for tile "<< address <<","<<service<<"\n";
#endif // VERBOSE

    status = true;
	while(status==true) {
		 service_manager.run(0);
		 if (service_manager.core_status==CS_busy) {
#ifdef VERBOSE
	cout << "Back to Tile to run service_core.run() \n";
#endif // VERBOSE
		   service_core.run();
		 }
		 transceiver.run();
    	 status= service_manager.status || (transceiver.tx_fifo.length()>0) || transceiver.rx_fifo.has_packets();
	}// while

#ifdef VERBOSE
	cout << "Tile::run() done\n";
#endif // VERBOSE
}



