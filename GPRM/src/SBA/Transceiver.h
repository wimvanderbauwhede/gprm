
#ifndef TRANSCEIVER_H_
#define TRANSCEIVER_H_

// Transceiver.rb
//
// :title: Gannet Service-based SoC project - Transmitter/Receiver class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//
////==============================================================================
////
//// Gannet Service-based SoC project - Tansmitter/Receiver class
////
////==============================================================================
//

#include "Base/Tile.h"
#include "Base/System.h"
#include "Types.h"
#include "Packet.h"

using namespace std;

namespace SBA {
class Transceiver {
	public:

    	Base::System* sba_system_ptr;
      Base::Tile* sba_tile_ptr;
	Service service;
	ServiceAddress address ;
	RX_Packet_Fifo rx_fifo;
	TX_Packet_Fifo tx_fifo ;
#if DISTR==1
    UDPSocket rx_socket, tx_socket;
#endif

    Transceiver(Base::System* sba_s_,Base::Tile* sba_t_,Service& s_, ServiceAddress addr_) :
    sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_),
    service(s_), address(addr_)
    {
    #if DISTR==1
        rx_socket.bind(INADDR_ANY, GWPORT + service);
    #endif
    };

 void run();

#if DISTR==1
 void receive_packets();
#endif // DISTR

#if USE_THREADS==1
 void receive_packets();
#endif // USE_THREADS

 void transmit_packets();
 void multicast();

}; // Transceiver
} // namespace SBA

#endif // TRANSCEIVER_H_
