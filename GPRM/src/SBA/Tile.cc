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

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/Tile.rb by ../../util/r2n.pl on Sun Apr 22 17:14:10 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******




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
/*
	def run(sba_system)
      if @transceiver.rx_fifo.has_packets()
        @status=true
        while (@status==true)
            @service_manager.run()
    		if (@service_manager.core_status==CS_busy)
        		@service_core.run()
    		end
            @transceiver.run()
            @status = (true and (@service_manager.status or (@transceiver.tx_fifo.length>0) or (@transceiver.rx_fifo.length>0)))
        end
      end
    end
*/


/* Ashkan Changed it
void Tile::run() {
        //System& sba_system=*((System*)sba_system_ptr);
    if (transceiver.rx_fifo.has_packets()) { // WV15122010: was .length()>0
        status=true;
//        cout << "Tile::run() for " << address << endl;
//        cout << ppPacket(transceiver.rx_fifo.front()) << endl;
        while (status==true) {
            service_manager.run();
            if (service_manager.core_status==CS_busy) {
                service_core.run();
            }
    	    transceiver.run();
            status= true && (service_manager.status || (transceiver.tx_fifo.length()>0) || (transceiver.rx_fifo.length()>0));
        }
    }
} //  of run()
*/

void Tile::run() {
#if USE_THREADS==1
	transceiver.rx_fifo.wait_for_packets(address);
#endif
#ifdef VERBOSE
	cout << "Tile::run() start for tile "<< address <<","<<service<<"\n";
#endif // VERBOSE
	//printf("Ashkan:Tile::run() start- Address: %d, Service: %d\n",address,service);

label:  status = true;
	while(status==true) {
	//while(1) { // Ashkan: I want to steal. Change it to busy waiting for now?! Maybe, 1-more-chance strategy is good enough!
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
#ifdef STEAL
		service_manager.run(1); //bool steal: it means steal only if your work is finished, one chance before sleeping on the Transceiver's Fifo
		//printf("Steal from Tile\n");		
		if (service_manager.core_status==CS_busy)
			service_core.run();
		transceiver.run();
		if(service_manager.status || (transceiver.tx_fifo.length()>0) || transceiver.rx_fifo.has_packets()) {goto label;}
#endif


#ifdef VERBOSE
	cout << "Tile::run() done\n";
#endif // VERBOSE
	//printf("Ashkan:Tile::run() DONE- Service: %d \n",service);
}

#if USE_THREADS==1
    void *SBA::run_tile_loop(void* voidp) {
    	SBA::Tile* tilep = (SBA::Tile*)voidp;
        while (1) {
        	tilep->run();
        }
        pthread_exit((void *) 0);
    }

    void Tile::run_th () {
#ifdef VERBOSE
    cout << "** Starting Tile " << service << " **\n";
#endif // VERBOSE
#ifdef THREAD_PINNING_OLD
		thread_mapping(address);
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_tile_loop, (void*)this);
#else
#ifndef DARWIN
        // Gordon's thread pinning code
        cpu_set_t cpuset;
        CPU_ZERO(&cpuset);
        CPU_SET(service - 1, &cpuset);
#endif
        pthread_attr_init(&attr);
#ifndef DARWIN
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuset);
#endif
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_tile_loop, (void*)this);
#endif
        //cout << "Thread ID: " << pthread_self() << endl;
        //printf("Thread ID:   %ld  Created on the Tile Adress: %d Service_Address: %d\n", tid, address, service);
        //thread_map[tid] = address;
    }
#endif // USE_THREADS==1
#if DISTR==1
    void SBA::run_proc()
#ifdef VERBOSE
        cout << "Starting Tile "<<service<<"\n";
#endif // VERBOSE
            while (true) {
                run();
            }
    }
#endif // DISTR

