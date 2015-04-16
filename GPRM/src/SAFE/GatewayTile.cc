// Tile.rb
//
// :title: Gannet Service-based SoC project - SBA Tile class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: GatewayTile.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/GatewayTile.rb by ../../util/r2n.pl on Sun Apr 22 17:14:13 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******




#include "GatewayTile.h"
// The SBA GatewayTile is the interface between the Service Manager and the Network.
// It transfers data from the Network tx_fifo to a local rx_fifo
// and from the local tx_fifo to the Network rx_fifo


    //-- ----------------------------------------------------------------------------
    //
    // Main methods
    //

using namespace std;
using namespace SBA;

void GatewayTile::run() {
#ifdef VERBOSE
                		cout << "Gateway::run()\n";
#endif // VERBOSE

#ifdef VERBOSE
	cout << "Running GatewayTile "<<GatewayTile::service<<"\n";
#endif // VERBOSE
	    gateway.run();
        transceiver.run();

} //  of run()

#if USE_THREADS==1
    void *SBA::run_gwtile_loop(void* voidp) {
#ifdef USE_TILERA
    	// B ashkan
    	/*const size_t size = getpagesize();
    	tmc_alloc_t alloc = TMC_ALLOC_INIT;
    	//tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_HERE);
    	tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_HASH);
		//tmc_alloc_set_home(&alloc, TMC_ALLOC_HOME_NONE);
		//tmc_alloc_set_home(&alloc, 3);   					// cache on cpu 3
		//tmc_alloc_set_node_preferred(&alloc, 0);  			// memory from controller 0
		SBA::GatewayTile* gwtilep = (SBA::GatewayTile*)tmc_alloc_map(&alloc, size);
		gwtilep = (SBA::GatewayTile*)voidp;
		if (gwtilep == NULL)
		  tmc_task_die("Failed to allocate memory for gwtilep.");*/
    	SBA::GatewayTile* gwtilep = (SBA::GatewayTile*)voidp;
        int cpu = gwtilep->cpu;
        // Bind to one tile
        if (tmc_cpus_set_my_cpu(cpu) != 0)
          tmc_task_die("tmc_cpus_set_my_cpu() failed.");
        //uint64_t speed = tmc_perf_get_cpu_speed();
        //printf("Speed: %lld \n", speed);

#ifdef VERBOSE
        cout << "Gateway Thread set on the tile#: " << cpu << "\n";
#endif  // VERBOSE
        // E ashkan
#else
        SBA::GatewayTile* gwtilep = (SBA::GatewayTile*)voidp;
#endif

        if(!gwtilep->finished) { // no reason to loop. Transmit once. Wait for result.
            gwtilep->run();
            //printf ("Gateway Counter \n");
        }
#ifdef VERBOSE
        cout <<"Gateway Exit Checkpoint \n";
#endif  // VERBOSE
        pthread_exit((void *) 0);
    }
    void GatewayTile::run_th () {
#ifdef VERBOSE
    cout << "Starting GatewayTile " << service << "\n";
#endif // VERBOSE
        // B ashkan
#ifdef USE_TILERA
    	cpu_set_t desired_cpus;
    	if (tmc_cpus_get_my_affinity(&desired_cpus) != 0)
          tmc_task_die("tmc_cpus_get_my_affinity() failed.");
		// target is the second cpu
        cpu = tmc_cpus_find_nth_cpu(&desired_cpus, 0); // Gateway Tile#
#endif
        // E ashkan
	pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
        pthread_create(&th, &attr, SBA::run_gwtile_loop, (void*)this);
        //printf("GATEWAY --> Thread ID:   %ld  Created\n", th);
    }
#endif // USE_THREADS==1

