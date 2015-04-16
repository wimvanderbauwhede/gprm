#ifndef SCHEDULE_H
#define SCHEDULE_H

#include <stdio.h>
//#define LAUNCHER
//#define MULTIPROGRAMMING 
//#define SHARING
//#define STEAL

 int thread_mapping(int);	// 1-1 Mapping between threads and cores (NUM_CPU)
#ifdef MULTIPROGRAMMING    
 int map_shared(void); 		// Maps the shared Chip_Info structure for the current GPRM process
 void get_sem(void); 		// Gets the my_sem lock (binary semaphore)
 void rel_sem(void); 		// Releases the my_sem lock (binary semaphore)
 int unmap_shared(void);	// Unmaps the shared Chip_Info structure for the current GPRM process
 int find_target(int,int); 		// Finds the target core!
 void pin_target(int);		// Increases the number of pinned services to this target
 void release_target(int);	// Decreases the number of pinned services to this target
 int Scheck(int);
 int XLL(void);				// Uses the XLL approach to find the targe
#endif

#endif
