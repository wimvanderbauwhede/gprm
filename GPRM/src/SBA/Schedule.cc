#ifdef LAUNCHER
#include "../../../launcher.h" //multiple definition of `chip_array' if places in Schedule.h
#endif
#include "Schedule.h"
#ifndef DARWIN
#define _GNU_SOURCE
#include <sched.h> // thread mapping
#endif
#define RESET_COLOR "\e[m"
#define MAKE_PURPLE "\e[35m"
#define MAKE_GREEN "\e[32m"
#define MAKE_RED "\e[31m"

int appid = 0;

 int thread_mapping(int address) {
#ifndef DARWIN
	cpu_set_t  mask;
	CPU_ZERO(&mask);
#endif
	int target_core; //printf("NUM_CPU: %d\n",NUM_CPU);
	
	if(address==0) target_core=239;//address;
	else if(address==1) target_core=0;	

	//4 per phyiscal core Distribution
	//else target_core=(((address-1)%240)); //2->1 241->240(0)(last_one)

	//2 per physical core Distribution
	else if (address<122) target_core=((address-2)*2+1)%240;// for the SULUG with 4 cores in total!
        else target_core=((address-2)*2+2)%240;
	//end of 2 per core

//	if((address%240)<120) target_core=((address*2)%NUM_CPU);// for the SULUG with 4 cores in total!
//	else target_core=((address*2+1)%NUM_CPU);
	
/*	
	else if (address<62) target_core=((address-2)*4+1)%240;// for the SULUG with 4 cores in total!
        else if(address<122) target_core=((address-2)*4+2)%240;
        else if(address<182) target_core=((address-2)*4+3)%240;
        else target_core=((address-2)*4+4)%240;
*/
//	printf("address %d, target_core: %d\n",address, target_core);


//TODO:
#ifdef MULTIPROGRAMMING
#ifdef SHARING  
        target_core = (target_core+((appid -1)*4))%240;
#endif
#endif
	//target_core=(address<=120?address*2:address*2+1)%NUM_CPU;
	//printf("add: %d\n",address);
	//target_core=address%NUM_CPU;
#ifndef DARWIN
	CPU_SET(target_core, &mask);
	if (sched_setaffinity(0, sizeof(mask), &mask) !=0)
		perror("sched_setaffinity");
#endif
	return target_core;
 }

#ifdef MULTIPROGRAMMING   
 int map_shared() {
	fd = open("./jim.mymemory", O_RDWR| O_CREAT, S_IRUSR| S_IWUSR );
	if (fd == 0) {
		int myerr = errno;
		printf("ERROR: open failed (errno %d %s)\n", myerr, strerror(myerr));
		return EXIT_FAILURE;
	}
	if (lseek(fd, length - 1, SEEK_SET) == -1) {
		int myerr = errno;
		printf("ERROR: lseek failed (errno %d %s)\n", myerr, strerror(myerr));
		return EXIT_FAILURE;
	}
	chip_array = (struct Chip_Info*)mmap(NULL, length, prot, flags, fd, offset);
	if (chip_array == 0) {
		int myerr = errno;
		printf("ERROR (child): mmap failed (errno %d %s)\n", myerr, strerror(myerr));
	}
	best_target_p = (int*)(chip_array + NUM_CPU*sizeof(struct Chip_Info*));
	my_sem = (sem_t*)(chip_array + 3*sizeof(struct Chip_Info*)); //Currently, the only lock is the chip_array[3].sem
#ifdef SHARING
	get_sem();
	*best_target_p = *best_target_p + 1;
	printf("best: %d\n",*best_target_p);
	appid = *best_target_p;
	rel_sem();
#endif
	return 0;
 }

 int unmap_shared() {
 	if (munmap(chip_array, length) == -1) {
 	   int myerr = errno;
 	   printf("ERROR (child): munmap failed (errno %d %s)\n", myerr, strerror(myerr));
 	}
 	if (close(fd) == -1) {
 		int myerr = errno;
 		printf("ERROR: close failed (errno %d %s)\n", myerr, strerror(myerr));
 	}
 	return 0;
  }

 void get_sem() {
	sem_wait(my_sem);
	//printf("GPRM has the lock\n");
 }

 void rel_sem() {
	 sem_post(my_sem);
	 //printf("GPRM released the lock\n");
 }

 int find_target(int previous_target, int parentOrnot) {
	 get_sem();
	 chip_array[1].pinned=1000000; // TODO: HACK
	 *best_target_p = previous_target%NUM_CPU;
	 chip_array[*best_target_p].pinned -= parentOrnot; //Give the priority of the *best_target_p core to this subtask
	 //if(parentOrnot==1)
		// printf("Parent priority: chip_array[%d].pinned (FOUND): %d\n",*best_target_p,chip_array[*best_target_p].pinned);
	 //printf("chip_array[%d] (PREVIOUS, but not pinned): %d\n",previous_target%NUM_CPU,chip_array[previous_target%NUM_CPU].pinned);
	 for(int i=0; i< NUM_CPU; i++) {
		 if(chip_array[i].pinned < chip_array[*best_target_p].pinned) //TODO: ARE YOU SURE <= is not better??
			 *best_target_p = i;
	 }
	 /*
	  * +2 is key here. To make use of all idle cores instead of those who r just released, but located at the top of the chip
	  */
	 chip_array[*best_target_p].pinned +=2;
	 //printf("chip_array[%d].pinned+2 (FOUND): %d\n",*best_target_p,chip_array[*best_target_p].pinned);
	 int final_target = *best_target_p + NUM_CPU; // We can't send 0 (Gateway)!
	 printf(MAKE_GREEN "find_target(%d): %d"  RESET_COLOR "\n",previous_target,final_target%NUM_CPU);
	 rel_sem();
	 return (final_target); // to get rid of the address 0. This number will be transformed to a correct location, based on the modulo formula in the Transceiver
  }

 void pin_target(int target) {
	 get_sem();
	 /*
	  * +2 is key here. To make use of all idle cores instead of those who r just released, but located at the top of the chip
	  */
	 chip_array[target%NUM_CPU].pinned +=2; // The place where the thread of this target is mapped to
	 //printf("chip_array[%d].pinned+2: %d\n",target%NUM_CPU,chip_array[target%NUM_CPU].pinned);
	 //printf(MAKE_GREEN "pin_target(%d): "  RESET_COLOR "\n",target%NUM_CPU);
	 rel_sem();
	 return;
 }

 void release_target(int target) {
	 get_sem();
	 chip_array[target%NUM_CPU].pinned--; // The place where the thread of this target is mapped to
	 //printf("chip_array[%d].pinned--: %d\n",target%NUM_CPU,chip_array[target%NUM_CPU].pinned);
	 if(chip_array[target%NUM_CPU].pinned<0)
		 printf(MAKE_RED "Less than 0" RESET_COLOR "\n");
	 //printf(MAKE_PURPLE "release_target(%d): "  RESET_COLOR "\n",target%NUM_CPU);
	 rel_sem();
	 return;
 }

int Scheck(int target){

	char tmp1[10]; char usr1[NUM_CPU][6]; char no1[100];
	FILE *p;
	if((p=fopen("/proc/stat","r"))==NULL){
         printf("\nUnable t open file status");
         exit(1);
	}
//	fscanf(p,"%[^ ] %[^ ] %[^\n]%*c",tmp1,usr1[0],no1); // the first line which is the total load
        //for(int i=0; i<NUM_CPU; i++){
	  // fscanf(p,"%[^ ] %[^ ] %[^\n]%*c",tmp1,usr1[i],no1);
	   //if(i==target) {
	//	printf("%d ", atoi(usr1[i])+10);
	//	break;
	  // }
//	}
             //printf("%d ", atoi(usr1[i])+10);
         if (fclose(p) != 0) printf("ERROR!!/n");
	return 0;
}
 
int XLL() {
	 get_sem();
	 //chip_array[3].temp += 17;
	 //printf("chip_array[3].temp, GPRM: %d\n", chip_array[3].temp);
	 *best_target_p = ((*best_target_p)+1)%NUM_CPU;
	 char tmp1[10]; char usr1[NUM_CPU][6]; char no1[100];
	 FILE *p;
	 if((p=fopen("/proc/stat","r"))==NULL){
		 printf("\nUnable t open file status");
		 exit(1);
	 }
	 fscanf(p,"%[^ ] %[^ ] %[^\n]%*c",tmp1,usr1[0],no1); // the first line which is the total load
	 int i=0;
	 for(i=0; i<NUM_CPU; i++) {
		 fscanf(p,"%[^ ] %[^ ] %[^\n]%*c",tmp1,usr1[i],no1);
	     //printf("%d ", atoi(usr1[i])+10);
	 }
	 if (fclose(p) != 0) printf("ERROR!!/n");
	 // set the latest change (load_current-load_last). set the load. bubble the chip_array
	 for(i=0; i<NUM_CPU; i++) {
		 chip_array[i].change = atoi(usr1[i]) - chip_array[i].load + chip_array[i].pinned;
		 chip_array[i].load = atoi(usr1[i])+10;//get rid of load 0;
	 }
	 for(i=0; i<NUM_CPU; i++) {
		 if((chip_array[i].change) < (chip_array[*best_target_p].change)) *best_target_p = i;
		 //if((chip_array[i].load) < (chip_array[best_target].load)) best_target = i;
	 }
	 //printf("best_target, actual pinning: %d\n", *best_target_p);

	 chip_array[*best_target_p].pinned++;// to increase the change of the next visit
	 rel_sem();
	 return (*best_target_p);
 }
#endif

