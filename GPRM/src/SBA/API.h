#ifndef API_H_
#define API_H_
/*
 * (c) 2014 Ashkan Tousimojarad <a.tousimojarad.1@research.gla.ac.uk>
 * The implementations of templates should be here, otherwise, linking error!
 */

//#include <omp.h>

#include "Base/CoreServices.h"
#include "System.h"
using namespace SBA;
namespace GPRM {
//namespace API {
		Word GReg(int a);
		void GReg(int a, Word b); //Reg(a)=b;
		Word get_shared(int);
		void put_shared(int,Word);
		void* get_arg(int);
		void put_arg(int,void*);

		/************/
		// Parallel Loops
		/************/
		/*template <typename Tclass>
		struct TFuncPointer
		{
			typedef int (Tclass::*Function)(int,int,int,float**);
		};*/

		/*
		 * No Parameter
		 */

		template <typename Tclass>
		static int GPar_cont_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int))
		{  //TODO: We have assumed that NTH is always less than the siz
			int turn=0;
			int chunk=(size-start)/PD;
			int counter=0;
			for(int i=start; i<size;) {
				if(turn%PD == ind) {
					for(counter=0;counter<chunk;counter++) {
						(T->*work_function)(counter+i,start,size);
					}
					i= PD*chunk+ind;
					if(i < size) {
						(T->*work_function)(i,start,size);
						return 0;
					}
				}
				else {
					turn++;
					i=i+chunk;
				}
			}
		 return 0;
		}

		/*
                 * 1 Parameter
                 */
                template<typename Tclass, typename Param1>
                static int GPar_cont_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,Param1), Param1 p1)
                {
                        int turn=0;
                        int chunk=(size-start)/PD;
                        int counter=0;
                        for(int i=start; i<size;) {
                                if(turn%PD == ind) {
                                        for(counter=0;counter<chunk;counter++) {
                                                (T->*work_function)(counter+i,start,size,p1);
                                        }
                                        i= PD*chunk+ind;
                                        if(i < size) {
                                                (T->*work_function)(i,start,size,p1);
                                                return 0;
                                        }
                                }
                                else {
                                        turn++;
                                        i=i+chunk;
                                }
                        }
                 return 0;
                }
		
		/*
                 * 2 Parameters
                 */
                template<typename Tclass, typename Param1, typename Param2>
                static int GPar_cont_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,Param1,Param2), Param1 p1, Param2 p2)
                {
                        
			int turn=0;
                        int chunk=(size-start)/PD;
                        int counter=0;
                        for(int i=start; i<size;) {
                                if(turn%PD == ind) {
                                        for(counter=0;counter<chunk;counter++) {
                                                (T->*work_function)(counter+i,start,size,p1,p2);
                                        }
                                        i= PD*chunk+ind;
                                        if(i < size) {
                                                (T->*work_function)(i,start,size,p1,p2);
                                                return 0;
                                        }
                                }
                                else {
                                        turn++;
                                        i=i+chunk;
                                }
                        }
                 return 0;
                }


//******************//
// GPAR_for
		template<typename Tclass>
		static int GPar_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int))
		{
			int turn=0;
				 for(int i=start; i<size;) {
					if(turn % PD == ind) {
						(T->*work_function)(i,start,size);
						i=i+PD;
					}
					else {
						i++;
						turn++;
					}
				}
			return 0;
		}

		/*
		 * 1 Parameter
		 */
		template<typename Tclass, typename Param1>
		static int GPar_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,Param1), Param1 p1)
		{
			int turn=0;
				 for(int i=start; i<size;) {
					if(turn % PD == ind) {
						(T->*work_function)(i,start,size,p1);
						i=i+PD;
					}
					else {
						i++;
						turn++;
					}
				}
			return 0;
		}

		/*
		 * 2 Parameters
		 */
		template<typename Tclass, typename Param1, typename Param2>
		static int GPar_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,Param1,Param2), Param1 p1, Param2 p2)
		{
			int turn=0;
				 for(int i=start; i<size;) {
					if(turn % PD == ind) {
						(T->*work_function)(i,start,size,p1,p2);
						i=i+PD;
					}
					else {
						i++;
						turn++;
					}
				}
			return 0;
		}

		/*
		 * 3 Parameters
		 */
		template<typename Tclass, typename Param1, typename Param2, typename Param3>
		static int GPar_for(int start,int size,int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,Param1,Param2,Param3), Param1 p1, Param2 p2, Param3 p3)
		{
			int turn=0;
				 for(int i=start; i<size;) {
					if(turn % PD == ind) {
						(T->*work_function)(i,start,size,p1,p2,p3);
						i=i+PD;
					}
					else {
						i++;
						turn++;
					}
				}
			return 0;
		}


		/*template <class Function, typename Param1, typename Param2>
		int GPar_for(int start,int size,int ind, int PD, Function work_function, Param1 p1, Param2 p2)
		{
			int turn=0;
				 for(int i=start; i<size;) {
					if(turn % PD == ind) {
						work_function(i,start,size,p1,p2);
						i=i+PD;
					}
					else {
						i++;
						turn++;
					}
				}
			return 0;
		}*/

		/************/
		// Parallel Nested Loops
		/************/

		/*
		 * No Parameter
		 */

		template <typename Tclass>
		static int GPar_nested_for(int start1, int size1, int start2, int size2, int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,int,int,int))
		{
			//omp_set_num_threads(4);
			int turn=0;
			//#pragma omp parallel for schedule (static,1) firstprivate(turn,arg1)
			for(int i=start1; i<size1; i++) {
				for(int j=start2; j<size2;) {
					if((turn>=0) && (turn % PD == ind)) { //Is this a task of this thread?
						(T->*work_function)(i,j,start1,size1,start2,size2);
						j=j+PD;
						if(j>=size2)
							turn = size2-j+ind; // e.x. with 16 threads and size=30. 0...16...32 -> turn becomes -2 which means when i becomes i+1, j=2 is the element that we have to compute
						}
					else {
						j++;
						turn++;
					}
				}
			}
			return 0;
		}

		/*
		 * 1 Parameter
		 */
		template <typename Tclass ,typename Param1>
		static int GPar_nested_for(int start1, int size1, int start2, int size2, int ind, int PD, Tclass* T, int (Tclass::*work_function)(int,int,int,int,int,int,Param1), Param1 p1)
		{
			//omp_set_num_threads(4);
			int turn=0;
			//#pragma omp parallel for schedule (static,1) firstprivate(turn,arg1)
			for(int i=start1; i<size1; i++) {
				for(int j=start2; j<size2;) {
					if((turn>=0) && (turn % PD == ind)) { //Is this a task of this thread?
						(T->*work_function)(i,j,start1,size1,start2,size2,p1);
						j=j+PD;
						if(j>=size2)
							turn = size2-j+ind; // e.x. with 16 threads and size=30. 0...16...32 -> turn becomes -2 which means when i becomes i+1, j=2 is the element that we have to compute
						}
					else {
						j++;
						turn++;
					}
				}
			}
			return 0;
		}

		/*template <class FunctionNested, typename Arg1, typename Arg2>
		int GPar_nested_for(int start1, int size1, int start2, int size2, int ind, int PD, FunctionNested work_function, Arg1 arg1, Arg2 arg2)
		{
			//omp_set_num_threads(4);
			int turn=0;
			//#pragma omp parallel for schedule (static,1) firstprivate(turn,arg1,arg2)
			for(int i=start1; i<size1; i++) {
				for(int j=start2; j<size2;) {
					if((turn>=0) && (turn % PD == ind)) { //Is this a task of this thread?
						work_function(i,j,start1,size1,start2,size2,arg1,arg2);
						j=j+PD;
						if(j>=size2)
							turn = size2-j+ind; // e.x. with 16 threads and size=30. 0...16...32 -> turn becomes -2 which means when i becomes i+1, j=2 is the element that we have to compute
						}
					else {
						j++;
						turn++;
					}
				}
			}
			return 0;
		}*/
	//APIC(){printf("API Constructor: %d\n",service);};
	//};

}// end of API namespace

#endif // API_H_
