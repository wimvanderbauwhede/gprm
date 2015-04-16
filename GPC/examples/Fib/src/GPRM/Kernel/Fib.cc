#include "Fib.h"
#define RESET_COLOR "\e[m"
#define MAKE_BLUE "\e[34m"

timeval t;
long start;


void GPRM::Kernel::Fib::add(int *a, int b) {
    *a += b;
}


int GPRM::Kernel::Fib::fib(int n) {
	int x,y;
	 if (n<2) return n;
	 x = fib(n-1);
	 y = fib(n-2);
	 return x+y;
 }
 
 int GPRM::Kernel::Fib::start_timer() {
    gettimeofday(&t, NULL);
    start = (t.tv_sec * 1000) + (t.tv_usec / 1000);
 
 }

 int GPRM::Kernel::Fib::timer(int res)
 {
    gettimeofday(&t, NULL);
 	long end = (t.tv_sec * 1000) + (t.tv_usec / 1000);
 	printf("Timer: %d\n",end-start);
    printf("Result: %d\n", res);
 	return 0;
 }

