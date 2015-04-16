using namespace std;
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <iostream>
#include <string.h>
#include <Timings.h>

#include <API.h>
using namespace API;

class MergeSort {
	public:
	static int inst_no;
	int local_inst_no;	
		int array(int);
		int max(int,int);
		int ser_ms(int*, int, int, int*);
		int* serial_ms(int, int);
		int* merge_two(int, int, int*, int*);
		int show (int);
	MergeSort() {
	 inst_no++;
	 local_inst_no = inst_no;
	}
};

