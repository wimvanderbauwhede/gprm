#include "MergeSort.h"
#define RESET_COLOR "\e[m"
#define MAKE_BLUE "\e[34m"

 int MergeSort::inst_no=0;
 int MergeSort::array(int size) {
	 double start = wsecond();
	 int* input_array = new int[size];
	 for(int i=0; i<size; i++) {
		 input_array[i]=i*((i%2)+2);
		 //in_array[i] = rand() % 100000000;
	 }
	int* scratch = new int[size];
	 double now= wsecond();
	 printf(MAKE_BLUE "End of Array:" RESET_COLOR "%f \n", now-start);
	 //cout << "in_array*: " << input_array << endl;
	GReg(1,Word(input_array));
	GReg(2,Word(scratch)); 
	custome_time=wsecond();
	 return 0;
 }

 int MergeSort::max(int x, int y)
 {
     if(x > y)
     {
         return x;
     }
     else
     {
         return y;
     }
 }

/* left is the index of the leftmost element of the subarray; right is one
  * past the index of the rightmost element */
int MergeSort::ser_ms(int* input, int left, int right, int* scratch)
{

  if(right == left + 1)
    {
        return 0;
    }
    else
    {
        int i = 0;
        int length = right - left;
        int midpoint_distance = length/2;
        int l = left, r = left + midpoint_distance;

          /* sort each subarray */
          ser_ms(input, left, left + midpoint_distance, scratch);
          ser_ms(input, left + midpoint_distance, right, scratch);

        /* merge the arrays together using scratch for temporary storage */
        for(i = 0; i < length; i++)
        {

            if(l < left + midpoint_distance &&
					 (r == right || max(*(input+l), *(input+r)) == *(input+l)))
            {
                *(scratch+i) = *(input+l);
           	 //cout << scratch->at(i) << endl;
                l++;
            }
            else
            {
           	 *(scratch+i) = *(input+r);
           	 //cout << scratch->at(i) << endl;
                r++;
            }
        }
        /* Copy the sorted subarray back to the input */
        memcpy ( input+left, scratch, (right-left)*sizeof(int));
    }
   return 0;
}

  //Elements are sorted in reverse order -- large to small */

 int* MergeSort::serial_ms(int index, int size)
 {
	 //printf("serial ms, ind: %d\n", index);
	 //cout << "in: " << (int*)GReg(1) <<endl;
	 //cout << "sc: " << (int*)GReg(2) <<endl;

	 int* input = ((int*)GReg(1)) + size*index;
	 int* scratch = ((int*)GReg(2)) + size*index;
	 //double mss = wsecond();
	ser_ms(input, 0, size, scratch);
	//double now = wsecond();
	//printf("End of MS:" RESET_COLOR "%f \n", now-mss);
	//for(int j=0; j<size; j++)
		//printf("input[%d]: %d\n",j,input[j]);
	return input;
 }

 int* MergeSort::merge_two(int index, int size, int* input1, int* input2) {
	printf("This merge_two is from intance: %d\n",local_inst_no); 
	//double mgg = wsecond();
	 //cout << "in1 " << input1 <<endl;
	 //cout << "in2 " << input2 <<endl;
	 //cout << "mg_sc: " << (int*)GReg(2) +1 <<endl;
	 int* scratch = ((int*)GReg(2)) + size*index;
	 //cout << "scratch" << scratch << endl;
	 int left = 0;
	 	int right = size/2;
	 	int i = 0;
	 	int midpoint_distance = size/2;
	 	 int l = left, r = left;
	 	 for(i = 0; i < size; i++) {
	 		 if(l < left + midpoint_distance && (r == right || max(*(input1+l), *(input2+r)) == *(input1+l)))
	 			{
	 			 *(scratch+i) = *(input1+l);
	 			 l++;
	 			}
	 		 else
	 			{
	 			 *(scratch+i) = *(input2+r);
	 			 r++;
	 			}
	 	 }
	 	 // You have to copy back to the main array (global array)
	 	 memcpy ( input1+left, scratch, size*sizeof(int));
	 	 /*for(int j=0; j<size; j++) {
	 	    printf("input1[%d]: %d\n",j,input1[j]);
	 	 }*/
	 	//double now = wsecond();
	 	//printf("End of MG:" RESET_COLOR "%f \n", now-mgg);
	 	 return input1;
 }

 int MergeSort::show (int size) {
	int* array1 = (int*)GReg(1);
	 for(int j = 0; j < size; j++)
		   printf("%d\n",array1[j]);
 }


