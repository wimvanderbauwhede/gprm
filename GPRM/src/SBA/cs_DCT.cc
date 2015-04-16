

#include "cs_DCT.h"

#include "System.h"
#include "Tile.h"
#include "ServiceCore.h"

using namespace std;
using namespace SBA;
/*
Word_List SCLib::cs_DCT(Base::ServiceCore* parent_ptr,MemAddresses& addresses)  {
	// Set up context
	ServiceCore* servicecore_ptr=(ServiceCore*)parent_ptr;
	ServiceCore& parent=*servicecore_ptr;
	System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
	System& sba_system=*sba_system_ptr;
	Tile& sba_tile=*(sba_system.instances[servicecore_ptr->address]);

	Word_List result_list;
	MemAddress address=addresses[0];
	Word nruns=sba_tile.data_storage.mget(address)[1];
	Word res_symbol=sba_tile.data_storage.mget(address)[0];

	SCLib::calc_dct(nruns);

	result_list.push_back(res_symbol);
	result_list.push_back(nruns);
	return result_list;
}
*/

void *SCLib::run_dct(void* voidp) {
    SBA::ServiceCore* servicecore_ptr = (SBA::ServiceCore*)voidp;
    //while (1) {
    int nruns=servicecore_ptr->state_register[1];
#ifdef VERBOSE
    std::cout <<"DCT: NRUNS: "<<nruns<<"\n";
#endif
     calc_dct(nruns);
    //}
#ifdef VERBOSE
     std::cout <<"DCT: DONE\n";
#endif
    servicecore_ptr->state_register[0]=2;
#ifdef THREADED_CORE    
    pthread_exit((void *) 0);
#endif    
}


void SCLib::calc_dct(int nruns) {

	/* example 8x8 block from Wallace's overview paper */
	float data[8][8] = {{139,144,149,153,155,155,155,155},
				 {144,151,153,156,159,156,156,156},
				 {150,155,160,163,158,156,156,156},
				 {159,161,162,160,160,159,159,159},
				 {159,160,161,162,162,155,155,155},
				 {161,161,161,161,160,157,157,157},
				 {162,162,161,163,162,157,157,157},
				 {162,162,161,161,163,158,158,158}};

	float workspace[8][8];

  int i, j;
  int runs;

  for (runs=0; runs<nruns; runs++) {
    /* level shift */
    for (i=0; i<8; i++) {
      for (j=0; j<8; j++) {
	workspace[i][j] = data[i][j] - 128.0;
      }
    }
    SCLib::jpeg_fdct_float((float *)workspace);
  }

}

/* following from jpeg-6b IJG distribution */

/*
 * jfdctflt.c
 *
 * Copyright (C) 1994-1996, Thomas G. Lane.
 * This file is part of the Independent JPEG Group's software.
 * For conditions of distribution and use, see the accompanying README file.
 *
 * This file contains a floating-point implementation of the
 * forward DCT (Discrete Cosine Transform).
 *
 * This implementation should be more accurate than either of the integer
 * DCT implementations.  However, it may not give the same results on all
 * machines because of differences in roundoff behavior.  Speed will depend
 * on the hardware's floating point capacity.
 *
 * A 2-D DCT can be done by 1-D DCT on each row followed by 1-D DCT
 * on each column.  Direct algorithms are also available, but they are
 * much more complex and seem not to be any faster when reduced to code.
 *
 * This implementation is based on Arai, Agui, and Nakajima's algorithm for
 * scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
 * Japanese, but the algorithm is described in the Pennebaker & Mitchell
 * JPEG textbook (see REFERENCES section in file README).  The following code
 * is based directly on figure 4-8 in P&M.
 * While an 8-point DCT cannot be done in less than 11 multiplies, it is
 * possible to arrange the computation so that many of the multiplies are
 * simple scalings of the final outputs.  These multiplies can then be
 * folded into the multiplications or divisions by the JPEG quantization
 * table entries.  The AA&N method leaves only 5 multiplies and 29 adds
 * to be done in the DCT itself.
 * The primary disadvantage of this method is that with a fixed-point
 * implementation, accuracy is lost due to imprecise representation of the
 * scaled quantization values.  However, that problem does not arise if
 * we use floating point arithmetic.
 */

/*
 * Perform the forward DCT on one block of samples.
 */

void SCLib::jpeg_fdct_float (float * data) {
  float tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
  float tmpe10, tmpe11, tmpe12, tmpe13;
  float tmpo10, tmpo11, tmpo12;
  float z1, z2, z3, z4, z5, z11, z13;
  float *dataptr;
  int ctr;
  /* Pass 1: process rows. */

  dataptr = data;
  for (ctr = DCTSIZE-1; ctr >= 0; ctr--) {
    tmp0 = dataptr[0] + dataptr[7];
    tmp7 = dataptr[0] - dataptr[7];
    tmp1 = dataptr[1] + dataptr[6];
    tmp6 = dataptr[1] - dataptr[6];
    tmp2 = dataptr[2] + dataptr[5];
    tmp5 = dataptr[2] - dataptr[5];
    tmp3 = dataptr[3] + dataptr[4];
    tmp4 = dataptr[3] - dataptr[4];

//      printf("tmp0: %f,tmp1: %f,tmp2: %f,tmp3: %f\n",tmp0,tmp1,tmp2,tmp3);
    /* Even part */

    tmpe10 = tmp0 + tmp3;	/* phase 2 */
    tmpe13 = tmp0 - tmp3;
    tmpe11 = tmp1 + tmp2;
    tmpe12 = tmp1 - tmp2;
//    printf("tmpe10: %f,tmpe11: %f,tmpe12: %f,tmpe13: %f\n",tmpe10,tmpe11,tmpe12,tmpe13);



    /* Odd part */

    tmpo10 = tmp4 + tmp5;	/* phase 2 */
    tmpo11 = tmp5 + tmp6;
    tmpo12 = tmp6 + tmp7;

    /* The rotator is modified from fig 4-8 to avoid extra negations. */
    z5 = (tmpo10 - tmpo12) * ((float) 0.382683433); /* c6 */
    z1 = (tmpe12 + tmpe13) * ((float) 0.707106781); /* c4 */
//    printf("z1: %f; tmpe12: %f, tmpe13: %f\n",z1,tmpe12,tmpe13);
    z2 = ((float) 0.541196100) * tmpo10 + z5; /* c2-c6 */
    z3 = tmpo11 * ((float) 0.707106781); /* c4 */
    z4 = ((float) 1.306562965) * tmpo12 + z5; /* c2+c6 */

    z11 = tmp7 + z3;		/* phase 5 */
    z13 = tmp7 - z3;

    dataptr[0] = tmpe10 + tmpe11; /* phase 3 */
    dataptr[1] = z11 + z4;
    dataptr[2] = tmpe13 + z1;	/* phase 5 */
    dataptr[3] = z13 - z2;
    dataptr[4] = tmpe10 - tmpe11;
    dataptr[5] = z13 + z2;	/* phase 6 */
    dataptr[6] = tmpe13 - z1;
    dataptr[7] = z11 - z4;

    dataptr += DCTSIZE;		/* advance pointer to next row */
  }

  /* Pass 2: process columns. */

  dataptr = data;
  for (ctr = DCTSIZE-1; ctr >= 0; ctr--) {
    tmp0 = dataptr[DCTSIZE*0] + dataptr[DCTSIZE*7];
    tmp7 = dataptr[DCTSIZE*0] - dataptr[DCTSIZE*7];
    tmp1 = dataptr[DCTSIZE*1] + dataptr[DCTSIZE*6];
    tmp6 = dataptr[DCTSIZE*1] - dataptr[DCTSIZE*6];
    tmp2 = dataptr[DCTSIZE*2] + dataptr[DCTSIZE*5];
    tmp5 = dataptr[DCTSIZE*2] - dataptr[DCTSIZE*5];
    tmp3 = dataptr[DCTSIZE*3] + dataptr[DCTSIZE*4];
    tmp4 = dataptr[DCTSIZE*3] - dataptr[DCTSIZE*4];

    /* Even part */

    tmpe10 = tmp0 + tmp3;	/* phase 2 */
    tmpe13 = tmp0 - tmp3;
    tmpe11 = tmp1 + tmp2;
    tmpe12 = tmp1 - tmp2;




    /* Odd part */

    tmpo10 = tmp4 + tmp5;	/* phase 2 */
    tmpo11 = tmp5 + tmp6;
    tmpo12 = tmp6 + tmp7;

    /* The rotator is modified from fig 4-8 to avoid extra negations. */
    z5 = (tmpo10 - tmpo12) * ((float) 0.382683433); /* c6 */
    z1 = (tmpe12 + tmpe13) * ((float) 0.707106781); /* c4 */
    z2 = ((float) 0.541196100) * tmpo10 + z5; /* c2-c6 */
    z3 = tmpo11 * ((float) 0.707106781); /* c4 */
    z4 = ((float) 1.306562965) * tmpo12 + z5; /* c2+c6 */

    z11 = tmp7 + z3;		/* phase 5 */
    z13 = tmp7 - z3;

    dataptr[DCTSIZE*0] = tmpe10 + tmpe11; /* phase 3 */
    dataptr[DCTSIZE*1] = z11 + z4;
    dataptr[DCTSIZE*2] = tmpe13 + z1; /* phase 5 */
    dataptr[DCTSIZE*3] = z13 - z2;
    dataptr[DCTSIZE*4] = tmpe10 - tmpe11;
    dataptr[DCTSIZE*5] = z13 + z2; /* phase 6 */
    dataptr[DCTSIZE*6] = tmpe13 - z1;
    dataptr[DCTSIZE*7] = z11 - z4;

    dataptr++;			/* advance pointer to next column */
  }

}
