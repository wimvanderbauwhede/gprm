#ifndef _CS_DCT_H_
#define _CS_DCT_H_
/* support macros for forward DCT routine from IJG jpeg library */

#define DCTSIZE 8
/*
 * This module is specialized to the case DCTSIZE = 8.
 */

#if DCTSIZE != 8
#error  "Sorry, this code only copes with 8x8 DCTs." /* deliberate syntax err */
#endif

#include "Types.h"
#include "Packet.h"
#include "Base/ServiceCore.h"

using namespace std;

namespace SBA {
namespace SCLib {

//Word_List cs_DCT(Base::ServiceCore*,MemAddresses&);

void *run_dct(void* voidp);

void calc_dct(int nruns);

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

void jpeg_fdct_float (float * data);

}; // SCLib
} // namespace SBA

#endif // _CS_DCT_H_
