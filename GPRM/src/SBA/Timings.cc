/*
 * Timings.cc
 *
 *  Created on: Jan 9, 2009
 *      Author: wim
 */

#include "Timings.h"
#include <stdlib.h> // for NULL
#include <sys/time.h>

double wsecond() {
    struct timeval sampletime;
    double         time;

    gettimeofday( &sampletime, NULL );
    time = sampletime.tv_sec + (sampletime.tv_usec / 1000000.0);
    return( time );
}

