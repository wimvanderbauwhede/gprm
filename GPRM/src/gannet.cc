/*
 *  (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */
#include "gannet.h"

void gprm(std::string tdc_file, const std::vector< void*>& args, unsigned int nthreads) {
	SBA::Runtime gannet(nthreads);
    //SBA::Runtime gannet(tdc_file,ncycles);
    gannet.init(tdc_file);
    //gannet.setNCycles(ncycles);
	gannet.run(args);
}

void gannetvm(std::string tdc_file, const std::vector< void*>& args, unsigned int ncycles=500) {
    SBA::Runtime gannet(tdc_file,ncycles);
    //gannet.setNCycles(ncycles);
	gannet.run(args);
}

void gannetvm(std::string tdc_file, unsigned int ncycles=500) {
    SBA::Runtime gannet(tdc_file,ncycles);
    //gannet.setNCycles(ncycles);
	gannet.run();
}
