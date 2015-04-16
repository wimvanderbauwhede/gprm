#ifndef _GANNET_H_
#define _GANNET_H_
/*
 *  (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */

#include <string>
#include <vector>
#include "../SBA/Types.h"
#include "../SBA/Runtime.h"

void gprm(std::string ,  const std::vector< void* >&, unsigned int );
void gannetvm(std::string ,  const std::vector< void* >&, unsigned int );
void gannetvm(std::string , unsigned int );

#endif // _GANNET_H_
