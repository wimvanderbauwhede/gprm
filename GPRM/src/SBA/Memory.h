
#ifndef MEMORY_H_
#define MEMORY_H_

//   
// :title: Gannet Service-based SoC project - SBA Memory class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk> 
//
// $Id: Memory.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/Memory.rb by ../../util/r2n.pl on Sun Apr 22 17:13:59 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#ifndef STATIC_ALLOC
#ifdef WV_SYSTEMC
#include <systemc.h>
#endif // WV_SYSTEMC
#include "Types.h"

using namespace std;

namespace SBA {

/**
 This class is a model for the RAM memory of the SBA Service Manager.
 It is a simple wrapper around a Store object
 Memory#read and Memory#write are aliases for Store#get and Store#put
 Memory#is_free is the inverse of Store#has
 */ 	
 
class Memory : public Store {
	public:
/*
 Main methods
*/

	/// RAM Write operation	
	void write(MemAddress address,Data data);
	/// RAM Read operation
	Data read(MemAddress address);
	/// Check if address is free
	bool is_free(MemAddress address);
};

} // namespace SBA
#endif


#endif // MEMORY_H_
