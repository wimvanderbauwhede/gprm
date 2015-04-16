/** \file Types.cc

 \brief Gannet Service-based SoC project - SystemC NoC Transmitter/Receiver module

*/

/*
 *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */

//==============================================================================
//
// Gannet Types
//
//==============================================================================

// $Id$
#ifndef STATIC_ALLOC
#include "Types.h"

using namespace SBA;

       	//* List Write operation
	void Store::mput(unsigned int address,Word_List data) {
		Store::storage[address]=data;
	}

	//* List Read operation
	Word_List Store::mget(unsigned int address) {
		return Store::storage[address];
	}

	//*  Write operation
	void Store::put(unsigned int address,Word data) {
		Store::storage[address].pop_back();
		Store::storage[address].push_back(data);
	}

	//*  Read operation
	Word Store::get(unsigned int address) {
		return Store::storage[address].at(0);
	}
 	//* Check if address is in use
	bool Store::has(unsigned int address) {
		return (Store::storage.count(address)==1);
	}
	//* Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void Store::remove(unsigned int address) {
#ifndef STATIC_ALLOC_
		map_iter iter=Store::storage.find(address);
		if (iter!=Store::storage.end()) {
			Store::storage.erase(iter);
		}
#else
		Store::storage[address]=0;
#endif
	}

	//* For monitoring
	unsigned int Store::utilized(void) {
#ifndef STATIC_ALLOC_
		return Store::storage.size();
#else
		return DATA_SZ;
#endif
	}

#endif // STATIC_ALLOC
