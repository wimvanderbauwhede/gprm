//
// :title: Gannet Service-based SoC project - SBA Memory class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: Memory.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/Memory.rb by ../../util/r2n.pl on Sun Apr 22 17:14:08 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******



#ifndef STATIC_ALLOC
#include "Memory.h"

using namespace std;
using namespace SBA;


	//* RAM Write operation
	void Memory::write(MemAddress address,Data data) {
		mput(address,data);
	}

	//* RAM Read operation
	Data Memory::read(MemAddress address) {
		return mget(address);
	}
#ifndef STATIC_ALLOC_
	//* Check if address is free
	bool Memory::is_free(MemAddress address) {
		return (has(address)?false:true);
	}
#endif
#endif
