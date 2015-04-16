//
// :title: Service-based SoC project - SBA LookupTable class
//

/*
 *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */

//
// $Id$

#include "LookupTable.h"

using namespace std;
using namespace SBA;
#ifdef STATIC_ALLOC
	Word LookupTable::DJBHash(const Word w) {
		Word hash = 5381;

		for (std::size_t i = 0; i < NBYTES; i++) {
			hash = ((hash << 5) + hash) + ((w>>i) & 0xFF);
		}

		return (hash & 0x7FFFFFFF);
	}

    uint LookupTable::calc_key(Word addr) {
    	//Word haddr=DJBHash(addr);
        return (addr & NBINS_MASK);
    }
    //
    LookupTable::LookupTable () :  overflow_counter(0), occ_counter(0), binsz(BINSZ)  {
        for (uint i=0;i<NBINS;i++) {
            counter[i]=0;
        }

        for (uint j=0;j<LOOKUP_TABLE_SZ;j++) {
            kmem[j]=F_AllOnes;
            vmem[j]=F_AllOnes;
        }

    }

	/* write:
	1. calc hash
	2. get 3 LSBs
	3. get counter value for the bin
	if <3, store in bin & increase counter
	else
	        get overflow counter
	        store in overflow
	        increase counter
	*/
	void LookupTable::write(Word addr, Word val) {
	    uint key=calc_key(addr);
	    Word prev_addr=0;
	    Word prev_val=0;
	    if (counter[key]<binsz) {
	    	prev_addr=kmem[key*binsz+counter[key]];
	        prev_val=vmem[key*binsz+counter[key]];
	        kmem[key*binsz+counter[key]]=addr;
	        vmem[key*binsz+counter[key]]=val;
	        if (prev_addr!=addr) { // new key
	        	counter[key]++;
	        }

	    } else { //overflow
	    	if (overflow_counter<LOOKUP_TABLE_SZ/2) {
	        prev_addr=kmem[LOOKUP_TABLE_SZ/2+overflow_counter];
	        prev_val=vmem[LOOKUP_TABLE_SZ/2+overflow_counter];
	        kmem[LOOKUP_TABLE_SZ/2+overflow_counter]=addr;
	        vmem[LOOKUP_TABLE_SZ/2+overflow_counter]=val;
	        if (prev_addr!=addr) { // new key
	        	overflow_counter++;
	        }
	    	} else {
	    		// LookupTable is full!
	    	}
	    }
	    if (prev_addr==F_AllOnes and prev_val==F_AllOnes and overflow_counter<LOOKUP_TABLE_SZ/2) {
	    	occ_counter++; // this should only increment if the slot was empty before
	    }
	}

	/* read:
	1. calc hash
	2. get 3 LSBs
	3. loop through bin counter & check key
	if not in bin, loop through overflow
	4. returns -1 if not present
	*/
	Word LookupTable::read(Word addr) {
	    uint key=calc_key(addr);
	    for (uint i=0;i<counter[key];i++) {
	        if (kmem[key*binsz+i]==addr) {
	            return vmem[key*binsz+i];
	        }
	    }
	    for (uint i=0;i<overflow_counter;i++) {
	        if (kmem[LOOKUP_TABLE_SZ/2+i]==addr) {
	            return vmem[LOOKUP_TABLE_SZ/2+i];
	        }
	    }
	    return F_AllOnes;
	}

	// delete in Ruby
	void LookupTable::erase(Word addr) {
	    uint key=calc_key(addr);

	    for (uint i=0;i<counter[key];i++) {
	        if (kmem[key*binsz+i]==addr) {
	        	kmem[key*binsz+i]=F_AllOnes;
	            vmem[key*binsz+i]=F_AllOnes;
	        	counter[key]--;
	        	occ_counter--;
	        	break;
	        }
	    }
	    for (uint i=0;i<overflow_counter;i++) {
	        if (kmem[LOOKUP_TABLE_SZ/2+i]==addr) {
	        	kmem[LOOKUP_TABLE_SZ/2+i]=F_AllOnes;
	            vmem[LOOKUP_TABLE_SZ/2+i]=F_AllOnes;
	        	overflow_counter--;
	        	occ_counter--;
	        	break;
	        }
	    }
	}

	// length in Ruby
	uint LookupTable::size() {
	    return occ_counter;
	}

	// has_key? in Ruby
	uint LookupTable::count(Word addr) {
	    uint key=calc_key(addr);
	    for (uint i=0;i<counter[key];i++) {
	        if (kmem[key*binsz+i]==addr) {
	            return 1;
	        }
	    }
	    for (uint i=0;i<overflow_counter;i++) {
	        if (kmem[LOOKUP_TABLE_SZ/2+i]==addr) {
	            return 1;
	        }
	    }
	    return 0;
	}

#else
 		LookupTable::LookupTable () {}

		void LookupTable::write(Uint64 addr, Sint64 val) {
			lut[addr]=val;
		}

		Sint64 LookupTable::read(Uint64 addr) {
			return lut[addr];
		}
		// delete in Ruby
		void LookupTable::erase(Uint64 addr) {
			lut.erase(addr);
		}

		// length in Ruby
		uint LookupTable::size() {
			return lut.size();
		}

		// has_key? in Ruby
		uint LookupTable::count(Uint64 addr) {
			return lut.count(addr);
		}
#endif

