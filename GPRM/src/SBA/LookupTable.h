/** 
    A simple Word-to-Word lookup table based on binning with an overflow bin

- assume LOOKUP_TABLE_SZ/2 (32) variables 
- assume NBINS (8) bins of 4 variables, i.e. look a log2(8)=3 LSBs
- if not in the bins, look in a 32-word overflow

I am making a slightly dangerous assumption: if the key is F_AllOnes and the value is F_AllOnes, the slot is empty.
So lookup[-1]=-1 spells disaster

What is the optimal number of bins for a given lookup table size?
The average lookup speed is BINSZ/2 for hits and LOOKUP_TABLE_SZ/4 for misses
So it depends on the ratio of hits to misses. According to the simple script I wrote to test this,
my intuitive feeling (64 bins) is almost correct: it is optimal if NBINS*BINSZ=1024
It also turns out that there is no need to have half of the memory as overflow. To store 1024 entries in 
the lookup table, 128 extra entries is sufficient. 

I'd make this a template class but then I have to merge the .cc and the .h

*/

#ifndef LOOKUP_TABLE_H_
#define LOOKUP_TABLE_H_

#ifdef STATIC_ALLOC
#define LOOKUP_TABLE_SZ 1024 // 64
#define NBINS 64 //8
#define BINSZ (LOOKUP_TABLE_SZ/2/NBINS) // 4
#define NBINS_MASK (NBINS-1) // 7 // 000..111
#else
#include <map>
#endif

#include "ServiceConfiguration.h"

//#include "Base/Types.h"
#include "Types.h"

using namespace std;

namespace SBA {
class LookupTable {
	
	private:
#ifndef STATIC_ALLOC	
		map<Uint64,Sint64> lut;
#else		 
	    Word kmem[LOOKUP_TABLE_SZ];
	    Word vmem[LOOKUP_TABLE_SZ];
	    uint counter[NBINS];    
	    uint overflow_counter;
	    uint occ_counter;
	    uint binsz;
	    
		Word DJBHash(const Word w);
	    
	    uint calc_key(Word addr) ;
#endif    
	public:

		LookupTable () ;
	    
#ifndef STATIC_ALLOC	
		void write(Uint64 addr, Sint64 val) ;	
	
		Sint64 read(Uint64 addr) ;
		
		// delete in Ruby
		void erase(Uint64 addr);
		
		// length in Ruby
		uint size() ;
		
		// has_key? in Ruby
		uint count(Uint64 addr) ;
#else
		void write(Word addr, Word val) ;	
	
		Word read(Word addr) ;
		
		// delete in Ruby
		void erase(Word addr);
		
		// length in Ruby
		uint size() ;
		
		// has_key? in Ruby
		uint count(Word addr) ;
#endif
};        

} // SBA namespace
#endif //  LOOKUP_TABLE_H_
