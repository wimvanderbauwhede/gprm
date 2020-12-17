/** \file Types.h

 \brief Gannet Service-based SoC project - General types for SBA


   (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi

   */


//==============================================================================
//
// General Types for Gannet
//
//==============================================================================

// $Id$

#ifndef _SBA_TYPES_H_
#define _SBA_TYPES_H_
#include <stdlib.h> // for exit()
#include <stdio.h> // for printf
#include <string>

#include <unordered_map>
#include <deque>

#if USE_THREADS==1
#include <pthread.h>
#endif

extern int appno; // Multiprogramming - Comes from the launcher.c to the main()

#include <iostream> // for cerr & cout!!

#include "ServiceConfiguration.h"

using namespace std;

//* Convenience macro for iterating over STL datastrcutures. Iterator is always name iter, and runs always from begin() to end().
#define foreach(_datastructure_type_,_datastructure_name_) \
for (_datastructure_type_::iterator iter=_datastructure_name_.begin();iter!=_datastructure_name_.end();iter++)

namespace SBA {
		typedef Word Symbol_t;

		typedef unsigned int CodeAddress; // typically fits into Task+Subtask
		typedef unsigned int MemAddress; // typically fits into Subtask
		typedef MemAddress  ServiceAddress;
		typedef MemAddress Subtask;
		typedef unsigned int Bit;
		typedef unsigned int Counter;
		typedef unsigned int Service; // 4 bytes
// FIXME: must be replaced by a custom lookup table, see Lookup_table.cc
//#ifndef STATIC_ALLOC
//		typedef map<Word,Word> LookupTable; // Typically the address will be the Subtask field, so we can use the rest for status, offset etc

//		typedef map<Word,Word> Address_Lookup; // Typically the address will be the Subtask field, so we can use the rest for status, offset etc
//#else
		//typedef LookupTable Address_Lookup;
//#endif
		struct StringPair { // used in Gateway
			string taskfile;
			string datafile;
		};


/** A template for a Perl/Ruby-style list with push/pop/shift/unshift/length methods
 * Note that pop() and shift() return the argument, unlike their STL counterparts.
 * shift/unshift operate at the front, push/pop at the back. So a proper FIFO uses only push/shift.
 * Of course all built-in deque methods are inherited as well.
 * Note that the deque<LType>:: qualifier is required for any inherited function
 * with no arguments that depend on a template parameter
 * WV11122010: It is not a good idea to inherit from an STL container
 * So we should use a wrapper instead
 */
#ifndef STATIC_ALLOC
template <typename LType> class List {
	private:
        deque<LType> lst;
        unsigned int wordsz;
	public:
//		typedef LType* iterator;
    	typedef std::_Deque_iterator<LType, LType&, LType*> iterator;
		LType shift() {
			LType t_elt=lst.front();
			lst.pop_front();
			return t_elt;
		}

		void unshift(LType& elt) {
			lst.push_front(elt);
		}
		void unshift(const LType& elt) {
			lst.push_front(elt);
		}
		void push(LType& elt) {
			lst.push_back(elt);
		}
		void push(const LType& elt) {
			lst.push_back(elt);
		}
		LType pop() {
			LType t_elt=lst.back();
			lst.pop_back();
			return t_elt;
		}
		LType& front() {
			return lst.front();
		}
		void pop_front() {
			lst.pop_front();
		}
		void pop_back() {
			lst.pop_back();
		}
		void push_back(LType& elt) {
			lst.push_back(elt);
		}
		void push_back(const LType& elt) {
			lst.push_back(elt);
		}
		unsigned int size() {
			return lst.size();
		}
		unsigned int length() {
			return lst.size();
		}
		void clear() {
			lst.clear();
		}
		LType& at(const int idx) {
		    return lst[idx];
		}
        const LType& operator[] (const int idx) const {
    	    return lst[idx];
        }
        LType& operator[] (const int idx) {
    	    return lst[idx];
        }
        List() : wordsz(sizeof(LType)) {};
		List(deque<LType>& d_) : wordsz(sizeof(LType))  {
			lst.swap(d_);
		};
		List(char* buf,int len) : wordsz(sizeof(LType)) {
    		int padding=len-nbytes*(len/wordsz);
    		int padword= (padding>0)?1:0;
    		char wordbytes[nbytes];
		    for (int i=0;i<len/wordsz;i++) {
		        wordbytes=buf+i*wordsz;
		        void* wordbytes_v=(void*)wordbytes;
		        LType elt=(LType)wordbytes_v;
		        lst.push_back(elt);
		    }
		    if (padding>0) {
		        int rest=nbytes-padding;
		        for (int j=0;j<wordsz;j++) {
		            if (j<rest) {
		            wordbytes[j]=buf[len/nbytes-1+j];
		            } else {
		            wordbytes[j]=0;
		            }
		        }
		        void* wordbytes_v=(void*)wordbytes;
		        LType elt=(LType)wordbytes_v;
		        lst.push_back(elt);
		    }
		}

		char* to_charbuf() {

    		char* charbuf=(char*)malloc(wordsz*lst.size());
    		int idx=0;
    		for (iterator iter_=lst.begin();iter_!=lst.end();iter_++) {
    		    LType elt = *iter_;
    		    void* elt_v=(void*)elt;
    		    char* wordbytes=(char*)elt_v;
                for (unsigned int i=0;i<wordsz;i++) {
                    charbuf[idx+i]=wordbytes[i];
                }
                idx+=wordsz;
    		}
    		return charbuf;
		}

		unsigned int nbytes() {
		    return wordsz*lst.size();
		}

		iterator begin() {
		    return (iterator)lst.begin();
		}

		iterator end() {
		    return  (iterator)lst.end();
		    }

}; // of List template
/*
template <typename LType> class List : public deque<LType> {
	public:
		LType shift() {
			LType t_elt=deque<LType>::front();
			deque<LType>::pop_front();
			return t_elt;
		}
		void push(LType& elt) {
			push_back(elt);
		}
		unsigned int length() {
			return deque<LType>::size();
		}
		void clear() {
			deque<LType>::clear();
		}
		List() {};

		List(deque<LType>& d_) {
//			(*this)(d_);
			this->swap(d_);
		};
}; // of List template
*/
// depth is ignored for dynamic alloc
//template <typename LType, Word depth> class Fifo  {
template <typename LType> class Fifo  {
    private:
        deque<LType> fifo;
        unsigned int _status;
	public:
        unsigned int status() {
            //std::cout << "Fifo status: "<<_status<<"\n";
            return _status;
        }
        bool has_packets() {
            return (_status==1);
        }
        LType front() {
        return fifo.front();
		}
		LType pop_front() {
			LType elt=fifo.front();
			fifo.pop_front();
			if (fifo.size()==0) _status=0;
			return elt;
		}

		LType shift() {
			LType t_elt=fifo.front();
			fifo.pop_front();
			if (fifo.size()==0) _status=0;
			return t_elt;
		}
		void unshift(LType& elt) {
			fifo.push_front(elt);
			_status=1;
		}
		void push(LType& elt) {
			fifo.push_back(elt);
			_status=1;
		}

		void push_back(LType& elt) {
			fifo.push_back(elt); // this throws an error in malloc
			_status=1;
		}

		LType pop() {
			LType t_elt=fifo.back();
			fifo.pop_back();
			if (fifo.size()==0) _status=0;
			return t_elt;
		}
		unsigned int size() {
			return fifo.size();
		}

		unsigned int length() {
			return fifo.size();
		}
		void clear() {
			fifo.clear();
			_status=0;
		}
		Fifo() : _status(0) {};
}; // of Fifo template

template <typename LType, Word depth> class Fifo_OLD : public deque<LType> {
	public:
		unsigned int status;
		LType shift() {
			LType t_elt=deque<LType>::front();
			deque<LType>::pop_front();
			if (deque<LType>::size()==0) status=0;
			return t_elt;
		}
		void unshift(LType& elt) {
			push_front(elt);
			status=1;
		}

		void push(LType& elt) {
            push_back(elt);
			status=1;
		}

		LType pop() {
			LType t_elt=deque<LType>::back();
			deque<LType>::pop_back();
			if (deque<LType>::size()==0) status=0;
			return t_elt;
		}

		unsigned int length() {
            return deque<LType>::size();
		}

		void clear() {
			deque<LType>::clear();
			status=0;
		}

		Fifo_OLD() : status(0) {};
}; // of Fifo_OLD template


#else // STATIC_ALLOC
template <typename LType, Word depth> class Fifo {
private:
	LType mem[depth];
	Word push_pointer;
	Word shift_pointer;
 	unsigned int _status;
public:

	Fifo () :  push_pointer(0), shift_pointer(0), _status(0) {};
 	unsigned int status() {
 	  return _status;
 	}
    bool has_packets() {
        return (_status==1);
    }
	void push(LType w) {
	    mem[push_pointer]=w;
	    if (push_pointer==depth-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
#ifdef VERBOSE
	    if (push_pointer>depth-1) {
	    	cerr << "Overflow: "<<push_pointer<<">"<<depth-1<<"\n";
	    	cout << "Overflow: "<<push_pointer<<">"<<depth-1<<"\n";
	    }
#endif //  VERBOSE
	    _status=1;
	}

	LType shift() {
	    LType w= mem[shift_pointer];

	    if (shift_pointer==depth-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	    unsigned int len=length();
	    if (len==0) _status=0;
	    return w;
	}

	LType unguarded_shift() {
	    LType w= mem[shift_pointer];
	    shift_pointer++;
	    return w;
	}

	LType pop() {
	    LType w=mem[push_pointer];
	    if (push_pointer==0) {
	        push_pointer=depth-1;
	    } else {
	        push_pointer--;
	    }
	    unsigned int len=length();
	    if (len==0) _status=0;
	    return w;
	}

	void unshift(LType w) {
	    mem[shift_pointer]=w;

	    if (shift_pointer==0) {
	        shift_pointer=depth-1;
	    } else {
	        shift_pointer--;
	    }
	    _status=1;
	}

	bool empty() {
	    return (push_pointer==shift_pointer);
	}
	// e.g. 8 spaces; 4,5,6,7,0 in use
	// occ is 5, but 0-4+1 = -3
	unsigned int length() {
		if (push_pointer>shift_pointer) {
	    	return push_pointer-shift_pointer;
	    } else if (push_pointer<shift_pointer) {
	    	return push_pointer-shift_pointer+depth;
	    } else {
	    	return 0;
	    }
	}
	unsigned int size() {
		return length();
	}

	bool full() {
	    return length()==depth;
	}

	void clear() {
		push_pointer=0;
		shift_pointer=0;
		_status=0;
	}
};
// This is a static implementation of a STL-style list. Main differences:
// fixed length
// no iterators
// assigning to at() or [] is not possible
template <typename LType, Word max_length> class List {
private:
	LType mem[max_length];
	uint push_pointer;
	uint shift_pointer;

public:
	List () : push_pointer(0), shift_pointer(0) {};
	List (uint length_) : push_pointer(0), shift_pointer(0) {}; // purely for byc2payload, to be compatible with dynamic case

	void push_back(LType w) { // push
	    mem[push_pointer]=w;
	    if (push_pointer==max_length-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
	}

	void push(LType w) { // push
	    push_back(w);
	}

	LType back() {// combine with pop_back() to do pop()
	    return mem[push_pointer];
	}
	void pop_back() { // combine with back() to do pop()
	    if (push_pointer==0) {
	        push_pointer=max_length-1;
	    } else {
	        push_pointer--;
	    }
	}

	LType pop() {
		LType w = back();
		pop_back();
		return w;
	}

	LType front() {// combine with pop_front() to do shift()
	    return mem[shift_pointer];
	}

	void pop_front() { // combine with front() to do shift()
	    if (shift_pointer==max_length-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	}

	LType shift() {
		LType w = front();
		pop_front();
		return w;
		/*
	    LType w= mem[shift_pointer];
	    if (shift_pointer==max_length-1) {
	        push_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	    return w;*/
	}



	LType& at(unsigned int address) {
		return mem[address];
	}
/*
	LType pop() {
	    LType w = mem[push_pointer];
	    if (push_pointer==0) {
	        push_pointer=max_length-1;
	    } else {
	        push_pointer--;
	    }
	    return w;
	}
*/
	void push_front(LType w) {
	    mem[shift_pointer]=w;

	    if (shift_pointer==0) {
	        push_pointer=max_length-1;
	    } else {
	        shift_pointer--;
	    }
	}

	void unshift(LType w) {
		push_front(w);
	}
/*
	bool empty() {
	    return (push_pointer==shift_pointer);
	}
*/
	// e.g. 8 spaces; 4,5,6,7,0 in use
	// occ is 5, but 0-4+1 = -3
	unsigned int size() {
		if (push_pointer>shift_pointer) {
	    	return push_pointer-shift_pointer;
	    } else if (push_pointer<shift_pointer) {
	    	return push_pointer-shift_pointer+max_length;
	    } else {
	    	return 0;
	    }
	}

	void clear() {
		push_pointer=0;
		shift_pointer=0;
	}

 	inline const LType& operator[] (const unsigned int i) const {
		return mem[i];
 	}
};

template <unsigned int length> class Static_Word_List {
private:
	Word mem[length];
	Word push_pointer;
	Word shift_pointer;

public:
	Static_Word_List () : push_pointer(0), shift_pointer(0) {};
	Static_Word_List (Uint16 length_) : push_pointer(0), shift_pointer(0) {}; // purely for byc2payload, to be compatible with dynamic case

	void push_back(Word w) { // push
	    mem[push_pointer]=w;
	    if (push_pointer==length-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
	}

	void push(Word w) { // push
	    mem[push_pointer]=w;
	    if (push_pointer==length-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
	}

	Word shift() {
	    Word w= mem[shift_pointer];
	    if (shift_pointer==length-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	    return w;
	}

	Word back() {// combine with pop_back() to do pop()
	    return mem[push_pointer];
	}
	void pop_back() { // combine with back() to do pop()
	    if (push_pointer==0) {
	        push_pointer=length-1;
	    } else {
	        push_pointer--;
	    }
	}
	void pop_front() { // combine with front() to do shift()
	    if (shift_pointer==length-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	}
	Word front() {// combine with pop_front() to do shift()
	    return mem[shift_pointer];
	}
	Word at(unsigned int address) {
		return mem[address];
	}

	void at(unsigned int address,Word val) {
		mem[address]=val;
	}


/*
	Word pop() {
	    Word w = mem[push_pointer];
	    if (push_pointer==0) {
	        push_pointer=depth-1;
	    } else {
	        push_pointer--;
	    }
	    return w;
	}

	void unshift(Word w) {
	    mem[shift_pointer]=w;

	    if (shift_pointer==0) {
	        push_pointer=depth-1;
	    } else {
	        shift_pointer--;
	    }
	}

	bool empty() {
	    return (push_pointer==shift_pointer);
	}
*/
	// e.g. 8 spaces; 4,5,6,7,0 in use
	// occ is 5, but 0-4+1 = -3
	unsigned int size() {
		if (push_pointer>shift_pointer) {
	    	return push_pointer-shift_pointer;
	    } else if (push_pointer<shift_pointer) {
	    	return push_pointer-shift_pointer+length;
	    } else {
	    	return 0;
	    }
	}
/*
	bool full() {
	    return occ()==depth;
	}

	unsigned int  status() {
			return (empty())?0:1;
	}
    bool has_packets() {
        return (_status==1);
    }

*/
	void clear() {
		push_pointer=0;
		shift_pointer=0;
	}

 	inline const Word& operator[] (const unsigned int i) const {
		return mem[i];
 	}
/*
 	const Word* payload()  const {
 		return mem+3;
 	}
 	const Word* header()  const {
 		return mem;
 	}
*/

	Static_Word_List<length> payload() {
		Static_Word_List<length> payload=*this;
		payload.shift_pointer+=3;
		return payload;
	}
/*
typedef unsigned int iterator;

unsigned int begin() {
	return shift_pointer;
}

unsigned int end() {
	return push_pointer;
}
*/
};

// This is a simple Array class, a trick really to pass arrays around
// Works only for numbers!!!
template <typename LType, Word size_> class Array {
private:
	LType mem[size_];
	Word length;
public:
	Array () : length(0) {
		for (uint i=0;i<size_;i++) {
			mem[i]=0;
		}
	}
	// Note that the return value must be a ref to be able to assign to it
 	inline LType& operator[] (const unsigned int i) {
		return mem[i];
 	}
 	// This is tricky: size() is required for compatibility with STL
 	// But it must be set externally!
 	void size(Word l) {
 		length=l;
 	}
 	Word size() {
 		return length;
 	}

 	void clear() {
		length=0;
		for (uint i=0;i<size_;i++) {
			mem[i]=0;
		}
 	}
};

#endif // STATIC_ALLOC

#ifndef STATIC_ALLOC
		typedef List<Word> Word_List;
		typedef deque<Word> List_Store;
		typedef deque<Word> Word_Fifo;
		typedef List<Word> Bytecode;
		typedef List< Bytecode > BytecodeQueue;
		typedef deque<MemAddress> MemAddresses; // was List
		typedef unordered_map<MemAddress,Word> Symbol_Table;
		typedef unordered_map<CodeAddress,uint> CodeStatus_Table;
#else // STATIC_ALLOC
		typedef Static_Word_List<MAX_PACKET_SZ> Word_List;
		typedef Static_Word_List<MAX_LIST_SZ> List_Store;
		typedef Fifo<Word,MAX_PACKET_SZ> Word_Fifo;
		typedef List<Word,MAX_BYC_SZ> Bytecode;
		typedef List< Bytecode, MAX_NPENDING_TASKS > BytecodeQueue;
		typedef Array< MemAddress, MAX_NARGS > MemAddresses;
		typedef Array<Word,DATA_SZ> Symbol_Table;
		typedef Array<uint,CODE_SZ> CodeStatus_Table;

#endif 	 // STATIC_ALLOC

		typedef Word_List Result;
		typedef Word_List Data;
		typedef Word_List Value; // WV: or Word?
		typedef Word_List Values;
		typedef void (*FuncPointer)();


		class ServicePair {
			public:
			FuncPointer core;
			ServiceAddress address; // WV10012011: I think this is obsolete
			ServicePair() {};
			ServicePair(ServiceAddress a_,FuncPointer fp_) : core(fp_), address(a_) {};
		};

		class DynConfigTuple {
		  public:
		      string lib;
		      string symbol;
		      DynConfigTuple () {};
		      DynConfigTuple (string l_,string s_) : lib(l_),symbol(s_) {};

		};
        typedef deque<StringPair> TaskDescList; // WV27-82008: TODO: get rid of this

#ifndef STATIC_ALLOC
	typedef Word_List Packet_t;
	typedef Word_List Header_t;
	typedef List<Packet_t> Packet_List;
#else
	typedef Static_Word_List<MAX_PACKET_SZ> Packet_t;
	typedef Static_Word_List<HEADER_SZ> Header_t;
	typedef List<Packet_t,PACKET_FIFO_SZ> Packet_List;
#endif // STATIC_ALLOC
	typedef Word_List Payload_t;
	typedef Word Payload_t_Word;

	//typedef Fifo<Packet_t,PACKET_FIFO_SZ> Packet_Fifo;
	typedef Fifo<Packet_t> Packet_Fifo;
	typedef Packet_Fifo TX_Packet_Fifo;


#if USE_THREADS==0
	typedef Packet_Fifo TRX_Packet_Fifo;
	typedef Packet_Fifo RX_Packet_Fifo;
#else // USE_THREADS==1

//template <typename Packet_t, Word depth>
class RX_Packet_Fifo {
	private:
    	deque<Packet_t> packets;
    	pthread_mutex_t _RXlock;
    	pthread_cond_t  _RXcond;
 	bool _status;
	public:
 		RX_Packet_Fifo () :  _status(0) {
 			pthread_mutex_init(&_RXlock, NULL);
 			pthread_cond_init(&_RXcond, NULL); // was 0
 		};
 		~RX_Packet_Fifo() {
 			pthread_mutex_destroy(&_RXlock);
 			pthread_cond_destroy(&_RXcond);
 		}
 	bool status() {
 		//FIXME: make this blocking? Ashkan added lock & unlock
 		pthread_mutex_lock(&_RXlock);
 		bool stat = _status;
 		pthread_mutex_unlock(&_RXlock);
     	return stat;
 	}

/* Ashkan commented this. Using has_packets() without locking is dangerous.
    bool has_packets() {
    	return (packets.size()>0);
    }
*/
    bool has_packets() {
// we want to block until the status is true
// so status() will block until it can return true
 	  pthread_mutex_lock(&_RXlock);
 	 bool has = (_status==1);
 	  pthread_mutex_unlock(&_RXlock);
 	  return(has);
    }

    void wait_for_packets(int address) {
    // we want to block until the status is true
      // so status() will block until it can return true
        pthread_mutex_lock(&_RXlock);
        while(packets.empty())
        {
            pthread_cond_wait(&_RXcond, &_RXlock);
        }
        _status=1;
        pthread_mutex_unlock(&_RXlock);
#ifdef VERBOSE
    cout << "RX_Packet_Fifo: UNBLOCK on wait_for_packets()\n";
#endif
	}

/*    void push(Packet_t const& data)
    {
        pthread_mutex_lock(&_RXlock);
        packets.push_back(data);
        _status=1;
        pthread_mutex_unlock(&_RXlock);
        pthread_cond_signal(&_RXcond); // only 1 thread should be waiting
    }

    bool empty() {
        pthread_mutex_lock(&_RXlock);
//        boost::mutex::scoped_lock lock(_RXlock);
        return packets.empty();
        pthread_mutex_unlock(&_RXlock);
    }

    unsigned int length() { // do we need to lock before checking the length?

        return packets.size();

    }

    unsigned int size() { // do we need to lock before checking the length?

        return packets.size();
    }

   We don't really want to block on shift()
 So I have to check when the fifo has 1 elt, then
 set status to 0 and shift that elt. As we check status we won't come back there
 then we use the blocking call simply to set the status back to 1

    Packet_t shift() {
        pthread_mutex_lock(&_RXlock);

        if (packets.size()==1) {
        	_status=0;
        }
        Packet_t t_elt=packets.front();
        packets.pop_front();
        pthread_mutex_unlock(&_RXlock);

		return t_elt;
    }
*/
    void push_back(Packet_t const& data) {
        pthread_mutex_lock(&_RXlock);
        const bool was_empty = (_status==0);
        packets.push_back(data);
        _status=1;
        pthread_mutex_unlock(&_RXlock);
        if (was_empty)
          pthread_cond_signal(&_RXcond); // only 1 thread should be waiting
      }

/*    Packet_t front() {
#ifdef VERBOSE
        cout << "front()\n";
#endif
//        pthread_mutex_lock(&_RXlock);
        Packet_t t_elt=packets.front();
//        pthread_mutex_unlock(&_RXlock);
		return t_elt;
    } Ashkan changed the pop_front to return a value.
*/
    Packet_t pop_front() {
    	pthread_mutex_lock(&_RXlock);
#ifdef VERBOSE
        cout << "RX_Packet_Fifo: pop_front()\n";
#endif
        while(packets.empty()) { // this is for double check, it should not happen
        	cout << "RX_Packet_Fifo: Thread Blocked For Some Strange Reason!" << endl;
        	pthread_cond_wait(&_RXcond, &_RXlock);
        }

        Packet_t t_elt=packets.front();
        packets.pop_front();
        if (packets.size() == 0) _status=0;
        pthread_mutex_unlock(&_RXlock);

#ifdef VERBOSE
        cout << "RX_Packet_Fifo: DONE pop_front() \n";
#endif
        return t_elt;
    }

/*	void clear() {
		packets.clear();
	}*/
}; // RX_Packet_Fifo

#endif // USE_THREADS

// ------------------------------------------------

#ifndef STATIC_ALLOC
		typedef unordered_map<Service,ServicePair> ServiceMap;
		typedef unordered_map<uint,DynConfigTuple> Configurations;
#else // STATIC_ALLOC
		class ServiceMap {
		private:
			ServicePair mem[MAX_NSERVICES]; // should become 256*256
		public:
			ServiceMap () {}
			// Note that the return value must be a ref to be able to assign to it
 			inline ServicePair& operator[] (const unsigned int i) {
				return mem[i];
 			}
		};
		class Configurations {
		private:
			DynConfigTuple mem[MAX_NDYNCONFIGS];
		public:
			Configurations () {}
			// Note that the return value must be a ref to be able to assign to it
 			inline DynConfigTuple& operator[] (const unsigned int i) {
				return mem[i];
 			}
		};

#endif 	 // STATIC_ALLOC

		/** SBA::Store is meant to be a generic model of a storage for arbitrary objects, hence the use of boost::any */
#ifndef STATIC_ALLOC
class Store {
#else
template <uint size> class Store {
#endif

	private:

#ifndef STATIC_ALLOC
	unordered_map<unsigned int,Word_List> storage;
	unordered_map<unsigned int,Word> storageWord;
	typedef unordered_map<unsigned int,Word_List>::iterator map_iter;
#else
	Word_List storage[size];
#endif // STATIC_ALLOC

	public:
#ifdef STATIC_ALLOC
       	//* List Write operation
	void (unsigned int address,Word_List data) {
		storage[address]=data;
	}

	//* List Read operation
	Word_List mget(unsigned int address) {
		return storage[address];
	}

	//*  Write operation
	void put(unsigned int address,Word data) {
		storage[address].pop_back();
		storage[address].push_back(data);

	}

	//*  Read operation
	Word get(unsigned int address) {
		return storage[address].at(0);
	}
	//* Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void remove(unsigned int address) {
		storage[address]=0;
	}

	//* For monitoring
	unsigned int utilized(void) {
		return size;
	}


#else // not STATIC_ALLOC
/*
	/// List Write operation
	void mput(unsigned int address,Word_List data);
	/// List Read operation
	Word_List mget(unsigned int address);
#ifdef RAM_FIFO
	// RAM-FIFO Push operation for list of words
	void mpush(unsigned int address,Word_List data);
    // RAM-FIFO Shift operation for list of words
    Word_List mshift(unsigned int address);
        // RAM-FIFO Head operation for list of words
    Word_List mhead(unsigned int address);
#endif // RAM_FIFO

	/// Write operation
	void put(unsigned int address,Word data);
	/// Read operation
	Word get(unsigned int address);

	/// Check if address is in use
	bool has(unsigned int address);

	/// Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void remove(unsigned int address);
	/// For monitoring
	unsigned int utilized(void);
*/
       	// * List Write operation
	void mput(unsigned int address,Word_List data) {
		//cout << "From mput:  " << data[0] <<endl;
		//Uint64 d1 = data[0];
		//printf("mput...before storage**** = %u\n",d1);
		storage[address]=data;
	}
	void mputWord(unsigned int address,Word data) {
			//cout << "From mputWord:  " << data <<endl;
			//Uint64 d1 = data[0];
			//printf("mput...before storage**** = %u\n",d1);
			storageWord[address]=data;
		}

   	// * List Append operation
	void mpush(unsigned int address,Word_List data) {
		for (Word_List::iterator iter_=data.begin();iter_!=data.end();iter_++) {
			Word w=*iter_;
			//storage[address].push_back(w);
			storage[address].push_back(w);
		}
		/*
		 If SBA::List was a list we could use merge; if it was a vectore we could use reserve and insert
		*/
	}

	Word mgetWord(unsigned int address) {
		//cout << "storageWord:" << storageWord[address] << endl;
		return storageWord[address];
		}
	// * List Read operation

	Word_List& mget(unsigned int address) {
		return storage[address];
	}

	// *  Write operation
	void put(unsigned int address,Word data) {
		//storage[address].pop_back();
		//storage[address].push_back(data);
		storage[address].pop_back();
		storage[address].push_back(data);
	}

	// *  Read operation
	Word get(unsigned int address) {
		//return storage[address].at(0);
		return storage[address].at(0);
	}
 	// * Check if address is in use
	bool has(unsigned int address) {
		return (storage.count(address)==1);
	}
	// * Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void remove(unsigned int address) {
		map_iter iter=storage.find(address);
		if (iter!=storage.end()) {
			storage.erase(iter);
		}
	}

	// * For monitoring
	unsigned int utilized(void) {
		return storage.size();
	}

#endif // STATIC_ALLOC

}; // end of Store class definition

//Pointer manipulation functions
// Return a pointer of type PType
template <typename PType> PType* getPointer(Word_List wl) {
	Word w = wl[1]; // Pointers are stored a Ext UInt
	void* vp = (void*)w;
	PType* tp=(PType*)vp;
	return tp;
}
// The above unwrap function does not work for objects
template <class OType> OType& getObjRef(Word_List wl) {
	Word w = wl[1];
	void* vp = (void*)w;
	OType* tp=(OType*)vp;
	OType& tt=*tp;
	return tt;
}
// stow a pointer of type PType
template <typename PType> Word_List putPointer(PType *tp) {
	Word_List wl;
	void* vp = (void*)tp;
	Word ivp =(Word)vp;
	wl.push_back(EXTSYM);
	wl.push_back(ivp);
	return wl;
}
// The above wrap function does not work for objects
template <class OType> Word_List putObject(OType t) {
	Word_List wl;
	OType *tp = new OType(t); // copy
	void* vp = (void*)tp;
	Word ivp =(Word)vp;
	wl.push_back(EXTSYM);
	wl.push_back(ivp);
	return wl;
}


} // SBA
#endif /*_SBA_TYPES_H_*/
