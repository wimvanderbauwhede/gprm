/** \file ServiceManagerObjects.h

 \brief Gannet Service-based SoC project - SystemC Service Manager Objects

 	This is a collection of objects used in the ServiceManager
 	The main classes are:
 	-Lookup
 	-Stack
 	-Subtask_List
 	All have a number of dependent classes and typedefs, see below.

*/

/*
 *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */

//==============================================================================
//
// Service Manager Module
//
//==============================================================================

// $Id$

#ifndef SERVICE_MGR_OBJECTS_H_
#define SERVICE_MGR_OBJECTS_H_

//#include <string>
#ifndef STATIC_ALLOC
#include <unordered_map>
#include <stack>
#endif
#include <vector>
//#include <deque>


#include "Types.h"
#include "Packet.h"
#include "ServiceConfiguration.h"
/*
#include "SystemConfiguration.h" // for S_LOOP. FIXME!
*/

using namespace std;

namespace SBA {

typedef Word_List Labels;
typedef unsigned int Address;
//typedef vector< Address > Addresses;
//typedef vector< unsigned int > SubtaskRefs;

// RegData_StatusList is a bitword that contains the status
// 1 (present) or 0 (absent/cleared) for every accessor of the buffer
// as worked out by the compiler
// I could add  accessor functions getRDStatus(uint i), setRDStatus(uint i, RegData_Status s)
// or I could simply create a class to do this.

class RegData_StatusWord {
	public:
		RegData_StatusWord () : statusword(0) {};
		void set(uint i, RegData_Status s) {
			uint status_i = (s << i);
			uint statusmask_i = RDS_MASK - (1 << i);
			statusword = (statusword & statusmask_i) + status_i;
		}
		uint get(uint i) {
			uint status = (statusword >> i) & 1;
			return status;
		}
	private:
		uint statusword;
};

class RegisterEntry {
	public:
	MemAddress data_address;
	//TODO: change this to a list or a
	RegData_StatusWord statusword;
	RegData_Status status;
	MemAddress subtask_address;
	CodeAddress code_address;
	// tuple support
	uint offset; // 1 byte
	uint fsize; // 1 byte

RegisterEntry () :
		data_address(0),
		status(RDS_absent),
		subtask_address(0),
		code_address(0)
		{};

	RegisterEntry (MemAddress da_, CodeAddress ca_, MemAddress sa_, RegData_Status st_) :
		data_address(da_),
		status(st_),
		subtask_address(sa_),
		code_address(ca_),
		offset(0),
		fsize(0)
		{};
};

//class State_Register {
//	private:
//	MWord mem[STATE_REG_SZ];
//	public:
//	// maybe best to initialise all states to 0
//	State_Register () {
//		for (int i=0;i<STATE_REG_SZ;i++) {
//			mem[i]=0;
//		}
//	}
// 	inline MWord& operator[] (const unsigned int i) {
//		return mem[i];
// 	}
//};

class State_Register {
	private:
		std::unordered_map<unsigned int,unsigned int> _state;
		std::unordered_map<unsigned int,MWord> _store;
	public:

	State_Register () {
	}
	inline MWord& get(const unsigned int service_id) {
		return _store[service_id];
	}
	inline void put(const unsigned int service_id, MWord ip) {
		_store[service_id]=ip;
	}
	inline void* getvp(const unsigned int service_id) {
		return (void*)_store[service_id];
	}

	inline void putvp(const unsigned int service_id, void* vp) {
		MWord ip =(MWord)vp;
		_store[service_id]=ip;
	}

	inline unsigned int get_state(unsigned int service_id) {
		return _state[service_id];
	}
	inline void set_state(unsigned int service_id, unsigned int st) {
		_state[service_id]=st;
	}

	inline void init(unsigned int service_id) {
		_state[service_id]=1;
	}
	inline void remove(unsigned int service_id) {
		_state.erase(service_id);
		_store.erase(service_id);
	}
	inline bool isEmpty(unsigned int service_id) {
			return (_state.count(service_id)==0);
	}

// 	inline MWord& operator[] (const unsigned int i) {
//		return store[i];
// 	}
};



//typedef Fifo<Word,NREQS> Requests;
#ifdef STATIC_ALLOC
typedef List<Word,NREQS> Requests;
#else
typedef List<Word> Requests;
#endif

class RequestTable {
	private:
#ifdef STATIC_ALLOC
        List<Requests,NREGS> requests_table;
#else
		List<Requests> requests_table;
#endif
    public:

	//------------------------------------------------------------------------------
	//  RequestTable::push()
	//------------------------------------------------------------------------------
    RequestTable () {
            for (unsigned int i=0;i<NREGS;i++) {
                Requests emptywl;
                requests_table.push(emptywl);
            }
        };
	//void RequestTable<SIZE> :: push(SBA::MemAddress address,SBA::Word item)
	void push(const MemAddress address,Word item)
	{
	    requests_table.at(address).push(item);
	}// funct: RequestTable :: push()


	//------------------------------------------------------------------------------
	//  RequestTable::shift()
	//------------------------------------------------------------------------------
	Word shift(const MemAddress address) {
	    Word item = requests_table.at(address).shift();
	    return item;
	}// funct: RequestTable :: shift()


	//------------------------------------------------------------------------------
	//  RequestTable::size()
	//------------------------------------------------------------------------------
	unsigned int size(const MemAddress address)
	{
	    unsigned int item = requests_table.at(address).size();
	    return (item);
	}// funct: RequestTable :: size()

};

/**
This class is a model for the address lookup table of the SBA Service Manager.
Lookup bind binds a label to an address. The lookup table is special because
it allows lookup of label by address as well as adress by label.
Lookup::labels_for returns a list of labels refering to the address argument;
Lookup::address returns the address refered to by the label argument.
*/

// WV 17062008: Obsolete now, even without STATIC_ALLOC
/*

class AddressLookupValue {
public:
	Address address;
	Data_Status status;
	SubtaskRefs refs;
	bool stream;
//	AddressLookupValue (Address addr_,bool stream_) : address(addr_), stream(stream_) {};
};

typedef	map< Label, AddressLookupValue > AddressLookup;
typedef	map< Address, Labels > LabelLookup;

class Lookup {
private:
	AddressLookup address_lookup;
	LabelLookup label_lookup;

public:
	void bind(Label& label,Address address) {
		Symbol_t tsym(label);
		bool stream = ((tsym & F_Name) >> FS_Name)==S_LOOP; //FIXME WV: bit of a hack, really. Should move this check to Service Manager and add an arg to bind or set_stream method
		//		address_lookup[label]={};
		address_lookup[label].address=address;
		address_lookup[label].stream=stream;
		//		label_lookup[address]=[] unless label_lookup.has_key?(address);
		label_lookup[address].push_back(label);
		//		label_lookup[address].push(label);
		address_lookup[label].status=DS_absent;
		//		address_lookup[label].refs=[];
	}

	bool stream(Label& label) {
		return address_lookup[label].stream;
	}

	bool has_label(Label& label) {
		return (address_lookup.count(label)>0);
	}

	bool has_address(Address address) {
		return (label_lookup.count(address)>0);
	}

	Labels labels() {
		Labels labels;
		foreach(AddressLookup,address_lookup) {
			labels.push_back(iter->first);
		}
		return labels;
		//return address_lookup.keys;
	}

	Labels& labels_for(Address address) {
		//	Labels labels_for(Address address) {
		return label_lookup[address];
	}

	Addresses addresses() {
		Addresses addresses;
		foreach(LabelLookup,label_lookup) {
			addresses.push_back(iter->first);
		}
		return addresses;
		//return label_lookup.keys;
	}

	void set_status(Label& label,Data_Status status_) {
		address_lookup[label].status=status_;
	}

	Data_Status status(Label& label) {
		return address_lookup[label].status;
	}

	SubtaskRefs& refs(Label& label) {
		return address_lookup[label].refs;
	}

	void add_ref(Label& label,unsigned int ref) {
		address_lookup[label].refs.push_back(ref);
	}

	Address pop_ref(Label& label) {
		Address elt=address_lookup[label].refs.back();
		address_lookup[label].refs.pop_back();
		return elt;
	}

	void del_ref(Label& label,unsigned int ref) {
		SubtaskRefs newrefcount;
		foreach(SubtaskRefs,address_lookup[label].refs) {
			if (*iter !=  ref) {
				newrefcount.push_back(*iter);
			} else {
#ifdef VERBOSE
				cout << "\tRemoved subtask " << ref << " from refcount for " << label << "\n";
#endif // VERBOSE
			}
		}
		address_lookup[label].refs=newrefcount;
	}

	Address address(Label& label) {
		if (address_lookup.count(label)>0) {
			return address_lookup[label].address;
		} else {
			return 0;
		}
	}

	Address remove(Label& label) {
		Address address=address_lookup[label].address;
		address_lookup.erase(label);
		Labels new_label_list;
#ifndef STATIC_ALLOC
		foreach(Labels,label_lookup[address]) {
			Label tmpl=*iter;
#else
		for (uint i=0;i<label_lookup[address].size();i++) {
			Label tmpl=label_lookup[address].back();
			label_lookup[address].pop_back();
#endif
			if (tmpl!=label) {
				new_label_list.push_back(tmpl);
			}
		}
		label_lookup[address]=new_label_list;
		if (label_lookup[address].size()==0) {
			label_lookup.erase(address);
		}
		return address;
	}

	unsigned int occ() {
		return label_lookup.size();
	}
	string inspect() {
		return string("FIXME: NOT IMPLEMENTED\0");
	}

}; // Lookup
*/
// -----------------------------------------------------------------------------
/** A simple stack model. Its main purpose is to initialise the STL stack.
 * TODO: all address stacks should be implemented in RAM
 * So the Stack module should provide stack-like access to RAM, i.e.
 * it should know the size and offset of the stack and keep a stack counter.
 */
#ifdef STATIC_ALLOC
using namespace SBA;

template <Word stack_size> class Stack {
private:
Word mem[stack_size];
Word stack_pointer;
Word offset;
public:
Stack (Word offset_) : stack_pointer(0), offset(offset_) {
	fill();
}

void fill() {
    for (Word i=0;i<stack_size;i++) {
        mem[i]=i+offset;
    }
    stack_pointer=stack_size-1;
}

void push(Word w) {

    if (stack_pointer<stack_size-1) {
        stack_pointer++;
        mem[stack_pointer]=w;
    } else {
    	if (stack_pointer==stack_size) {
	        stack_pointer=0;
    	    mem[stack_pointer]=w;
    	}
#ifdef VERBOSE
    	else {

    		cout << "STACK OVERFLOW: "<<stack_pointer<<" > "<<stack_size-1<<endl;
    	}
#endif // VERBOSE
    }
}

Word pop() {

    Word w=mem[stack_pointer];
    if (stack_pointer>0) {
        stack_pointer--;
    } else {
#ifdef VERBOSE
    	cout << "STACK UNDERFLOW\n";
#endif // VERBOSE
    	stack_pointer=stack_size;
    }
    return w;
}

bool empty() {
    return (stack_pointer==0);
}

uint size() {
	if (stack_pointer<stack_size) {
    return stack_pointer+1;
	} else {
		return 0;
	}
    }
/*
bool full() {
    return mem[depth]==depth-1;
}
*/
};

#else
template <Word stack_size> class Stack {
private:
	stack<Address> address_stack;
public:
	//* Constructor: Fill the stack with addresses
//	Stack(unsigned int stack_size,unsigned int offset) {
	Stack(unsigned int offset) {
		for (unsigned int i = 0;i<stack_size;i++) {
			address_stack.push(i+offset);
		}
	}

	//* Push a freed address back onto the stack.
	void push(unsigned int item) {
		address_stack.push(item);
	}
	void push_back(unsigned int item) {
		address_stack.push(item);
	}
	//* Pop an address to use off the stack.
	unsigned int pop() {
		unsigned int top=address_stack.top();
		address_stack.pop();
		return top;
	}
	//* Check if stack is empty
	bool empty() {
		return address_stack.empty();
	}
/*
	string inspect() {
		return string("FIXME: NOT IMPLEMENTED");
	}
*/

	unsigned int size() {
		return address_stack.size();
	}
};
#endif // STATIC_ALLOC
// -----------------------------------------------------------------------------

//typedef map< Word, Argument_Status > SymbolStatus;
//typedef map< Word, Kind_t > SymbolKind;
typedef Word Argument;
//typedef List< Symbol_t > Arguments;
//typedef vector< Symbol_t > Arguments;
typedef Word_List Arguments;
//typedef map< Argument_Status, Arguments > ByStatus;

typedef vector< Word > Subtask_Argument_List;
typedef vector< Symbol_t > Subtask_Symbol_List;
//typedef MemAddresses Subtask_Argument_List;

// -----------------------------------------------------------------------------
/*
 WV22dec2012 I want to extend this with status for each argument; we might want to store Kind and Quoted as well
 The idea is to replace the symbol_table with these entries

 Word arg_symbol=elt;
Kind Quoted Ext Status Subtask Name
Subtask -> parent_subtask => no need
Status -> argument status

                arg_symbol=setSubtask(arg_symbol,parent_subtask);
                arg_symbol=setStatus(arg_symbol,DS_absent);
				arg_symbol=setStatus(arg_symbol,DS_requested)
				symbol_table[data_address]=arg_symbol;

The easiest way is to simply store the symbol in parallel with the argument and replace the status
 */
class Subtask_List_Item {
public:
	Subtask_Status status; // not needed for migration
	uint nargs; // max 16, 4 bits
	uint nargs_absent; // not needed for migration
	uint mode; // 2 bits
	uint reg; // 3 bits
	Subtask_Symbol_List symbols; // includes DS_Status! 16*64b
	Subtask_Argument_List arguments; // 16*64b
	char argmodes[MAX_NARGS]; // OBSOLETE // 16*8b
	Symbol_t called_as; // original reference symbol for the subtask 64b

	Service to; // the destination for the original reference packet 16b
	Service return_to; // the return address for the original reference packet 16b
	Word return_as; // the location to store the result 64b
	// for redirection and multicasting
    uint redir;  // OBSOLETE
    Word ack_to;  // OBSOLETE as soon as return_as takes this value
	uint waiting_for_ack; // OBSOLETE
	bool waiting; // not needed for migration
	CodeAddress code_address; // 32b

    uint service_id; // 8 bits? or 4, if we limit the service to 4
// tuple support
    uint offset; // OBSOLETE
    uint fsize; // OBSOLETE
    MemAddress result_address; // OBSOLETE
    Word result; // 64b
    bool migrated; 	// To determine if the subtask is migrated already!
    int target; 		// The actual target core on which the subtask is running

	Subtask_List_Item() :
			status(STS_new),
			nargs(0),
			nargs_absent(MAX_NARGS),
			mode(0),
			reg(0),
			called_as(0),
			to(0),
			return_to(0),
			return_as(0),
			redir(0),
			ack_to(0),
			waiting_for_ack(0),
			waiting(false),
			code_address(0),
			service_id(0),
			offset(0),
			fsize(0),
			result_address(0), // OBSOLETE
			result(0),
			migrated(false),
			target(-1)
	            {
#ifdef STATIC_ALLOC
	            	symbols.size(MAX_NARGS);
	            	arguments.size(MAX_NARGS);
#endif
	            };
			      void clear_arguments () {
			    	  symbols.clear();
			    	  arguments.clear();
			      }

	/*//Copy constructor
	Subtask_List_Item(const Subtask_List_Item&);

	//Assignment operator
	Subtask_List_Item& operator=(const Subtask_List_Item& a);*/
}; // of Subtask_List_Item

#ifndef STATIC_ALLOC
typedef List< Subtask > Subtasks;
typedef unordered_map< Subtask, Subtask_List_Item > SubtaskMap;
#else
typedef Static_Word_List<CODE_SZ> Subtasks;
#endif
// ----------------------------------------------------------------------------
class Subtask_List {
private:
#ifndef STATIC_ALLOC
	SubtaskMap subtasks_list;
#else
	Subtask_List_Item subtasks_list[CODE_SZ];
	bool occ[CODE_SZ];
#endif
public:
#ifdef STATIC_ALLOC
	Subtask_List () {
		for (uint i=0;i<CODE_SZ;i++) {
			occ[i]=false;
		}
	}
#endif


	void add (const Subtask subtask) {
		Subtask_List_Item item;
		subtasks_list[subtask]=item;
		subtasks_list[subtask].clear_arguments();
#ifdef STATIC_ALLOC
		occ[subtask]=true;
#endif
	}

	void remove (const Subtask subtask) {

#ifdef VERBOSE
//		cout <<"DELETING "<<subtask<<endl;
#endif // VERBOSE
//		subtasks_list.erase(subtask);
		subtasks_list[subtask].status=STS_deleted;
		subtasks_list[subtask].nargs_absent=MAX_NARGS;
		subtasks_list[subtask].nargs=0;
		subtasks_list[subtask].clear_arguments();
#ifdef STATIC_ALLOC
		occ[subtask]=false;
#endif
	}

	const Subtask_List_Item get(Word arg) {
		return subtasks_list[(Subtask)arg];
	}

	void cpy(Subtask subtask, Subtask_List_Item item) {
		/*for( std::vector<Symbol_t>::const_iterator i = item.symbols.begin(); i != item.symbols.end(); ++i)
		    std::cout <<"Ashkan: "<<hex<< *i <<dec<<' ';*/
		subtasks_list[subtask] = item;
		/*for( std::vector<Symbol_t>::const_iterator i = subtasks_list[subtask].symbols.begin(); i != subtasks_list[subtask].symbols.end(); ++i)
			std::cout <<"Ashkan: "<<hex<< *i <<dec<< ' ';*/
	}

	void migrated(Subtask subtask, bool b) {
		subtasks_list[subtask].migrated = b;
	}

	ServiceAddress get_target(Subtask subtask) {
		return subtasks_list[subtask].target;
	}

	void set_target(Subtask subtask, ServiceAddress new_target) {
		subtasks_list[subtask].target = new_target;
	}

	Subtask_Argument_List& arguments(const Subtask subtask) { // but not const
		return subtasks_list[subtask].arguments;
	}
	Subtask_Symbol_List& symbols(const Subtask subtask) { // but not const
		return subtasks_list[subtask].symbols;
	}
	Word argument(const Subtask subtask,unsigned int idx) { // but not const
		// HACK!!! FIXME!!!
if (subtasks_list.count(subtask)>0) {
	if (subtasks_list[subtask].arguments.size()>0) {
		return subtasks_list[subtask].arguments.at(idx);
	} else {
		return 0;
	}
} else {
	return 0;

}
	}

	void argument(const Subtask subtask,unsigned int idx,Word arg) { // but not const
		//cout << "Ashkan-SMObject: This is arg: "<< hex <<arg <<dec<< endl;
		subtasks_list[subtask].arguments.at(idx)=arg;
	}

	Word symbol(const Subtask subtask,unsigned int idx) { // but not const
		return subtasks_list[subtask].symbols.at(idx);
	}

	void symbol(const Subtask subtask,unsigned int idx,Word arg) { // but not const
		subtasks_list[subtask].symbols.at(idx)=arg;
	}

	// must have is_quoted and arg_status methods too

	char* argmodes(const Subtask subtask) { // but not const
		return subtasks_list[subtask].argmodes;
	}

    void argmodes(const Subtask subtask, unsigned int idx, char argmode) { // but not const
		subtasks_list[subtask].argmodes[idx]=argmode;
	}

	Subtask_Status status (const Subtask subtask) {
		return subtasks_list[subtask].status;
	}

	void status (const Subtask subtask,Subtask_Status status_) {
		subtasks_list[subtask].status=status_;
#ifdef VERBOSE
		//cout << "Subtask_List: Set status for "<<subtask<<" to "<<status_<<":"<<subtasks_list[subtask].status<<"\n";
#endif // VERBOSE
	}

	Word return_as (const Subtask subtask) {
		return subtasks_list[subtask].return_as;
	}
	void return_as (const Subtask subtask,Word return_as_) {
		subtasks_list[subtask].return_as=return_as_;
#ifdef VERBOSE
//		cout << "Subtask_List: Set return_as for "<<subtask<<" to "<<return_as_<<":"<<subtasks_list[subtask].return_as<<"\n";
#endif // VERBOSE
	}
	Symbol_t called_as(const Subtask subtask) {
		return subtasks_list[subtask].called_as;
	}
	void called_as(const Subtask subtask,Symbol_t called_as) {
		subtasks_list[subtask].called_as=called_as;
	}
	Service to(const Subtask subtask) {
		return subtasks_list[subtask].to;
	}
	void to(const Subtask subtask, Service to) {
		subtasks_list[subtask].to=to;
	}
	Service return_to(const Subtask subtask) {
		return subtasks_list[subtask].return_to;
	}
	void return_to(const Subtask subtask, Service return_to) {
		subtasks_list[subtask].return_to=return_to;
	}

	Word ack_to(const Subtask subtask) {
		return subtasks_list[subtask].ack_to;
	}
	void ack_to(const Subtask subtask, Word ack_to) {
		subtasks_list[subtask].ack_to=ack_to;
	}

	uint redir(const Subtask subtask) {
		return subtasks_list[subtask].redir;
	}
	void redir(const Subtask subtask, uint redir) {
		subtasks_list[subtask].redir=redir;
	}

	uint waiting_for_ack(const Subtask subtask) {
		return subtasks_list[subtask].waiting_for_ack;
	}
	void waiting_for_ack(const Subtask subtask, uint waiting_for_ack) {
		subtasks_list[subtask].waiting_for_ack=waiting_for_ack;
	}

	bool waiting(const Subtask subtask) {
		return subtasks_list[subtask].waiting;
	}
	void waiting(const Subtask subtask, bool waiting) {
		subtasks_list[subtask].waiting=waiting;
	}


	CodeAddress code_address(const Subtask subtask) {
		return subtasks_list[subtask].code_address;
	}
	void code_address(const Subtask subtask, CodeAddress code_address) {
		subtasks_list[subtask].code_address=code_address;
	}

	uint service_id(const Subtask subtask) {
		return subtasks_list[subtask].service_id;
	}
	void service_id(const Subtask subtask, uint service_id) {
		subtasks_list[subtask].service_id=service_id;
	}

	 uint nargs(const Subtask subtask) {
            return subtasks_list[subtask].nargs;
	 }

     void nargs(const Subtask subtask, uint val) {
         subtasks_list[subtask].nargs=val;
#ifdef STATIC_ALLOC
         subtasks_list[subtask].arguments.size(val);
#endif // STATIC_ALLOC
    }

	 uint nargs_absent(const Subtask subtask) {
            return subtasks_list[subtask].nargs_absent;
	 }

     void nargs_absent(const Subtask subtask, uint val) {
         subtasks_list[subtask].nargs_absent=val;
    }

     void decr_nargs_absent(const Subtask subtask) {
         subtasks_list[subtask].nargs_absent--;
//         return subtasks_list[subtask].nargs_absent;
    }

     void incr_nargs_absent(const Subtask subtask) {
         subtasks_list[subtask].nargs_absent++;
//         return subtasks_list[subtask].nargs_absent;
    }

    uint mode(const Subtask subtask) {
            return subtasks_list[subtask].mode;
    }
        void mode(const Subtask subtask, uint val) {
            subtasks_list[subtask].mode=val;
        }

    uint reg(const Subtask subtask) {
            return subtasks_list[subtask].reg;
    }
        void reg(const Subtask subtask, uint val) {
            subtasks_list[subtask].reg=val;
        }

	uint offset(const Subtask subtask) {
		return subtasks_list[subtask].offset;
	}
	void offset(const Subtask subtask, uint val) {
		subtasks_list[subtask].offset=val;
	}
	uint fsize(const Subtask subtask) {
		return subtasks_list[subtask].fsize;
	}
	void fsize(const Subtask subtask, uint val) {
		subtasks_list[subtask].fsize=val;
	}
	MemAddress result_address(const Subtask subtask) {
		return subtasks_list[subtask].result_address;
	}
	void result_address(const Subtask subtask, MemAddress val) {
		subtasks_list[subtask].result_address=val;
	}


	Word result(const Subtask subtask) {
		//cout << "Ashkan says subtasks_list[subtask].result: " << subtasks_list[subtask].result <<endl;
		return subtasks_list[subtask].result;
	}
	void result(const Subtask subtask, Word val) {
		subtasks_list[subtask].result=val;
	}
	bool migrated(const Subtask subtask) {
		return subtasks_list[subtask].migrated;
	}

   Subtasks subtasks() {
       Subtasks subtasks;
#ifndef STATIC_ALLOC
	for(SubtaskMap::iterator iter=subtasks_list.begin();iter!=subtasks_list.end();iter++) {
           subtasks.push_back(iter->first);
       }
#else
	for (uint i=0;i<CODE_SZ;i++) {
		if (occ[i]) {
			subtasks.push_back(i);
		}
	}
#endif
       return subtasks;
   }

   Subtask_List_Item entry(const Subtask subtask) {
       return subtasks_list[subtask];
   }
/*
   string inspect() {
       return string("FIXME: NOT IMPLEMENTED");
       }
*/
   }; // of Subtask_List

}
#endif /*SERVICE_MGR_OBJECTS_H_*/

