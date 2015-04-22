
#ifndef BASE_SERVICECORECONTROL_H_
#define BASE_SERVICECORECONTROL_H_

//
// Gannet Service-based SoC project - SBA Service Core class
//
// (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#include "System.h"
#include "Tile.h"
//#include "Base/System.h"
//#include "Base/Tile.h"
//#include "Base/ServiceCore.h"
#include "ServiceManager.h"
#include "ServiceConfiguration.h"
#include "LookupTable.h"
#include "ServiceManagerObjects.h"

using namespace std;
using namespace SBA;

namespace SBA {
class System;
namespace Base {
class ServiceCoreControl {
	private:
	Subtask _current_subtask;
	State_Register _state_register;
	public:

	Base::System* sba_system_ptr;
	Base::Tile* sba_tile_ptr;
//	State_Register state_register;
	LookupTable                     lookup_table;
	Core_Status                     core_status;
	Packet_Type core_return_type;
	uint ack_ok;
	uint n_args ;
	Service service;
	Service nservice;
	ServiceAddress address;

	uint tid;
	uint scid;
	uint sclid;
	uint opcode;
	bool verbose ;
        ServiceCoreControl() {};
        ServiceCoreControl(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_, ServiceAddress& addr_, uint tid_) :
        	_current_subtask(0),
            sba_system_ptr(sba_s_),
            	sba_tile_ptr(sba_t_),
            	core_status(CS_idle),
                service(s_), address(addr_),
                tid(tid_),
                scid(0)
			{

			};

 void run();

 virtual void select_wrapper(unsigned int) =0;
 void suspend();

 Word iterate();
 Word iterate(Word v);

 bool init_state(unsigned int service_id);
 void store_state(unsigned int service_id,void*);
 void* load_state(unsigned int service_id);
#ifdef KERNEL_HAS_STATE
#ifdef KERNEL_LOCK
 pthread_mutex_t get_mutex(unsigned int service_id);
#endif
#endif

 // Cast a Word to some type, this should be a template function, taking a Word!
 Word_List* unpack(Word);
 // cast a pointer to Word, i.e. a uint64_t
 Word pack(Word_List*);

 Subtask_Argument_List& args();
// MemAddresses& addresses();
 uint argmode(uint);
 //void* arg(uint);
 Word arg(uint);
 Word core_arg(uint);
 Word symbol(uint);
 Word arg(Subtask,uint);
 Word symbol(Subtask,uint);
 uint nargs();
 uint nargs(Subtask);
 MemAddress addr(uint);
 void result(Word_List);
 void result(Word);
 void resultWord(Word);

// void put(MemAddress,Word_List);
// void put(MemAddress,Word);
// Word get(MemAddress);

 uint method();
 bool isQuoted(uint);
 bool isRef(uint);
 bool is_quoted_ref(uint);
 bool waiting();
 void dispatch_reference(uint);
 void dispatch_reference(uint,uint);
 void dispatch_reference(Symbol_t,uint,uint);
 //B Ashkan
  void dispatch_reference_temp(uint);
  void dispatch_reference_temp(uint,uint);
  void dispatch_reference_temp(Symbol_t,uint,uint);
 //E Ashkan
 void repost_subtask(Subtask);
 void block_subtask(Subtask);
 void reset_subtask(Subtask);

 void block_current_subtask();
 void reset_current_subtask();
 SBA::ServiceManager& service_manager();
 SBA::System& system();
// SBA::GatewayTile& gatewaytile();
 Subtask current_subtask();
 Word_List& get_code(Word);
 void put_code(Word, Word_List& );
 uint get_snid();

}; // Base::ServiceCoreControl
}} // namespace SBA

#endif // BASE_SERVICECORECONTROL_H_

