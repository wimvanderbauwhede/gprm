
#ifndef INTERFACE_H_
#define INTERFACE_H_

// Interface.rb
//
// :title: Gannet Service-based SoC project - Interface class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: Interface.rb 2253 2009-02-17 17:35:14Z socgroup $

#include "Base/System.h"
#include "TaskDescription.h"
#include "Types.h"
#include "ServiceConfiguration.h"

using namespace std;

namespace SBA {
class Interface  {
	public:
		Base::System* sba_system_ptr;
		Base::Tile* sba_gwtile_ptr;
		uint iodescs[MAX_NTASKS];

		BytecodeQueue tdcs;
		Interface(Base::System* sba_s_, Base::Tile* sba_t_)
		:  sba_system_ptr(sba_s_), sba_gwtile_ptr(sba_t_)
		  {
		  };

		static Bytecode read_bytecode(string);
//		Bytecode read_bytecode(uint status);
//		uint receive(uint);
//		void send(Word_List&,uint);
}; // Interface
} // namespace SBA

#endif // INTERFACE_H_
