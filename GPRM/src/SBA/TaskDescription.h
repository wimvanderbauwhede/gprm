
#ifndef TASKDESCRIPTION_H_
#define TASKDESCRIPTION_H_

//
// :title: Garnet Service-based SoC project - SBA TaskDescription class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: TaskDescription.rb 2155 2009-01-28 11:39:41Z socgroup $


#include "ServiceConfiguration.h" 
#include "Packet.h" 
#include <vector>
#include <stdio.h> 

using namespace std;

namespace SBA {
class TaskDescription {
	public:

	Packet_List Packets;

//  TaskDescription(string,uint);
  TaskDescription(Bytecode&);

private:
// void byc2td(string);
 void wl2td(Bytecode&);
// Header_t byc2header(FILE*);
// Word_List byc2playload(Uint16,FILE*);
    
}; // TaskDescription
} // namespace SBA

#endif // TASKDESCRIPTION_H_
