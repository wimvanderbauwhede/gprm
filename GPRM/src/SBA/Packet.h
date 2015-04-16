
#ifndef PACKET_H_
#define PACKET_H_

// SBA_Packet
//
// :title: Garnet Service-based SoC project - SBA Packet class
//
// (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#ifdef VERBOSE
#include <string>
#include <sstream>
#endif
#include "Types.h"
#include "ServiceConfiguration.h"


using namespace std;
using namespace SBA;

namespace SBA {



//==============================================================================
//
// General Types for Gannet -- included from C++/SBA/Packet_Types.h
//
//==============================================================================


typedef uint8 Kind_t; //3
typedef uint8 Datatype_t; //1
typedef uint8 Ext_t; //1
typedef uint8 Quoted_t; //1
typedef uint8 Task_t; //2
typedef uint16 Subtask_t; //16
typedef uint8 Mode_t; //2

typedef uint8 Packet_type_t; //3
typedef uint8 Ctrl_t; //2
typedef uint8 Redir_t; //3

typedef Word ExtValue_t; //54
typedef uint32 Name_t; //32
typedef uint32 Value_t; //32
typedef uint16 NSymbols_t; //16
typedef uint16 Count_t; //16

typedef uint16 Length_t; //16
typedef uint16 Return_to_t; //16
typedef uint16 To_t; //16

typedef Task_t DS_t; //2
typedef Word Symbol_t;
typedef Word Bool_t; // bool encoded as K_B Word


Word mkSymbol(Word,Word,Word,Word,Word,Word,Word);
Kind_t getKind(Word);
Word setKind(Word,Word);
Datatype_t getDatatype(Word);
Word setDatatype(Word,Word);
Ext_t getExt(Word);
Word setExt(Word,Word);
Quoted_t getQuoted(Word);
Word setQuoted(Word,Word);
Word unQuote(Word);
Word quote(Word);
Task_t getTask(Word);
Word setTask(Word,Word);
Task_t getArgPos(Word);
Word setArgPos(Word,Word);

Subtask_t getSubtask(Word);
Word setSubtask(Word,Word);

MemAddress getDataAddress(Word);

Name_t getName(Word);
Word setName(Word,Word);
DS_t getStatus(Word);
Word setStatus(Word,DS_t);

Value_t getValue(Word);
Word setValue(Word,Word);
ExtValue_t getExtValue(Word);
void* getPointer(Word);
Word setExtValue(Word,Word);

NSymbols_t getNSymbols(Word);
Word setNSymbols(Word,Word);
uint getNPadBytes(Word);

//Int getInt(Word_List);
Int getInt(Word);
Int getInt(Word_List&);

Word getUInt(Word);
Word getUInt(Word_List);

Word getWord(Word_List);
double getFloat(Word);
double getFloat(Word_List);

char getChar(Word_List);

string getString(Word_List);


uint getOpcode(Word);
Word setOpcode(Word w, uint opcode);
uint getSCId(Word);

uint getSCLId(Word);
Word setSCLId(Word,uint);

uint getSNId(Word);
Word setSNId(Word,uint);
Word mkSubtaskCodeAddressTup(Subtask,MemAddress);
Subtask getSubtaskFromTup(Word);
MemAddress getCodeAddressFromTup(Word);
uint getSNIdFromTup(Word);

CodeAddress getCodeAddress(Word);

Word setCodeAddress(Word,Word,Word);

uint getNArgs(Word);

uint getToken(Word);

uint getNCons(Word);


uint getReg(Word);

uint getMode(Word);

uint getOffset(Word);

uint getSize(Word);

Header_t mkHeader(Word,Word,Word,Word,Word,Word,Word,Word);
Packet_type_t getPacket_type(const Header_t&);
Header_t setPacket_type(Header_t,Word);

Ctrl_t getCtrl(Header_t&);

Header_t setCtrl(Header_t&,Word);
Redir_t getRedir(Header_t&);
Header_t setRedir(Header_t&,Word);
Length_t getLength(const Header_t&);

Header_t setLength(Header_t&,Word);
To_t getTo(const Header_t&);
Header_t setTo(Header_t&,Word);
Return_to_t getReturn_to(const Header_t&);
Header_t setReturn_to(Header_t&,Word);

Word getSubtaskArgpos(const Header_t&);
Word getSubtaskArgpos_p(const Packet_t&);


 Word getAck_to(const Header_t&);
	Word getAck_to_p(const Packet_t& packet);
    Word getReturn_as_p(const Packet_t& packet) ;
	Return_to_t getReturn_to_p(const Packet_t& packet);
    Redir_t getRedir_p(const Packet_t& packet) ;
    Ctrl_t getCtrl_p(const Packet_t& packet) ;
    Packet_type_t getPacket_type_p(const Packet_t& packet);
    Length_t getLength_p(const Packet_t& packet);

 Word getReturn_as(const Header_t&);

 Header_t setReturn_as(Header_t&,Word);

 Header_t setAck_to(Header_t&,Word);
 Header_t setSubtaskArgpos(Header_t& header,Word field) ;
 Packet_type_t getType(const Header_t&);

 Header_t setType(Header_t&,Packet_type_t&);



 Packet_t mkPacket(Header_t&,Word_List&);
 Packet_t mkPacket_new(Header_t&,Word);

 Header_t getHeader(Packet_t&);

 Word_List setHeader(Packet_t&,Header_t&);

 Word_List getPayload(Word_List);
 Word getPayload_Word(Word_List);

 Word_List getField(Word_List,uint,uint);
Word mkBool(uint);

Symbol_t mkIntSymbol(Word val) ;
Symbol_t mkPointerSymbol(void*) ;

deque<Int> to_signed_int_list( Word_List);

 string ppSymbol(Word);

 string ppHeader(Header_t);

 string ppPayload(Word_List);

 string ppPacket(Word_List);
 } // Namespace SBA


#endif // PACKET_H_
