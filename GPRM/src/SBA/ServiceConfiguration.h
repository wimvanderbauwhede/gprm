// ServiceConfiguration.rb
//
// Gannet Service-based SoC project - Service Configuration module
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: ServiceConfiguration.rb 2534 2009-04-23 11:21:04Z socgroup $


#ifndef SERVICE_MGR_CONF_H_
#define SERVICE_MGR_CONF_H_

#include "Base/Types.h" //
#include "./Base/Types.h" // WHY?

using namespace SBA;

namespace SBA {

enum Symbol_Kind { // 4 bits => we could have 16 of them. Or 3 bits is enough
        K_Unknown=16, // 0000 # only used by Compiler
        K_S=0, // 0000 # only used by Compiler
        K_D=1, // 0001 # 'Data', resurrected for CACHE/BUFFER/ACC. I think it is obsolete
        K_A=2, // 0110 # 'Argument'
        K_L=3, // 0011 # 'Lexical', this was used to request lexical variables but maybe K_D could take this function
        K_R=4, // 0100 # 'Reference', the most important Kind
        K_C=5, // 0101 # 'Code'
        K_B=6, // 0110 # 'Built-in', for constants
        K_P=7, // 0111 # New kind for pointers
        K_F=8  // 1000 # New kind for functions, i.e. lambdas. I should rename K_L to K_V and K_F to K_L
//        K_Q=7 // 0111 # used as spill-over for K_B|T_s and K_B|T_b in 64-bit
        // K_E=8, # 1000 # not used
        // K_SL=, # 1011
        // K_SD=, # 1001
        // K_SU=, # 1010
    };

// The actual values are not that important, bit we could say
// 000 Any
// 001 Int
// 010 Float
// 011 String? Or Char?
// 1xx: a list of the above. So 111 would be a string, a list of chars
//  T_d | T_i | T_f | T_c | T_L | T_I | T_F | T_s | T_q | T_x | T_Error
enum Symbol_Type { // 3 bits. Actually 1 bit is enough if we get rid of that stupid T_u in APPLY
        T_d=0, // 000 # Data, i.e. Any
        T_i=1, // 001 # signed int => LSB=1
        T_f=2, // 010 # IEEE float/double => LSB=0
        T_c=3, // 011 # char --> we don't have this
        T_L=4, // 100 # List (of Data) --> not needed, that is just a pointer
        T_I=5, // 101 # Word_List --> not needed, that is just a pointer
        T_F=6, // 110 # List of Float --> not needed, that is just a pointer
        T_s=7, // 111 # String (List of chars)
    };


//  T_x | T_d | T_u | T_i | T_f | T_s | T_l | T_n | T_b | T_q
//  If it's a number it's either Float or Int in 2's complement. So 1 bit can indicate this.
//  If it's not a number then can use the bit for something else
//    Symbol_Types={ # 3 bits. Actually 1 bit is enough if we get rid of that stupid T_u in APPLY
//        'i'=>0, # 011 # signed int => LSB=1
//        'f'=>1, # 100 # IEEE float => LSB=0
//        's'=>2, # 010 # string
//        'b'=>3, # 011 # bool -- OBSOLETE, use i
//        'd'=>4, # 100 # Data, i.e. Any
//    }

// For compatibility with streaming data, we either add a Mode field (stream or 'datagram') or we use a bit in the Type field
// We use the MSB to indicate the mode.
enum Packet_Type { // >7, so needs at least 4 bits. We have a 3 bits ... but things like MM can be done using a reference!
// and for non-reconfigurable systems, no need for the last three.
        P_error=0, // Payload could be error message. In general not Symbols
        P_subtask=1, // list of Symbols. At least 2 elts: (S ...); Can be stream
        P_code=2, // list of Symbols; Can be stream
        P_reference=3, // 1-elt list of Symbols; Can be stream
        P_request=4, // 1-elt list of Symbols; Can be stream
        P_data=5, // preferred; Can be stream
        P_mm=6, // list of Symbols. Usually 1-elt
        P_fragment=7,
        P_lookup=8, // "what is the address of service n?", with n a number => 1-elt list of uint64
        P_address=9, //
        P_advertise=10, // "my address is n", with n a number => 1-elt list of uint64
        P_migrate=11, // subtask migration to another Service Node
        P_clean_up=12 //Clean_up for subtask migration
    };

enum Core_Status {
    CS_idle=0,
    CS_ready=1,
    CS_busy=2,
    CS_done=3,
    CS_managed=4,
    CS_done_eos=5,
    CS_skip=6,
    CS_eos=7
    };

enum Subtask_Status {
   STS_new=0, // 000 activated
   STS_pending=1, // 001 means being parsed or waiting for execution I guess
   STS_processing=2, // 010 being executed
   STS_processed=3, // 011 execution finished
   STS_cleanup=4, // 100 should be 'cleaned up'?
   STS_blocked=5, // 101
   STS_inactive=6, // 110 I'll use this for recursion where we reclaim addresses but don't remove the subtask
   STS_deleted=7, // 111 purely for HW mem management: indicates that the subtask at a particular address is deleted
   STS_skip=8,
   STS_eos=9,
   STS_migrated=10, //Ashkan: Seems to be not very useful!
   STS_stolen=11 // Do NOT steal it. it is for myself
    };

// If error=4 and cleared=3, we need 2 bits for the actual Data_Status, another bit for error.
// As memory is at least byte-addressable and most likely 32-bit, we have plenty of bits left for
// other information regarding the storage. So we use a bitmask an have e.g. 2 bits for ACK support
// and 2 bits for streaming/fifo support
enum Data_Status {
   DS_absent=0, // 00
   DS_present=1, // 01
   DS_requested=2,  // 10
   DS_eos=3, // 11 EOS is a special case of present
   DS_cleared=4, // 100 does not fit into 2 bits! So cleared becomes absent, should be OK
   DS_error=5 // 101 does not fit into 2 bits!
    };

enum Code_Status {
    CS_Absent=0,
    CS_ActReq=1,
    CS_Present=2,
    CS_Reset=3 // Activation Request && Present => Present
    };

enum RegData_Status {
    RDS_absent=0,
    RDS_present=1
//    RDS_requested=2
//    RDS_eos=3
    };

enum Mode {
    M_normal=0,
    M_cache=1,
    M_var=1,
    M_acc=1,
    M_stream=2,
    M_buf=2,
    M_eos=3
    };

// Note that constants must match /^[A-Z][A-Z][A-Z0-9_]*/
// NONE=0
#define NA 0

#if DISTR==1
#define GWPORT 7188
#endif // // DISTR

// GANNET_LANGUAGE=1
#define STATE_REG_SZ 8
#define SBA_USE_ADDRESS_STACKS 1
#define SBA_BRIDGE_ADDR 32
#define SBA_BRIDGE_HW_FIFO_SZ 64
// WV17082008: the TX fifo in the Gateway must store all packets for a task, can be large
#define PACKET_FIFO_SZ 64
//#define PACKET_FIFO_SZ 32768 //Ashkan changed it
#define MAX_REGISTERFILE_SZ 1024
//ifdef STATIC_ALLOC

#if DISTR==0
#define MAX_NSERVICES  263 //64
//#define MAX_NSERVICES  1024 // Ashkan changed it
#define MAX_NTASKS 4 // determines the number of tasks, so related to F_Task etc
#else // // DISTR
#define MAX_NSERVICES   1
#define MAX_NTASKS 1 // determines the number of tasks, so related to F_Task etc
#endif // // DISTR
#define MAX_NDYNCONFIGS   64
//#define MAX_NARGS 16 // determines size of arg address fifo
#define MAX_NARGS 255 // Ashkan changed it //8bits
#define MAX_BYC_SZ 1024 // max size of program bytecode
// #define MAX_NPENDING_TASKS 8 // tasks waiting to be executed
#define MAX_NPENDING_TASKS 256 //Ashkan Changed it
//endif // STATIC_ALLOC
#define MAX_STATIC_SUBTASKS 32768
#define MAX_DYNAMIC_SUBTASKS 32768

#define NARGS_SZ 8 //Ashkan: not used I guess!
#define REGS_SZ 3

#define MAX_CODE_PACKET_SZ 32 // in fact, the HW assumes the code itself to be 8 Words. This is too small for most lambdas, I fear.
#define MAX_DATA_PACKET_SZ 16 // currently, the HW has 8 Words per chunk, so the max packet size is 11; for DCT we'd need 64+3
#define HEADER_SZ 3
#define REF_PACKET_SZ 4 // 3 words header + 1 ref symbol
#define FIXED_PACKET_SZ 4 // for Quarc with S&F
#define MAX_PACKET_SZ 514 // waste space for convenience. Length field is 9 bits so max payload is 511; header=3
#define MAX_LIST_SZ 256 // waste space for convenience
//#define MAX_LIST_SZ 4096 //Ashkan changed it
#define NREGS 8 // Actually, we could have a few more I think
//#define NREGS 1024 // Ashkan changed it
#define NREQS 8
//#define NREQS 1024 //Ashkan changed it
#if SBA_USE_ADDRESS_STACKS==1
#define SUBTASKS_SZ 16284//ASHKAN CHANGED IT from 256 to 16284 (still have some space)
#define SUBTASKS_OF 0 //WV14112008: was 1 in r1884, later on changed to 0 to be compatible with C++. Set to 1 now for debugging recursive lambdas
#define BUILTINS_SZ 384 // 128 per type
#define BUILTINS_OF 3 // for DEBUG! should be 0!
//#define DATA_SZ 1023 // i.e. 10 bits
#define DATA_SZ 16384// ASHKAN CHANGED IT (This solved the Fibonacci problem of more than 9 levels)
#define DATA_OF 1 // reserve 0
#define SERVICE_CORE_RAM_SZ 64
//    LEVELS_SZ=256
//    LEVELS_OF=1024
//    IR_SUBTASKS_SZ=256
//    IR_SUBTASKS_OF=0
//    the Code stack starts from 1, not 0, so unless we subtract -1, we need 1 address extra, so 257
//    That's ugly so I take 512
//    Just as well as code addresses are constructed with the Task so they go up to ???
#define CODE_SZ 256
#define REC_STACK_SZ 16
#endif // // SBA_USE_ADDRESS_STACKS
//GWT_RES_SZ=16
//GWT_DATA_SZ=16

#define NCORE_THREADS 4
#define MULTI_THREADED_CORE 0

#define MAX_NACCESSORS   8
#define RDS_MASK 0xFFUL

#define NBYTES 8

// 64 bits=8 BYTES. 1 BYTEULL
// Kind:Typ:E | Qu:Task   | Subtask       Padding | NSymbols
// 0110:000:1 | 01:000000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000
// 6   :1     | 4   :0    | 0   :0    | 0   :0    | 0   :0    | 0   :0    | 0   :0    | 0   :1
// K_B :t_D:Ext:Q
#define EXTSYM 0x6140000000000001ULL
// 0111|000:1|01:00 0000|
#define PTRSYM 0x7140000000000000ULL
// 0110:000:0 | 01:000000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000 | 0000:0000
#define ZERO 0x6240000000000000ULL
#define ONE 0x6240000000000001ULL
#define FALSE 0x6240000000000000ULL
#define TRUE 0x6240000000000001ULL

// NIHIL is an extended symbol of length 0
#define NIHIL 0x6140000000000000ULL
const Word F_AllOnes=0xFFFFFFFFFFFFFFFFULL;
// All ones
const Word F_Symbol=0xFFFFFFFFFFFFFFFFULL;

// 8 bits (HW uses 10 bits address space. 2 bits paging, 3 bits for chunk size, 5 bits for number of chunks
//FIXME: shouldn't I extend this for 64 bits, no need to keep it so small?
// We have 6 bits for Task, 16 for Subtask. Check subdivision of Subtask for Code use ...
//Instead of the 5 bits for HW, let's go for 10 bits
// TODO: 0x0000_03ff_0000_0000
const Word F_CodeAddress=0x0000001F00000000ULL;
// 8 bits (HW needs 10?)
// TODO: 0xffff_fc00_ffff_ffff
const Word FN_CodeAddress=0xFFFFFFE0FFFFFFFFULL;
// identical to FS_Subtask
const Word FS_CodeAddress = 32 ;
const Word FB_CodeAddress = 5 ;// TODO: 10;
const Word FW_CodeAddress=0x1F ;// TODO 0x3FFULL;
// FW_CodeAddress=31 # i.e. max. number of subtask code segments is 31! TODO: 1023

// identical to F_Task, so 6 bits
const Word F_CodePage=0x003F000000000000ULL;
const Word FW_CodePage=0x3FULL;
const Word FB_CodePage=6;

// identical to FN_Task
const Word FN_CodePage=0xFCFFFFFFFFFFFFFFULL;
// The code address is
//    page=(word & F_CodePage)>> FS_Task #t Word
//    address=(word & F_CodeAddress) >> FS_Subtask #t Word
//    service=(word & F_Service)>>FS_Name #t Word
//    code_address=(service << FS_Service)+(page << FS_CodePage)+address #t Word
const Word FS_CodePage = FB_CodeAddress ;
// 10 bits, i.e. FB_CodeAddress. With this shift, we need (FS_Task-FS_CodePage) to compress the address

const Word F_Service=0x00000000000000FFULL;
const Word FN_Service=0xFFFFFFFFFFFFF00ULL;
const Word FS_Service=FS_CodePage+FB_CodePage;

const Word F_Name=0x00000000FFFFFFFFULL;
const Word FN_Name=0xFFFFFFFF00000000ULL;
const Word FS_Name = 0;
const Word FW_Name=0xFFFFFFFFULL;
const Word FB_Name=32;


//IMPORTANT: Ashkan Changed it, because there were no space for numbers > 63. it is used by the getTask function. R:0:0:0:"63":...
// Ashkan: I have given 2 bits from Subtask to task
/*const Word F_Subtask=0x00003FFF00000000ULL;
const Word FN_Subtask=0xFFFFC000FFFFFFFFULL;
const Word FS_Subtask = 32;
const Word FW_Subtask=0x3FFFULL;
const Word FB_Subtask=14;

const Word F_Task=0x003FC00000000000ULL;
const Word FN_Task=0xFFC03FFFFFFFFFFFULL;
const Word FS_Task = 46;
const Word FW_Task=0x3FCULL;
const Word FB_Task=8;*/


/*
 * Ashkan
 */
/*const Word F_Subtask=0x00001FFF00000000ULL;
const Word FN_Subtask=0xFFFFE000FFFFFFFFULL;
const Word FS_Subtask = 32;
const Word FW_Subtask=0x1FFFULL;
const Word FB_Subtask=13;

const Word F_Task=0x003FE00000000000ULL;
const Word FN_Task=0xFFC01FFFFFFFFFFFULL;
const Word FS_Task = 45;
const Word FW_Task=0x3FEULL;
const Word FB_Task=9;*/

const Word F_Subtask=0x0000FFFF00000000ULL;
const Word FN_Subtask=0xFFFF0000FFFFFFFFULL;
const Word FS_Subtask = 32;
const Word FW_Subtask=0xFFFFULL;
const Word FB_Subtask=16;

const Word F_Task=0x003F000000000000ULL;
const Word FN_Task=0xFFC0FFFFFFFFFFFFULL;
const Word FS_Task = 48;
const Word FW_Task=0x3FULL;
const Word FB_Task=6;

const Word F_Quoted=0x00C0000000000000ULL;
const Word FN_Quoted=0xFF3FFFFFFFFFFFFFULL;
const Word FS_Quoted = 54;
const Word FW_Quoted=0x3ULL;
const Word FB_Quoted=2;

// 0000 0001 0000
const Word F_Ext=0x0100000000000000ULL;
// 1111 1110 1111
const Word FN_Ext=0xFEFFFFFFFFFFFFFFULL;
const Word FS_Ext = 56;
const Word FW_Ext=0x1ULL;
const Word FB_Ext=1;

// 00001110 0000
const Word F_Datatype=0x0E00000000000000ULL;
// 11110001 1111
const Word FN_Datatype=0xF1FFFFFFFFFFFFFFULL;
const Word FS_Datatype = 57;
const Word FW_Datatype=0x7ULL;
const Word FB_Datatype=3;

const Word F_Kind=0xF000000000000000ULL;
// 00001111 1111
const Word FN_Kind=0x0FFFFFFFFFFFFFFFULL;
const Word FS_Kind = 60;
const Word FW_Kind=0xFULL;
const Word FB_Kind=4;

// Count field is long obsolete!!!
const Word F_Count=0x0000000000000000 ;// NO COUNT FIELD!ULL;
const Word FN_Count=0xFFFFFFFFFFFFFFFFULL;
const Word FS_Count = 0;
const Word FW_Count=0x0ULL;
const Word FB_Count=0;

// We store a 32-bit number in the 4 LSBs, i.e. the Name field
const Word F_Value=0x00000000FFFFFFFFULL;
const Word FN_Value=0xFFFFFFFF00000000ULL;
const Word FS_Value=0;
const Word FW_Value=0xFFFFFFFFULL;
const Word FB_Value=32;

// We store a 54-bit number in the LSBs for extended symbols
const Word F_ExtValue=0x003FFFFFFFFFFFFFULL;
const Word FN_ExtValue=0xFF40000000000000ULL;
const Word FS_ExtValue=0;
const Word FW_ExtValue=0x3FFFFFFFFFFFFFULL;
const Word FB_ExtValue=54;

// Identical to the Name field
const Word F_NSymbols=0x00000000FFFFFFFFULL;
const Word FN_NSymbols=0xFFFFFFFF00000000ULL;
const Word FS_NSymbols=0;

// First 4 bits of the Subtask field
const Word F_NPadBytes=0x0000000F00000000ULL;
const Word F_NNPadBytes=0xFFFFFFF0FFFFFFFFULL;
const Word FS_NPadBytes=32;
const Word FW_NPadBytes=0xFULL;
const Word FB_NPadBytes=4;

// For Packet header
// Header Word1: 8 | 5|3 | 16 | 16 | 16
const Word F_Packet_type=0xFF00000000000000ULL;
const Word F_Ctrl=0x00F8000000000000ULL;
const Word F_Redir=0x0007000000000000ULL;
const Word F_Length=0x0000FFFF00000000ULL;
const Word F_To=0x00000000FFFF0000ULL;
const Word F_Return_to=0x00000000000FFFFULL;
const Word F_Send_to[4]={0x000000000000FFFFULL,0x00000000FFFF0000ULL,0x0000FFFF00000000ULL,0xFFFF000000000000ULL};

// Reverse masks
const Word FN_Packet_type=0x00FFFFFFFFFFFFFFULL;
const Word FN_Ctrl=0xFF07FFFFFFFFFFFFULL;
const Word FN_Redir=0xFFF8FFFFFFFFFFFFULL;
const Word FN_Length=0xFFFF0000FFFFFFFFULL;
const Word FN_To=0xFFFFFFFF0000FFFFULL;
const Word FN_Return_to=0xFFFFFFFFFFFF0000ULL;
const Word FN_Send_to[4]={0xFFFFFFFFFFFF0000ULL,0xFFFFFFFF0000FFFFULL,0xFFFF0000FFFFFFFFULL,0x0000FFFFFFFFFFFFULL};

// Field width masks
const Word FW_Packet_type=0xFFULL;
const Word FW_Ctrl=0xF8ULL;
const Word FW_Redir=0x07ULL;
const Word FW_Length=0xFFFFULL;
const Word FW_To=0xFFFFULL;
const Word FW_Return_to=0xFFFFULL;

// Field width as number of bits
const Word FB_Packet_type=8;
const Word FB_Ctrl=5;
const Word FB_Redir=3;
const Word FB_Length=16;
const Word FB_To=16;
const Word FB_Return_to=16;

// Shifts
const Word FS_Packet_type=56;
const Word FS_Ctrl=51;
const Word FS_Redir=48;
const Word FS_Length=32;
const Word FS_To=16;
const Word FS_Return_to=0;
const Word FS_Send_to[4]={0ULL,16ULL,32ULL,48ULL};

// For ACC/BUFFER/CACHE
const Word F_NArgs=0xFF ;// 8 BITS;
const Word FW_NArgs = F_NArgs;

const Word FW_Reg=0xF ;// 4 BITS;
const Word FB_Reg=4;
const Word FS_Reg=10;
const Word FW_Mode=0x3 ;// 2 BITS;
const Word FS_Mode=14;

// Looks like we have ?? bits left in the K_D symbol
// Token: 5 bits, next to Name field (TODO: make larger for 64 bits)
const Word FW_Token=0x1FULL;
const Word F_Token=0x1F0000ULL;
const Word FS_Token=16;
// NCons: 3 bits, next to Token (TODO: make larger for 64 bits)

const Word F_NCons=0xE00000ULL;
const Word FW_NCons=0x7ULL;
const Word FS_NCons=19;

const Word FW_DataAddress=0x03FF ;// 10 BITS (TODO: MAKE LARGER FOR 64 BITS)ULL;

// We have Subtask == Mode|1Bit|Reg|DataAddress (2|[1]|3|[2]|8) == Mode|1Bit|Reg|2Bits|NArgs

// K_S needs Mode (buf=2|var=1|normal=0), K_D needs mode (stream=2,var=1)
// K_S needs Reg, so does K_D
// K_S needs NArgs
// K_S is never quoted, K_D can be quoted
// K_S needs SCId|Opcode, K_D needs Name
// K_S maybe needs SId as well, later on

// So in conclusion:
// 4  :3         :1    :2       :6       :16 2|4                         :32 (8|8|8|8)
// K_S:(Datatype):(Ext):(Quoted):Task    :Mode|Reg|2Bits|NArgs           :SCLId|SCId|Opcode
// K_D:(Datatype):(Ext=0):Quoted  :Task    :Mode|Reg|10Bits                :Value
// K_R:(Datatype):(Ext):Quoted  :CodePage:6Bits|5Bits|CodeAddress        :Name
// K_C:(Datatype):(Ext):Quoted  :Task    :Mode|1Bit|Reg|5Bits|CodeAddress:Name
// K_B:Datatype  :0    :Quoted  :Task    :16Bits                         :Value
// K_B:Datatype  :1    :Quoted  :Task    :16Bits                         :NSymbols
// K_B:Datatype  :1    :Quoted  :Value (54 bits)
// K_L:(Datatype):(Ext):Quoted  :Task    :DataAddress                    :Name
// K_A:(Datatype):(Ext):Quoted  :Task    :*                              :Name

// As we need only 5 bits for Code Address in HW we can leave Reg and Mode
// as part of the Subtask field
// For the VM it might be more tricky... I think we'll have to review using Datatype to indicate
// that a task is to be stored/looked up at APPLY. There's no way we can squeeze Reg, Mode, Name and CodeAddress in 16 bits
//
//# We want to store the register address in Datatype+Ext+Quoted, so 3 bits
//# So we need another FS_Reg. I think we have FS_Reg_D and FS_Reg_C
//F_Reg_C=0x1C000000ULL
//FS_Reg_C = FS_Quoted
//FB_Reg_C = 3
//FW_Reg_C= 7

// For multi-threaded cores, we need F_SCLId, F_SCId and F_Opcode

const Word F_Opcode=0xFFULL;
const Word FW_Opcode=0xFFULL;
const Word FS_Opcode=0;
const Word FB_Opcode=8;

const Word F_SCId=0xFF00ULL;
const Word FW_SCId=0xFFULL;
const Word FS_SCId=8;
const Word FB_SCId=8;
// 256 libraries is a lot. I guess 64 should be enough
/*const Word F_SCLId=0x00FF0000ULL;
const Word FN_SCLId=0xFF00FFFFULL;
const Word FW_SCLId=0xFFULL;
const Word FS_SCLId = 16;
const Word FB_SCLId=8;

// This is the Node ID, which of course we need for routing.
// let's say 256 nodes is enough; we could go to 1024 by taking 2 bits from SCLId
const Word F_SNId=0xFF000000ULL;
const Word FN_SNId=0x00FFFFFFULL;
const Word FW_SNId=0xFFULL;
const Word FS_SNId = 24;
const Word FB_SNId = 8;*/

// 256 libraries is a lot. I guess 64 should be enough
 const Word F_SCLId=0x000F0000ULL;
 const Word FN_SCLId=0xFFF0FFFFULL;
 const Word FW_SCLId=0xFULL;
 const Word FS_SCLId = 16;
 const Word FB_SCLId=4;
//
// // This is the Node ID, which of course we need for routing.
// // let's say 256 nodes is enough; we could go to 1024 by taking 2 bits from SCLId. Ashkan: Let's Do iT!
 const Word F_SNId=0xFFF00000ULL;
 const Word FN_SNId=0x000FFFFFULL;
 const Word FW_SNId=0xFFFULL;
 const Word FS_SNId = 20;
 const Word FB_SNId = 12;

// tuple support
const Word FW_Offset=0xFFFFULL;
const Word FS_Offset = 0;
const Word FW_Size=0xFFFFULL;
const Word FS_Size = 16;


// Last opcode is used for "wait". Currently UNUSED
const Word M_wait = (1<<FB_Opcode)-1;

typedef Data_Status Argument_Status;
/// The core must return a Gannet data type
typedef Symbol_Type Core_Type;
typedef Symbol_Type Data_Type;


}
#endif /*SERVICE_MGR_CONF_H_*/

