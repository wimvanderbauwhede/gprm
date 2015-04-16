//==============================================================================
//
// General Types for Gannet -- for use in genPacket.pl
//
//==============================================================================


#ifdef WV_SYSTEMC // but maybe even in SystemC we don't use these
typedef sc_uint<FB_Kind> Kind_t; 
typedef sc_uint<FB_Datatype> Datatype_t; 
typedef sc_uint<FB_Ext> Ext_t; 
typedef sc_uint<FB_Quoted> Quoted_t; 
typedef sc_uint<FB_Task> Task_t; 
typedef sc_uint<FB_Subtask> Subtask_t; 
typedef sc_uint<FB_Name> Name_t; 
typedef sc_uint<FB_Count> Count_t; 

typedef sc_uint<FB_Packet_type> Packet_type_t; 
typedef sc_uint<FB_Prio> Prio_t; 
typedef sc_uint<FB_Redir> Redir_t; 
typedef sc_uint<FB_Length> Length_t; 
typedef sc_uint<FB_To> To_t; 
typedef sc_uint<FB_Return_to> Return_to_t; 

#else
typedef uint8 Kind_t; //3
typedef uint8 Datatype_t; //1
typedef uint8 Ext_t; //1
typedef uint8 Quoted_t; //1
typedef uint8 Task_t; //2
typedef uint16 Subtask_t; //16

typedef uint8 Packet_type_t; //3
typedef uint8 Prio_t; //2
typedef uint8 Redir_t; //3

#if WORDS==64
typedef uint16 Name_t; //16
typedef uint16 Count_t; //16

typedef uint16 Length_t; //16
typedef uint16 Return_to_t; //16
typedef uint16 To_to_t; //16
#elif WORDSZ==32
typedef uint8 Name_t; //8
typedef uint8 Count_t; //0

typedef uint8 Length_t; //8
typedef uint8 Return_to_t; //8
typedef uint8 To_t; //8
#endif // WORDSZ
#endif // WV_SYSTEMC

typedef Task_t DS_t; //2
typedef Word Symbol_t;
typedef Word_List Packet_t;
typedef Word_List Header_t;
typedef Word_List Payload_t;
typedef List<Packet_t> Packet_List;
typedef Fifo<Packet_t> Packet_Fifo;

