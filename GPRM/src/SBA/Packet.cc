// SBA_Packet
//
// :title: Garnet Service-based SoC project - SBA Packet class
//
// (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#include <string>
#include <sstream>
#include "Packet.h"

using namespace std;
using namespace SBA;

Word SBA::mkSymbol(Word kind,Word datatype,Word ext,Word quoted,Word task,Word subtask,Word name) {
	Word wkind=(kind << FS_Kind) & F_Kind;
	Word wdatatype=(datatype << FS_Datatype) & F_Datatype;
	Word wext=(ext << FS_Ext) & F_Ext;
	Word wquoted=(quoted << FS_Quoted) & F_Quoted;
	Word wtask=(task << FS_Task) & F_Task;
	Word wsubtask=(subtask << FS_Subtask) & F_Subtask;
	Word wname=(name << FS_Name) & F_Name;
	Word word=wkind+wdatatype+wext+wquoted+wtask+wsubtask+wname;
	return word;
}

Kind_t SBA::getKind(Word word) {
	return (word & F_Kind) >> FS_Kind;
}

Word SBA::setKind(Word word,Word field) {
	return (word & FN_Kind) + (field << FS_Kind);
}

Datatype_t SBA::getDatatype(Word word) {
	return (word & F_Datatype) >> FS_Datatype;
}

Word SBA::setDatatype(Word word,Word field) {
	return (word & FN_Datatype) + (field << FS_Datatype);
}

Ext_t SBA::getExt(Word word) {
	return (word & F_Ext) >> FS_Ext;
}

Word SBA::setExt(Word word,Word field) {
	return (word & FN_Ext) + (field << FS_Ext);
}

Quoted_t SBA::getQuoted(Word word) {
	return (word & F_Quoted) >> FS_Quoted;
}

Word SBA::setQuoted(Word word,Word field) {
	return (word & FN_Quoted) + (field << FS_Quoted);
}

Word SBA::unQuote(Word word) {
	return (word & FN_Quoted) + ((Word)0 << FS_Quoted);

}

Word SBA::quote(Word word) {
	return (word & FN_Quoted) + ((Word)1 << FS_Quoted);
}

Task_t SBA::getTask(Word word) {
	return (word & F_Task) >> FS_Task;
}

Word SBA::setTask(Word word,Word field) {
	return (word & FN_Task) + (field << FS_Task);
}

Task_t SBA::getArgPos(Word word) {
	return (word & F_Task) >> FS_Task;
}

Word SBA::setArgPos(Word word,Word field) {
	return (word & FN_Task) + (field << FS_Task);
}



Subtask_t SBA::getSubtask(Word word) {
	return (word & F_Subtask) >> FS_Subtask;
}


MemAddress SBA::getDataAddress(Word word) {
    return ((word & F_Subtask) >> FS_Subtask) & FW_DataAddress;
}


Word SBA::setSubtask(Word word,Word field) {
	return (word & FN_Subtask) + (field << FS_Subtask);
}

Name_t SBA::getName(Word word) {
	return (word & F_Name) >> FS_Name;
}

Word SBA::setName(Word word,Word field) {
	return (word & FN_Name) + (field << FS_Name);
}

DS_t SBA::getStatus(Word word) {
    return getTask(word);
}

Word SBA::setStatus(Word word,DS_t status) {
	return setTask(word,status);
}

void* SBA::getPointer(Word word) {
    return (void*)((word & F_ExtValue) >> FS_ExtValue);
}

ExtValue_t SBA::getExtValue(Word word) {
    return (word & F_ExtValue) >> FS_ExtValue;
}

Word SBA::setExtValue(Word word,Word field) {
    return (word & FN_ExtValue) + (field << FS_ExtValue);
}

Symbol_t SBA::mkPointerSymbol(void* vp) {
	return setExtValue(PTRSYM,(Word)vp);
}

Value_t SBA::getValue(Word word) {
	return (word & F_Value) >> FS_Value;
}

Word SBA::setValue(Word word,Word field) {
    return (word & FN_Value) + (field << FS_Value);
}

NSymbols_t SBA::getNSymbols(Word word) {
    return (word & F_NSymbols) >> FS_NSymbols;
}

Word SBA::setNSymbols(Word word,Word field) {
    return (word & FN_NSymbols) + (field << FS_NSymbols);
}

uint SBA::getNPadBytes(Word word) {
    return ((word & F_NPadBytes) >> FS_NPadBytes) & 0x7; // 0..7
}


Int SBA::getInt(Word_List& words) {
     Int int_result; Word result;
    if (getExt(words[0])==1){
        result=words[1];
        int_result=(Int)result;//printf("from getint: Ext==1, result == %d\n", int_result);
    } else {
        result=getValue(words[0]);//printf("from getint: %d\n", (int)result);
        Word  one=1;
        int_result= (result>(one<< (FB_Value-1)))?(result-(one<< FB_Value)):result;
    }
    return int_result;
}


Int SBA::getInt(Word word) {
     Int int_result; Word result;
	 result=getValue(word);
	 Word  one=1;
	 int_result= (result>(one<< (FB_Value-1)))?(result-(one<< FB_Value)):result;
    return int_result;
}


Word SBA::getUInt(Word_List words) {
     Word result;
    if (getExt(words[0])==1){
        result=words[1];
    } else {
        result=getValue(words[0]);
    }
    return result;
}

Word SBA::getUInt(Word word) {
     Word result;
	 result=getValue(word);
    return result;
}


Word SBA::getWord(Word_List words) {
     Word result;
    if (getExt(words[0])==1){
        result=words[1];
    } else {
        result=getValue(words[0]);
    }
    return result;
}

double SBA::getFloat(Word word) {
     double result;
    if (getExt(word)==1){
        Word word=0; // FIXME: get the pointer and dereference!
#if WORDSZ==64
            double *dbl_p=(double*)word;
            result= *dbl_p;
#else // WORDSZ==32
            float *flt_p=(float*)word;
            float flt_result= *flt_p;
            result=(double)flt_result;
#endif // WORDSZ
    } else {
#if WORDSZ==64
        Word val=getValue(word);
        float *flt_p=(float*)val;
        float flt_result= *flt_p;
        result=(double)flt_result;
#else // WORDSZ==32
           cerr << "Floats must be extended for 32-bit FPU\n"; exit(-1);
#endif // WORDSZ
    }

    return result;

}

double SBA::getFloat(Word_List words) {
     double result;
    if (getExt(words[0])==1){
        Word word=words[1];
#if WORDSZ==64
            double *dbl_p=(double*)word;
            result= *dbl_p;
#else // WORDSZ==32
            float *flt_p=(float*)word;
            float flt_result= *flt_p;
            result=(double)flt_result;
#endif // WORDSZ
    } else {
#if WORDSZ==64
        Word        word=getValue(words[0]);
        float *flt_p=(float*)word;
        float flt_result= *flt_p;
        result=(double)flt_result;
#else // WORDSZ==32
           cerr << "Floats must be extended for 32-bit FPU\n"; exit(-1);
#endif // WORDSZ
    }

    return result;

}


char SBA::getChar(Word_List words) {
    uint val = getInt(words);
     char ch=(char)(val & 0xFF);
    return ch;
}


string SBA::getString(Word_List words) {
    Word  header =  words.front(); words.pop_front();
    uint     nwords=getNSymbols(header);
    uint padding=getNPadBytes(header);

    string str;
    str.reserve(nwords*NBYTES-padding);
    for(uint i=0;i<nwords;i++) {
        uint npad=(i==nwords-1)?padding:0;
        Word strword=words.shift();
        for (uint j=0;j<NBYTES-npad;j++) {
            char byte=(char)((strword>>8*(NBYTES-j-1))&255);
            str+=byte;
        }
    }


    return str;
}



uint SBA::getOpcode(Word word) {
    return (getName(word)>>FS_Opcode) & FW_Opcode;
}
Word SBA::setOpcode(Word w, uint opcode) {
	Word name = getName(w);
	name = name - getOpcode(name) + ((opcode & FW_Opcode) << FS_Opcode);
	return setName(w,name);
}

uint SBA::getSCId(Word word) {
    return (getName(word)>>FS_SCId) & FW_SCId;
}


uint SBA::getSCLId(Word word) {
    return (getName(word)>>FS_SCLId) & FW_SCLId;
}
Word SBA::setSCLId(Word w,uint snid) {
	uint name_no_sclid =(getName(w) & FN_SCLId);
	uint name = name_no_sclid + ((snid & FW_SCLId)<<FS_SCLId);
	return setName(w,name);
}


uint SBA::getSNId(Word word) {
    return (getName(word)>>FS_SNId) & FW_SNId;
}
Word SBA::setSNId(Word w,uint snid) {
	uint name_no_snid =(getName(w) & FN_SNId);
	uint name = name_no_snid + ((snid & FW_SNId)<<FS_SNId);
	return setName(w,name);
}

Word SBA::mkSubtaskCodeAddressTup(Subtask st,MemAddress ca) {
	Word w = (((Word)st & 0xFFFFFFFF)<<32)+((Word)ca & 0xFFFFFFFF);
	return w;
};
Subtask SBA::getSubtaskFromTup(Word w) {
	return ((w>>32) & 0xFFFFFFFF);
};

MemAddress SBA::getCodeAddressFromTup(Word w) {
	return (w & FW_Subtask); // 16 bits
};

uint SBA::getSNIdFromTup(Word w) {
	return (w >> FS_SNId) & FW_SNId; // 8 bits
};


CodeAddress SBA::getCodeAddress(Word word) {
//    Word page=(word & F_CodePage)>> FS_Task; // OBSOLETE
//    Word address=(word & F_CodeAddress) >> FS_Subtask;
//    Word sclid = getSCLId(word);
//    Word scid = getSCId(word);
//    Word service = (sclid<<(FS_SCLId - FS_SCId))+scid;
//    Word code_address = address+(scid<<FS_SCId)+(sclid<<FS_SCLId);
    Word code_address = getSubtask(word);
//    Word service=(word & F_Service)>>FS_Name;
//    Word     code_address=(service << FS_Service)+address;
#if VERBOSE==1
//        cout << "Word: " <<ppSymbol(word)<< ""<<endl;
//        cout << "Service: " <<service<< " | Page: " <<page<< " | Address: " <<address<< " => " <<code_address<< ""<<endl;
//        cout << "Service: " <<service<< " | Address: " <<address<< " => CodeAddress: " <<code_address<< ""<<endl;
        //cout << "CodeAddress: " <<code_address<< ""<<endl;
#endif // VERBOSE
	return code_address;
}


Word SBA::setCodeAddress(Word word,Word task,Word subtask) {

cerr << "FIXME! setCodeAddress is not implemented";
exit(1);

	Word wt=(word & FN_CodePage) + (task << FS_Task);
	Word wst=(wt & FN_CodeAddress) + (subtask << FS_CodeAddress);
	return wst;
}


uint SBA::getNArgs(Word word) {
    return (getSubtask(word) & F_NArgs);
}


uint SBA::getToken(Word word) {
    return (getSubtask(word) >> FS_Token) & FW_Token;
}


uint SBA::getNCons(Word word) {
    return (getSubtask(word) >> FS_NCons) & FW_NCons;
}



uint SBA::getReg(Word word) {
    return (getSubtask(word) >> FS_Reg) & FW_Reg;
}


uint SBA::getMode(Word word) {
    return ((getSubtask(word) >> FS_Mode) & FW_Mode);
}


uint SBA::getOffset(Word word) {
    return (word >> FS_Offset) & FW_Offset;
}


uint SBA::getSize(Word word) {
    return (word >> FS_Size) & FW_Size;
}


Header_t SBA::mkHeader(Word packet_type,Word prio,Word redir,Word length,Word to,Word return_to,Word ack_to,Word return_as) {
	Word wpacket_type=(packet_type << FS_Packet_type) & F_Packet_type;
	Word wprio=(prio << FS_Ctrl) & F_Ctrl;
	Word wredir=(redir << FS_Redir) & F_Redir;
	Word wlength=(length << FS_Length) & F_Length;
	Word wto=(to << FS_To) & F_To;
	Word wreturn_to=(return_to << FS_Return_to) & F_Return_to;
	Word w1=wpacket_type+wprio+wredir+wlength+wto+wreturn_to;
     Header_t wl;
     wl.push_back(w1);
     wl.push_back(ack_to);
     wl.push_back(return_as);
     return wl;
}

Packet_type_t SBA::getPacket_type(const Header_t& header) {
	Word w1=header[0];
	return (w1 & F_Packet_type) >> FS_Packet_type;
}

Header_t SBA::setPacket_type(Header_t header,Word field) {
	Header_t  modheader;
	Word w0=(header[0] & FN_Packet_type) + (field << FS_Packet_type);
	modheader.push_back(w0);
	modheader.push_back(header[1]);
	modheader.push_back(header[2]);
	return modheader;
}


Ctrl_t SBA::getCtrl(Header_t& header) {
	Word w1=header[0];
	return (w1 & F_Ctrl) >> FS_Ctrl;
}


Header_t SBA::setCtrl(Header_t& header,Word field) {
	Header_t  modheader;
	Word w0=(header[0] & FN_Ctrl) + (field << FS_Ctrl);
	modheader.push_back(w0);
	modheader.push_back(header[1]);
	modheader.push_back(header[2]);
	return modheader;
}

Redir_t SBA::getRedir(Header_t& header) {
	Word w1=header[0];
	return (w1 & F_Redir) >> FS_Redir;
}

Header_t SBA::setRedir(Header_t& header,Word field) {
	Header_t  modheader;
	Word w0=(header[0] & FN_Redir) + (field << FS_Redir);
	modheader.push_back(w0);
	modheader.push_back(header[1]);
	modheader.push_back(header[2]);
	return modheader;
}

Length_t SBA::getLength(const Header_t& header) {
	Word w1=header[0];
	return (w1 & F_Length) >> FS_Length;
}


Header_t SBA::setLength(Header_t& header,Word field) {
	Header_t  modheader;
	Word w0=(header[0] & FN_Length) + (field << FS_Length);
	modheader.push_back(w0);
	modheader.push_back(header[1]);
	modheader.push_back(header[2]);
	return modheader;
}

To_t SBA::getTo(const Header_t& header) {
	Word w1=header[0];
	return (w1 & F_To) >> FS_To;
}

Header_t SBA::setTo(Header_t& header,Word field) {
	Header_t  modheader;
	Word w0=(header[0] & FN_To) + (field << FS_To);
	modheader.push_back(w0);
	modheader.push_back(header[1]);
	modheader.push_back(header[2]);
	return modheader;
}

Return_to_t SBA::getReturn_to(const Header_t& header) {
	Word w1=header[0];
	return (w1 & F_Return_to) >> FS_Return_to;
}

Header_t SBA::setReturn_to(Header_t& header,Word field) {
	Header_t  modheader;
	Word w0=(header[0] & FN_Return_to) + (field << FS_Return_to);
	modheader.push_back(w0);
	modheader.push_back(header[1]);
	modheader.push_back(header[2]);
	return modheader;
}


Word SBA::getSubtaskArgpos(const Header_t& header) {
       return header[1];
   }
	Word SBA::getSubtaskArgpos_p(const Packet_t& packet) {
       return packet[1];
   }



 Word SBA::getAck_to(const Header_t& header) {
        return header[1];
    }
	Word SBA::getAck_to_p(const Packet_t& packet) {
        return packet[1];
    }
	Word SBA::getReturn_as_p(const Packet_t& packet) {
        return packet[2];
    }
	Return_to_t SBA::getReturn_to_p(const Packet_t& packet) {
        Word w1 = packet[0];
        return (w1 & F_Return_to) >> FS_Return_to;
    }

    Redir_t SBA::getRedir_p(const Packet_t& packet) {
        Word w1=packet[0];
    	return (w1 & F_Redir) >> FS_Redir;
    }
    Ctrl_t SBA::getCtrl_p(const Packet_t& packet) {
        Word w1=packet[0];
        return (w1 & F_Ctrl) >> FS_Ctrl;
    }

    Packet_type_t SBA::getPacket_type_p(const Packet_t& packet) {
    	Word w1=packet[0];
	    return (w1 & F_Packet_type) >> FS_Packet_type;
	}
    Length_t SBA::getLength_p(const Packet_t& packet) {
    	Word w1=packet[0];
	    return (w1 & F_Length) >> FS_Length;
	}


 Word SBA::getReturn_as(const Header_t& header) {
        return header[2];
    }


 Header_t SBA::setReturn_as(Header_t& header,Word field) {
         Header_t  modheader;
         modheader.push_back(header[0]);
         modheader.push_back(header[1]);
         modheader.push_back(field);
         return modheader;
    }


 Header_t SBA::setSubtaskArgpos(Header_t& header,Word field) {
         Header_t  modheader;
         modheader.push_back(header[0]);
         modheader.push_back(field);
         modheader.push_back(header[2]);
         return modheader;
    }

 Header_t SBA::setAck_to(Header_t& header,Word field) {
         Header_t  modheader;
         modheader.push_back(header[0]);
         modheader.push_back(field);
         modheader.push_back(header[2]);
         return modheader;
    }


 Packet_type_t SBA::getType(const Header_t& header) {
		return getPacket_type(header);
	}


 Header_t SBA::setType(Header_t& header,Packet_type_t& field) {
		return setPacket_type(header,field);
	}




 Packet_t SBA::mkPacket(Header_t& header,Word_List& payload) {
         Word_List packet;
        for(uint i=0;i<=HEADER_SZ-1 ;i++) {
            packet.push_back(header[i]);
        }
        for(Word_List::iterator iter_=payload.begin();iter_!=payload.end();iter_++) {
        	Word w=*iter_;
            packet.push_back(w);
        }
        return packet;
    }

 Packet_t SBA::mkPacket_new(Header_t& header,Word payload) {
          Word_List packet;
         for(uint i=0;i<=HEADER_SZ-1 ;i++) {
             packet.push_back(header[i]);
         }
             packet.push_back(payload);
         return packet;
     }


 Header_t SBA::getHeader(Packet_t& packet) {
    	 Header_t header;
    	header.push_back(packet[0]);
    	header.push_back(packet[1]);
    	header.push_back(packet[2]);
        return header;
    }


 Word_List SBA::setHeader(Packet_t& packet,Header_t& header) {
         Word_List npacket;
        for(Word_List::iterator iter_=header.begin();iter_!=header.end();iter_++) {
        	Word w=*iter_;
            npacket.push_back(w);
        }
        Word_List payload=getPayload(packet);
        for(Word_List::iterator iter_=payload.begin();iter_!=payload.end();iter_++) {
        	Word w=*iter_;
            npacket.push_back(w);
        }
        return npacket;
    }


 Word_List SBA::getPayload(Word_List packet) {
         Word_List pl;
        uint i=0;
        for(Word_List::iterator iter_=packet.begin();iter_!=packet.end();iter_++) {
        	Word w=*iter_;
             if (i>=3) {pl.push_back(w);}
            i+=1;
        }
        return pl;
    }

Word SBA::getPayload_Word(Word_List packet) {
         Word pl = NIHIL;
         uint i=0;
         for(Word_List::iterator iter_=packet.begin();iter_!=packet.end();iter_++) {
        	 Word w=*iter_;
             if (i==3) {
            	 //w = w & 0x0000FFFFFFFFFFFF; //Ashkan did this to remove the pointer type // WV use getPointer on the result instead
            	 pl = w;
             }
             i+=1;
         }
         return pl;
     }
 Word_List SBA::getField(Word_List result,uint offset,uint size) {
         Word_List field;
        uint i=0;
        if (size==0){
            if (offset==0){
                return result;
            } else {
                size=result.size();
            }
        }
        for(Word_List::iterator iter_=result.begin();iter_!=result.end();iter_++) {
        	Word w=*iter_;
             if (i>=offset) {field.push_back(w);}
            i+=1;
            if (i>offset+size){
                break;
            }
        }
        return field;
    }


Word SBA::mkBool(uint bval) {
    Symbol_t boolsym = mkSymbol(K_B,T_i,0,1,0,0,bval);
    return boolsym;
}

Symbol_t SBA::mkIntSymbol(Word val) {
    Word w=ZERO;
    if (
            ((val>=0) && (val < 0xFFFFFFFFUL))
           || ((val< 0) && (-val < 0xFFFFFFFFUL))
       ) {
        int32_t tval=(int32_t)val;
        w=setValue(w,tval);
    } else {
        void* vp;
        if (val>=0) {
            u_int64_t* ulwp=new u_int64_t;
            *ulwp = val;
            vp = (void*)ulwp;
        } else {
            int64_t* slwp=new int64_t;
            *slwp = val;
            vp = (void*)slwp;
        }
        Word vpw=(Word)vp;
        w=setExtValue(w,vpw);
        w=setExt(w,1);
    }
   return w;
}

 deque<Int> SBA::to_signed_int_list( Word_List wl) {
           deque<Int> numlist;
        for(Word_List::iterator iter_=wl.begin();iter_!=wl.end();iter_++) {
        	Word w=*iter_;
         Int sw=(Int)w;
            numlist.push_back(sw);
        }
        return numlist;
    }
//#if VERBOSE==1
 string SBA::ppSymbol(Word w) {
         ostringstream outs;
         switch (getKind(w)) {
         case K_R:
        	 outs << "R";
        	 break;
         case K_S:
        	 outs << "S";
        	 break;
         case K_B:
        	 outs << "B";
        	 break;
         case K_P:
        	 outs << "P";
        	 break;
         case K_L:
        	 outs << "L";
        	 break;
         default:
        	 outs << (uint)getKind(w);
         };

         switch (getKind(w)) {
         case K_R:
         case K_S:
        	 outs << ":"<< (uint)getDatatype(w) <<":"<< (uint)getExt(w) <<":"<<(uint)getQuoted(w)<<":"<< (uint)getTask(w) <<":"<< (uint)getSubtask(w) <<":"
        	 << (uint)getSNId(w)<<"."<< (uint)getSCLId(w)<<"."<< (uint)getSCId(w)<<"."<< (uint)getOpcode(w);
        	 break;
         case K_B:
        	 if (getExt(w)==0) {
        	 outs << ":"<< (uint)getDatatype(w)  <<"::"<< (uint)getValue(w);
        	 } else {
        		 outs  <<":"<< (uint)getDatatype(w) <<"::0x"<< std::hex<< (uint)getExtValue(w) <<std::dec <<"";
        	 }
        	 break;
         case K_P:
        	 outs << ":" << std::hex<< getPointer(w) <<std::dec <<"";
        	 break;
         default:
        	 outs << ":"<< (uint)getDatatype(w) <<":"<< (uint)getExt(w) <<":"<<(uint)getQuoted(w)<<":"<< (uint)getTask(w) <<":"<< (uint)getSubtask(w) <<":"<< (uint)getName(w);
         };
	string os=outs.str();
		return os;
    }


 string SBA::ppHeader(Header_t wl) {
        Word w2=wl[1];
        Word w3=wl[2];

	ostringstream outs;
	switch(getPacket_type(wl)) {
		case P_data:
			outs << "pDa";
			break;
		case P_reference:
			outs << "pRe";
			break;
		case P_code:
			outs << "pCo";
			break;
		case P_subtask:
			outs << "pSu";
			break;
		case P_migrate:
			outs << "pMi";
			break;
		case P_clean_up:
			outs << "pCl";
			break;
		default:
			outs << "?"<<(uint)getPacket_type(wl);
	};
//	outs << (uint)getPacket_type(wl) << ":" << (uint)getCtrl(wl) <<":"<< (uint)getRedir(wl) <<":"<<(uint)getLength(wl)<<":"<< (uint)getTo(wl) <<":"<<(uint)getReturn_to(wl) <<"\n"<< ppSymbol(w2)<<"\n"<<ppSymbol(w3);
	outs  << ":" <<(uint)getLength(wl)<<":"<< (uint)getTo(wl) <<":"<<(uint)getReturn_to(wl) <<"\n"<< ppSymbol(w2)<<"\n"<<ppSymbol(w3);
	string os=outs.str();
		return os;
    }


 string SBA::ppPayload(Word_List wl) {
             ostringstream outs;
            uint maxnwords=4;
            if (wl.size()<maxnwords){
                maxnwords=wl.size();
            }
            if (wl.size()>0                ){
                for(uint i=0;i<=maxnwords-1 ;i++) {
                	outs << ppSymbol(wl[i]) <<"\n";
                }
                uint rest=wl.size()-maxnwords;
                if (rest!=0){
                outs<<"... and " <<rest<< " more words\n";
                }
	        string os=outs.str();
                return os;
            } else {
            string                              os="";
            return os;
            }

    }


 string SBA::ppPacket(Word_List wl) {
	ostringstream outs;

	outs << ppHeader(getHeader(wl)) <<"\n------------\n"<< ppPayload(getPayload(wl))<<"\n";
        	string os=outs.str();
		return os;
    }
//#endif // VERBOSE

