//
// :title: Garnet Service-based SoC project - SBA TaskDescription class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: TaskDescription.rb 2155 2009-01-28 11:39:41Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/TaskDescription.rb by ../../util/r2n.pl on Sun Apr 22 17:14:09 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#include <vector>
#include <stdio.h>
#include "TaskDescription.h"


using namespace std;
using namespace SBA;




	//Constructor
//	TaskDescription::TaskDescription(string tdc_file) {
//    	byc2td(tdc_file);
//    }; // constructor
	TaskDescription::TaskDescription(Bytecode& tdc_wl) {
    	wl2td(tdc_wl);
    }; // constructor


// OBSOLETE
// void TaskDescription::byc2td(string tdc_file) {
// cout << tdc_file << endl;
//         FILE * fd=fopen(tdc_file.c_str(),"r");
//        uint npackets=0;
//    	uint b0=0;
//    	for(uint ii=0;ii<=NBYTES-1 ;ii++) {
//    		 unsigned short int byte=fgetc(fd);
//    		if (ii==0){
//    			b0=byte;
//    		} else if (ii==1 ){
//    			npackets=(b0 << 8)+byte;
//    		}
//    	}
//#ifdef VERBOSE
//    	cout << "NPackets: " <<npackets<< ""<<endl;
//#endif // VERBOSE
//        Word_List subtask_packet;
//        for(uint np=0;np<=npackets-1 ;np++) {
//    		 Header_t header_words;
//    	    header_words=byc2header(fd);
//            Uint16 length=getLength(header_words);
//            Packet_type_t                   	 ptype= getPacket_type(header_words);
//             Header_t header(header_words);
//            Word_List payload=byc2playload(length,fd);
//            Packet_t packet = mkPacket(header,payload);
//#ifdef VERBOSE
//                      cout << "========"<<endl;
//                      cout << ppPacket(packet)<<endl;
//#endif // VERBOSE
//            if ((ptype==P_subtask)){
//                subtask_packet=packet;
//            } else {
//                 Packets.push_back(packet);
//            }
//        }
//        Packets.push_back(subtask_packet);
//        fclose(fd);
//    }

 void TaskDescription::wl2td(Bytecode& tdc_wl) {

	 	Word npackets=(Word)tdc_wl.front();tdc_wl.pop_front();
    	npackets = npackets >> (NBYTES*8-16);
        if (npackets==0){
            exit(0);
        }
#ifdef VERBOSE
    	cout << "NPackets: " <<npackets<< ""<<endl;
#endif // VERBOSE
    	Packets.clear();
        Word_List subtask_packet;
        for(uint np=0;np<=npackets-1 ;np++) {
    		 Header_t header_words;
        	for(uint phw=0;phw<=2 ;phw++) {
            	 Word hw=tdc_wl.front();tdc_wl.pop_front();
                header_words.push_back(hw);
    	     }
             uint length=getLength(header_words);
//             Packet_type_t                     ptype= getPacket_type(header_words);
              Header_t header(header_words);
#ifdef VERBOSE
            cout << "Length: " <<length<< ""<<endl;
#endif // VERBOSE

            Word_List  payload;
            for(uint j=0;j<=length-1 ;j++) {
                 Word plw=tdc_wl.front();tdc_wl.pop_front();
                payload.push_back(plw);
            }

             Packet_t packet = mkPacket(header,payload);
#ifdef VERBOSE
                      cout << "========"<<endl;
                      cout << "TD:** " <<ppPacket(packet)<<endl;
#endif // VERBOSE
                Packets.push_back(packet);
           }
    }

// Header_t TaskDescription::byc2header(FILE* fd) {
//    	 Header_t header_words; // Word_List header_words(3);
//    	for(uint phw=0;phw<=2 ;phw++) {
//        	Word hw=0;
//	        for(uint hwb=0;hwb<=NBYTES-1 ;hwb++) {
//	             Uint64 byte=fgetc(fd);
//                hw+=(byte<<(8*(NBYTES-1-hwb)));
//            }
//            header_words.push_back(hw);
//	     }
//	     return header_words;
//    } // of byc2header()
//
//
// Word_List TaskDescription::byc2playload(Uint16 length,FILE* fd) {
//        Word_List  payload;
//        for(int j=0;j<=length-1 ;j++) {
//            Word plw=0;
//            for(uint plwb=0;plwb<=NBYTES-1 ;plwb++) {
//                 Word byte=fgetc(fd);
//                plw+=(byte<<(8*(NBYTES-1-plwb)));
//            }
//            payload.push_back(plw);
//        }
//        return payload;
//    } // of byc2payload()

