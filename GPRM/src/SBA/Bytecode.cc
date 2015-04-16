// Bytecode.rb
//   
// Gannet Service-based SoC project - Service Configuration module
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: Bytecode.rb 2263 2009-02-19 11:48:28Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/Bytecode.rb by ../../util/r2n.pl on Sun Apr 22 17:14:05 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******




#include "Bytecode.h"

Word SBA::bitmask(uint8 n,uint8  m) {

        Word bm=(((Word)1<<(1+n-m))-(Word)1)<<m;       
        return bm;
}

Word SBA::getrange(Word w,uint8 n,uint8  m) {
  if (m>0) {
        return (w & SBA::bitmask(n,m))>>m;
        } else {
        return w & SBA::bitmask(n,m);
        }
}

Word SBA::field(uint8 n,uint8 m, uint f) {
return ((Word)f<<m) & SBA::bitmask(n,m);
}

Word SBA::setrange(Word w, uint8 n, uint8 m,uint f) {
/* must do w - [current value of field] + field(n,m,f) */
    Word cf=SBA::getrange(w,n,m);
    Word nw=w-cf;
    nw=nw+SBA::field(n,m,f);
    return nw;
}

