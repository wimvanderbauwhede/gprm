
#ifndef BYTECODE_H_
#define BYTECODE_H_

// Bytecode.rb
//   
// Gannet Service-based SoC project - Service Configuration module
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
// $Id: Bytecode.rb 2263 2009-02-19 11:48:28Z socgroup $

// ****** Code generated from /home/ashkan/Gan04/Garnet/SBA/Bytecode.rb by ../../util/r2n.pl on Sun Apr 22 17:14:05 2012 ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#include <cmath>

#include "Types.h"

using namespace SBA;

namespace SBA {

Word bitmask(uint8 n,uint8  m);
Word getrange(Word w,uint8 n,uint8  m);
Word field(uint8 n,uint8 m, uint f);
Word setrange(Word w, uint8 n, uint8 m,uint f);


 }


#endif // BYTECODE_H_
