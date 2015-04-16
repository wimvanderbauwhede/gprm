#ifndef _SBA_BASE_TYPES_H_
#define _SBA__BASE_TYPES_H_

#include <sys/types.h>

using namespace std;

typedef unsigned int uint; // most likely 32bit

typedef u_int8_t uint8;
typedef u_int16_t uint16;
typedef u_int32_t uint32;
//typedef u_int64_t uint64;

namespace SBA {
        typedef u_int64_t Uint64;
        typedef u_int32_t Uint32;
		typedef u_int16_t Uint16;
		typedef u_int8_t Uint8;
		typedef int64_t Sint64;
        typedef int32_t Sint32;
        typedef unsigned long int MWord; // machine word size, needed for void* manipulation
		typedef Uint64 Word;
		typedef Uint64 Label;
		typedef Sint64 Int;
		typedef double Float;
		typedef Word Uint;
} // SBA
#endif // _BASE_TYPES_
