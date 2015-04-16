#include "Base/Let.h"

using namespace std;
using namespace SBA; 

void Base::Let::assign(SBA::Symbol_t var, SBA::Symbol_t val) {
    _store[var]=val;
}

void Base::Let::put_subtask(SBA::Symbol_t var, SBA::Subtask st) {
    _subtask[var]=st;
}

SBA::Subtask Base::Let::get_subtask(SBA::Symbol_t var) {
	return _subtask[var];
};

SBA::Symbol_t Base::Let::read(SBA::Symbol_t var) {
    return _store[var];
}

bool Base::Let::exists(SBA::Symbol_t var) {
    return (_store.count(var)>0);
}

bool Base::Let::pending_reads(SBA::Symbol_t var) {
    return (_pending_reads[var].size()>0);
}

 SBA::Subtask Base::Let::get_pending_read(SBA::Symbol_t var) {
	 SBA::Subtask st = _pending_reads[var].front();
     _pending_reads[var].pop_front();
     return st;
 }
 
void Base::Let::put_pending_read(SBA::Symbol_t var, SBA::Subtask read) {
    _pending_reads[var].push_back(read);
}

bool Base::Let::pending_streams(SBA::Symbol_t var) {
    return (_pending_streams[var].size()>0);
}

 SBA::Subtask Base::Let::get_pending_stream(SBA::Symbol_t var) {
	 SBA::Subtask st = _pending_reads[var].front();
	 _pending_streams[var].pop_front();
     return st;
 }

void Base::Let::put_pending_stream(SBA::Symbol_t var, SBA::Subtask stream) {
	_pending_streams[var].push_back(stream);
}


void Base::Let::clear() {
	// Before we clear the store etc, we need to de-allocated the memory for all stored pointers
	// So we test if it is a pointer using K_P and if it is, we delete it.
	// The Datatype can be used to work out if it is delete or delete[]
//	for (std::map<SBA::Symbol_t,SBA::Symbol_t>::iterator iter_=_store.begin(); iter_!=_store.end();iter_++) {
//		SBA::Symbol_t sym = (*iter_).second;
//		if (SBA::getKind(sym)==SBA::K_P) {
//			// FIXME: for delete to work properly, we need the correct type! Big problem!
//			void* vp = (void*)(SBA::getExtValue(sym));
//			delete vp;
//		}
//	}
	_store.clear();
	_pending_reads.clear();
	_pending_streams.clear();
}

void Base::Let::remove(SBA::Symbol_t var) {
	_store.erase(var);
	_pending_reads.erase(var);
	_pending_streams.erase(var);
}
