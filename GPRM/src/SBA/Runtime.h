#ifndef RUNTIME_H_
#define RUNTIME_H_

// SBA/Runtime.rb
//
// :title: Gannet Service-based SoC project - SBA Runtime class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//
//==============================================================================
//
// Gannet Service-based SoC project - SBA Runtime class
//
//==============================================================================
//

#include "Types.h"
#include "SystemConfiguration.h"
#include "System.h"
#include <string>
#include <vector>

using namespace std;

namespace SBA {
class Runtime {
public:
	unsigned int nthreads;
//	TaskDescList task_descriptions;
	std::string task_description;
	Bytecode bytecode;
	System sba;
	bool reuse;

	Runtime(unsigned int nth_) : nthreads(nth_), sba("NONE", nth_), reuse(false),  ncycles(500) {};
//	Runtime(TaskDescList& td_) :
//		nthreads(NSERVICES), task_descriptions(td_), sba(task_descriptions, nthreads), reuse(false) {
//	};
	Runtime(std::string td_, uint ncycles_ = 500) :
		nthreads(NSERVICES),  task_description(td_), sba(task_description, nthreads), reuse(false), ncycles(ncycles_) {
	};

//	Word_List& run_OFF();
	void init(std::string td_, uint ncycles_ = 500);
	void setNCycles(uint);
    void args(const vector< void* >&);
/*
	void args(void*);
	void args(void*,void*);
	void args(void*,void*,void*);
	void args(void*,void*,void*,void*);
*/
    void* run(const vector< void* >& );

	void* run();
/*
	void* run(void*);
	void* run(void*,void*);
	void* run(void*,void*,void*);
	void* run(void*,void*,void*,void*);
*/
private:
	uint ncycles;
};
// Clas Runtime
}// SBA

#endif // RUNTIME_H_
