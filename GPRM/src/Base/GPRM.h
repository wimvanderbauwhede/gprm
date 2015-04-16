#ifndef __GPRM_H__
#define __GPRM_H__
#define seq
#define par
#ifdef GPRM_API
#include "SBA/Runtime.h"
#endif
namespace GPRM {
namespace Base {
class Task {
protected:
#ifdef GPRM_API
	SBA::Runtime* _gprm;
#endif
	bool _constructed;
public:
	Task(int nthreads=0) {
#ifdef GPRM_API
		_gprm = new SBA::Runtime(nthreads);
#endif
		_constructed = true;
	};

	void run();

};
}
}
#endif
