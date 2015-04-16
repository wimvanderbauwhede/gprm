#include "../src/GPRM/Task/__TASK_NAME__.h"
#include <vector>
// We take the task signature from the .gpc file and prepend GPRM::Task::
//
__TASK_DEFINITION__ {
	std::vector< void* > args;
// construct args
	__CAST_ARGS__
	_gprm->init("__TDC_PATH__");
	void* res_vp = _gprm->run(args); // so run gets argument const std::vector< uint64_t* >&
// extract return value res
//	void* res_vp = args.back();
//	T2* res = (T2*)res_vp;
	__CAST_RES__
	__RETURN_RES__
//	return res;
}


