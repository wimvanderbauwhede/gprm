#include "Task.h"
// We take the task signature from the .gpc file and prepend GPRM::Task::
//
__TASK_DEFINITION__ {
	std::vector< uint64_t* > args;
// construct args
	__SET_ARGS__
	
	gannet->run(args); // so run gets argument const std::vector< uint64_t* >&
// extract return value res
	__GET_RES__
	return res;
}
