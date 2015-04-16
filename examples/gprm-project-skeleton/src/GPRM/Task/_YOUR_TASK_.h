#ifndef __YOUR_TASK_H__
#define __YOUR_TASK_H__
#include "Base/GPRM.h"
#include "GPRM/Kernel/_YOUR_KERNEL_.h"

namespace GPRM {

    class _YOUR_TASK_: public GPRM::Base::Task {
        private:
            GPRM::Kernel::_YOUR_KERNEL_ yk;
        public:
            using GPRM::Base::Task::Task;
            _YOUR_TYPE_ _YOUR_TASK_METHOD(_YOUR_ARGS_);
    };
}
#endif
