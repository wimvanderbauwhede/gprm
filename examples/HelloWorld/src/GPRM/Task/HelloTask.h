#include "Base/GPRM.h"
#include "GPRM/Kernel/Hello.h"
#include <vector>
#include <string>

typedef std::vector< std::string > Words;

namespace GPRM {

    class HelloTask : public GPRM::Base::Task {
        private:
            GPRM::Kernel::Hello hk;
        public:
            using GPRM::Base::Task::Task;
            void sayHello(Words*);
    };
}

