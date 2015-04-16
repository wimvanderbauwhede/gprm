#include <vector>
#include <string>
#ifdef GPRM_API
// very bad name!
// #include <API.h> 
//using namespace GPRM;
#endif

namespace GPRM {
namespace Kernel {

class Hello {
	public:
		int say(std::vector< std::string >*, int nid);
};

}
}

