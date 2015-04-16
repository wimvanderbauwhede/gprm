// I should call this Stack!
#ifndef _LS_APPLY_
#define _LS_APPLY_

#include <stack>
namespace SBA {
namespace Base {

class Apply {
	private:
		std::stack<u_int32_t> _stack;
	public:
		Apply(unsigned int start_addr, unsigned int stacksz) {
			for (u_int32_t i=start_addr;i<start_addr+stacksz;i++) {
			_stack.push(i);
			}
		};
		inline u_int32_t top() {
			return _stack.top();
		}
		inline void pop() {
			_stack.pop();
		}
		inline void push(u_int32_t elt) {
			_stack.push(elt);
		}
};
}}
#endif // _LS_APPLY_
