/*
 * (c) 2014 Ashkan Tousimojarad <a.tousimojarad.1@research.gla.ac.uk>
 */
#include "API.h"
#include "System.h"
//using namespace std;
using namespace GPRM;

Word GPRM::GReg(int a) {
	System& sba_sys=*((System*)System::APIsys);//printf("API: %d\n",System::sys);
	//printf("from api: this: %d\n",this->service);
	return sba_sys.regs.at(a) & 0x0000FFFFFFFFFFFF;;
}

void GPRM::GReg(int a, Word b) {
	System& sba_sys=*((System*)System::APIsys);
	sba_sys.regs.at(a) = b;
}

Word GPRM::get_shared(int a) {
	System& sba_sys=*((System*)System::APIsys);//printf("API: %d\n",System::sys);
	//printf("from api: this: %d\n",this->service);
	return sba_sys.regs.at(a) & 0x0000FFFFFFFFFFFF;;
}

void GPRM::put_shared(int a, Word b) {
	System& sba_sys=*((System*)System::APIsys);
	sba_sys.regs.at(a) = b;
}

void* GPRM::get_arg(int a) {
	System& sba_sys=*((System*)System::APIsys);//printf("API: %d\n",System::sys);
	//printf("from api: this: %d\n",this->service);
	return sba_sys.args.at(a) ;
}

void GPRM::put_arg(int a, void* b) {
	System& sba_sys=*((System*)System::APIsys);
	sba_sys.args.at(a) = b;
}
