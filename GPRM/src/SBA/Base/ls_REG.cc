 void Base::CoreServices::ls_REG() {
     // REG has only one method and does always the same thing: sequencing computations and returning the last one
 #ifdef VERBOSE
          cout << "REG CORE\n";
 #endif // VERBOSE
	uint method_code=method();
	Word res=0;
	unsigned int regno = getUInt(core_arg(0));
	switch (method_code) {
		case M_CoreServices_REG_write: {
			Word val = core_arg(1);
			system().regs.at(regno)=val;
			res=val;
			break;
		}
		case M_CoreServices_REG_read: {
			res = system().regs.at(regno);
			break;
		}
		case M_CoreServices_REG_inc: {
			system().regs.at(regno)++;
			res = system().regs.at(regno);
			break;
		}
		case M_CoreServices_REG_dec: {
			system().regs.at(regno)--;
			res = system().regs.at(regno);
			break;
		}
		case M_CoreServices_REG_mul: {} // reg*=val
		case M_CoreServices_REG_add: {} // reg+=val
		case M_CoreServices_REG_sub: {} // reg-=val
		case M_CoreServices_REG_div: {} // reg/=val
		case M_CoreServices_REG_mulacc: {} // reg+=val1*val2
		default: {}
	};
 #ifdef VERBOSE
	cout << "REG CORE: Returning "<<res<<"\n";
 #endif // VERBOSE
	result(res);
 }
