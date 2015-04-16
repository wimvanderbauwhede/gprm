void Base::CoreServices::ls_CTRL() {
	// CTRL 
	// (ctrl.run '(merge_sort ) '4)
#if VERBOSE == 1
				std::cout << "CTRL CORE .run(): " << method() << "<>" << M_CoreServices_CTRL_run << "\n";
#endif // VERBOSE

	switch ( method() ) {

		case M_CoreServices_CTRL_run:
			if (is_quoted_ref(0)) {
				// OK, a quoted ref, we can do this
				uint dest = getUInt(arg(1));
#if VERBOSE == 1
				std::cout << "CTRL CORE .run(): Setting dest for "<<ppSymbol(arg(0)) <<" to " << dest <<"\n";
#endif // VERBOSE
				dispatch_reference(0,dest);
				break;
			} else {
#if VERBOSE == 1
				std::cout << "CTRL CORE .run(): arg 0 is not a quoted ref, returning.\n";
#endif // VERBOSE
				result(arg(0));
			}
			break;
		default:
#if VERBOSE == 1
			std::cout << "CTRL CORE: don't know this method, returning.\n";
#endif // VERBOSE
			result(arg(0));

	};

}
