

# Compiling for single threaded C++
cpp:: cpp_main.o cpp_Fib.o cpp_fib_gprm.o 
	g++ -o fib main.o Fib.o fib_gprm.o
	
	
cpp_main.o :: 
	g++ -c "main.cc"

cpp_fib_gprm.o :: 
	g++ -c -x c++ "fib_gprm.gpc"

cpp_Fib.o :: 
	g++ -c "./src/GPRM/Kernel/Fib.cc"

# Compiling for GPRM
