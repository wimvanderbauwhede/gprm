#include "Hello.h"
#include <iostream>

using namespace GPRM::Kernel;

int Hello::say(std::vector< std::string>* words, int nid) {
    std::cout << "<"<<words->at(nid) << "> from node "<< nid << "\n";
    return nid;
}
