#include "GPRM/Task/HelloTask.h"

int main () {
    const unsigned int nthreads=8;
    GPRM::HelloTask* gprm=new GPRM::HelloTask(nthreads);
    std::string sentence[12]={"'t Wordt"," al sterre"," dat men ziet"," in dat hoog"," en blauw"," verschiet daar,"," blijde"," sterren, ","anders niet,"," in dat hoog"," en blauw"," verschiet."};
    std::vector< std::string >* words = new  std::vector< std::string >;
    for(unsigned int i=0;i<12;i++) {
        words->push_back(sentence[i]);
    }
    gprm->sayHello(words);
    return 0;
}
