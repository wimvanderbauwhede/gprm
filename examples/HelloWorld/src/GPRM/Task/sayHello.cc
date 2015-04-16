#define seq
#include "GPRM/Task/HelloTask.h"

void GPRM::HelloTask::sayHello(Words* words) {    
    for(int i=0;i<8;i+=1) {
        hk.say( words,i);
    }
}
