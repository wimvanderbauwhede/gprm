About
===

GPC is a front end language for the Glasgow Parallel Reduction Machine (GPRM) framework and is also my honours year project as a Student at the University of Glasgow. The language is a subset of C++ with two added keywords ("seq" and "par") to denote either sequential or parallel execution of code blocks. By default statements are evaluated in parallel.

The compiler (GPCC) is a hybrid compiler/interpreter implemented in Haskell which performs type/scope checking on the entire GPC file. It then interprets the execution path of the GPC code from the entry function (except for actual kernel calls) and generates the kernel calls as Glasgow Parallel Intermediate Representation (GPIR) code to be ran on the GPRM. The purely functional nature of the language makes it possible to interpret the entire program at compile time.

My dissertation on the design and compliation of the language can be found [here](https://github.com/RossMeikleham/Dissertation)

The GPRM is a research project at the University Of Glasgow, more information on it can be found [here]( http://arxiv.org/pdf/1312.2703v1.pdf)


Example
=======
```c++
#include "GPRM/Task/TestTask.h"

int n = 4; // All variables in GPC are constant

void GPRM::TestTask::entryFunction() {
    seq { // Sequentially evaluate each statement in this block

        /* For loop, loops are completely unrolled at compile time
           and each individual statement is executed in parallel */
       for (int i = 0; i < n; i += 1) {
                test.method(i);
        }
    }
}
```

Building
========
```
cabal configure --enable-tests
cabal build
```

Installing
==========
`cabal install`

Running
=======
`gpcc source-file.gpc [--threads=n]`

If the number of threads aren't specified when compiling, the compiler will set the maximum
thread count to the number of available cores on the machine. Threads are used to assign each
GPIR task to a thread when it is run on the GPRM.

If compilation is successful this should generate a GPIR file of the same name as the source but with a .td extension, this file will be in the same folder as the source file.

See the examples folder for some programs which can currently be compiled, as well as the "pass" tests in the tests folder.

Running Tests
=============
`cabal test`
