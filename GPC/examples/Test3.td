;Test3.yml

'(GPRM.Kernel.Test.Test[0] '0) 
'(GPRM.Kernel.Test.Test[0] '1) 
(par 
    (reg.write[0] '4 
        (+[0] 
            (reg.read[0] '1) '6)) 
    (reg.write[0] '5 
        (-[0] 
            (reg.read[0] '2) '7)) 
    (reg.write[0] '6 testsExpLit 0.m0[0]) 
    (par 
        (testsExpIdent "i".m1[0] 
            (reg.read[0] '1) 
            (reg.read[0] '2) 
            (reg.read[0] '3) 
            (+[0] 
                (reg.read[0] '6) '5)) 
        (testsExpIdent "i".m1[0] 
            (reg.read[0] '1) 
            (reg.read[0] '2) 
            (reg.read[0] '3) 
            (+[0] 
                (reg.read[0] '6) '5)) 
        (testsExpIdent "i".m1[0] 
            (reg.read[0] '1) 
            (reg.read[0] '2) 
            (reg.read[0] '3) 
            (+[0] 
                (reg.read[0] '6) '5))) '0)