;Test.yml

(par 
    '(Kernel.Multi.Multi[0] '20 '20) 
    (seq 
        '(reg.write[0] '1 '6) 
        '(if[0] 
            (<[0] 
                (reg.read[0] '1) '3) 
            '(+[0] 
                (par 
                    (reg.write[0] '2 test.print[0]) 
                    (par 
                        (test.print[0] '0) 
                        (test.print[0] '1) 
                        (test.print[0] '2) 
                        (test.print[0] '3) 
                        (test.print[0] '4) 
                        (test.print[0] '5)) 
                    (reg.write[0] '3 test.m1[0]) '1) '48))))