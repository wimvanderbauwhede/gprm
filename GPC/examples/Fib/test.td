;test.yml

(seq 
    '(reg.write[0] '1 m1.t[0]) 
    '(par 
        (reg.write[0] '2 
            (m1.t[0] 
                (reg.read[0] '1)))))