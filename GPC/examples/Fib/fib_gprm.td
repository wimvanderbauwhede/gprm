;fib_gprm.yml

(seq 'f.start_timer[0] 
    '(reg.write[0] '3 
        (par 
            (reg.write[0] '1 
                (f.fib[0] '9)) 
            (reg.write[0] '2 
                (f.fib[0] '8)) 
            (+[0] 
                (reg.read[0] '1) 
                (reg.read[0] '2)))) 
    '(f.timer[0] 
        (reg.read[0] '3)))