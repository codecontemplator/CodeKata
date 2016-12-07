-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

fibSeq = fibSeq' 0 1
    where        
        fibSeq' n' n = n' : fibSeq' n (n'+n)
