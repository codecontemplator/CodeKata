-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

import Control.Monad
import Control.Monad.State 

sumUsingFor ::  (Foldable t, Num s) => t s -> s
sumUsingFor xs = execState loop 0
    where 
        loop = forM_ xs $ \x -> modify (+x)


