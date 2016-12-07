-- http://www.shiftedup.com/2015/05/07/five-programming-problems-every-software-engineer-should-be-able-to-solve-in-less-than-1-hour

import Control.Monad
import Control.Monad.State 

-- whileM_ from Control.Monad.LoopWhile
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()

sumUsingWhile :: (Num a) => [a] -> a            
sumUsingWhile xs = snd $ execState loop (xs,0)
    where
        loop = whileM_ (not . null . fst <$> get) $
                    modify $ \(x:xs,sum) -> (xs, x+sum)

