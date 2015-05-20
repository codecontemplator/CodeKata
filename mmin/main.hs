import Prelude hiding (take)
import Data.Heap
--import Data.Maybe (fromJust)

{-
import Data.List.Ordered
fsm :: Ord a => Int -> [a] -> [a]
fsm m = foldr update []
  where      
    insert = insertBagBy (flip compare)
    update x minSet@(~(maxElem:minSet'))
        | length minSet < m = insert x minSet
        | x < maxElem       = insert x minSet'
        | otherwise         = minSet

fsm2 :: Ord a => Int -> [a] -> [a]
fsm2 m xs = foldr update (sort xs_) _xs
  where      
    (xs_,_xs) = splitAt m xs
    rcomp  = flip compare
    sort   = sortBy rcomp
    insert = insertBagBy rcomp
    update x minSet@(maxElem:minSet')
        | x < maxElem       = insert x minSet'
        | otherwise         = minSet

-}

{-
fsm :: Ord a => Int -> [a] -> [a]
fsm m = toList . foldr update (empty :: MaxHeap a)
  where   
    findMax   = fromJust . viewHead
    deleteMax = drop 1
    update x minSet
        | size minSet < m     = insert x minSet
        | x < findMax minSet  = insert x (deleteMax minSet)
        | otherwise           = minSet
-}

fsm :: Ord a => Int -> [a] -> [a]
fsm m = take m . (fromList :: Ord a => [a] -> MinHeap a)

fs :: Ord a => [a] -> a
fs = foldr1 (\x min -> if x < min then x else min)
