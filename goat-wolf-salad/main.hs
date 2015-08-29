-- https://go-left.com/blog/programming/100-little-programming-exercises/

import Control.Monad
import Data.List

data Item = Goat | Wolf | Salad | Empty deriving (Show, Eq)

solve = mover [Goat,Wolf,Salad] [] []
	where
		mover [] _ _ = return []
		mover left right boat = do 
		                -- unload boat
		                let left' = left ++ boat
		                -- select what to move
		                x <- left  
		                -- load boat
		                let boat' = [x]
		                let left'' = left' \\ boat'
		                -- verify that we can leave
		                guard $ compatible left''
		                -- handle next step
		                map (x:) (movel left'' right boat')

		movel [] _ _ = return []
		movel left right boat = do 
		                -- unload boat
		                let right' = right ++ boat
		                -- select what to move
		                if compatible right' then
		                	-- no problem leaving all on the right side. lets just go back empty
			                map (Empty:) (mover left right' [])
		                else do
		                	-- we have a problem. pick one to move back to the left
			                y <- right  
			                -- load boat
			                let boat' = [y]
			                let right'' = right' \\ boat'
			                -- verify that we can leave
			                guard $ compatible right''
			                -- handle next step
			                map (y:) (mover left right'' boat')

		compatible [Goat,Wolf] = False
		compatible [Wolf,Goat] = False
		compatible [Salad,Goat] = False
		compatible [Goat,Salad] = False
		compatible _ = True
