data Operation = Change | InsertXs | InsertYs | DeleteXs | DeleteYs | ReOrder

getf Change   = \(x:xs) (y:ys)   -> ([],xs,ys)
getf InsertXs = \xs     ys@(y:_) -> ([],y:xs,ys)
getf InsertYs = \xs@(x:_) ys     -> ([],xs,x:ys)
getf DeleteXs = \(x:xs) ys       -> ([],xs,ys) 
getf DeleteYs = \xs     (y:ys)   -> ([],xs,ys)
getf ReOrder  = \(x:xs)   ys     ->
	                case mkHead x ys [] of
	                    Just ys' -> ([ReOrder],xs,ys')
	                    _ -> ([],x:xs,ys)
            where
                mkHead _ []     _  = Nothing
                mkHead h (x:xs) ys 
                    | h == x    = Just (xs ++ ys) 
                    | otherwise = mkHead h xs (x:ys)

unifyable args =
    case args of
        (_, [], [])                -> return True
        (ops, x:xs, y:ys) | x == y -> unifyable (ops,xs,ys)
        (ops, xs, ys)              -> do { op <- ops; unifyable ((getf op) xs ys) }

neighbours xs ys = any id $ unifyable (operations, xs, ys)
    where 
        operations = 
            case length xs - length ys of
                0  -> [Change,ReOrder]
                1  -> [InsertYs]
                -1 -> [InsertXs]
                _  -> []
