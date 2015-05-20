import Math.LinearEquationSolver

-- ref: http://www.codechef.com/problems/E2
-- ref: http://mathworld.wolfram.com/LightsOutPuzzle.html
-- ref: http://cvc4.cs.nyu.edu/cvc4-builds/win32-opt/
-- ref: http://arxiv.org/ftp/math/papers/0702/0702488.pdf

indices n = [(i,j) | i <- [0..n-1], j <- [0..n-1]]

makeA n i j =
    map f (indices n)
  where
    f (i',j') = case (abs(i'-i),abs(j'-j)) of
                  (0,0) -> 1
                  (1,0) -> 1
                  (0,1) -> 1
                  _     -> 0
makeM n = map (-2:) (makeM' n)
  where makeM' n = map (uncurry (makeA n)) (indices n)

-- mapM_ putStrLn $ map show $ makeM 3

solve n b = do
    s <- solve' n b
    case s of
      Just x -> return $ map (\x -> mod x 2) $ tail $ x
      _ -> return []
  where
    solve' n b = solveIntegerLinearEqs CVC4 (makeM n) b

test = solve 3 board
  where board = [0,1,0,1,1,0,0,1,1]
