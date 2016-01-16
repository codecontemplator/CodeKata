-- http://en.wikipedia.org/wiki/Gaussian_elimination
{-
for k = 1 ... min(m,n):
   Find the k-th pivot:
   i_max  := argmax (i = k ... m, abs(A[i, k]))
   if A[i_max, k] = 0
     error "Matrix is singular!"
   swap rows(k, i_max)
   Do for all rows below pivot:
   for i = k + 1 ... m:
     Do for all remaining elements in current row:
     for j = k + 1 ... n:
       A[i, j]  := A[i, j] - A[k, j] * (A[i, k] / A[k, k])
     Fill lower triangular matrix with zeros:
     A[i, k]  := 0
-}

{-
gauss 1 rs = rs
gauss k ((a_kk:r):rs) = r : gauss (k-1) (eliminate c (map (splitAt 1) rs))
  eliminate a_kk a_ik (c:cs) =
-}

-- All-Integer Echelon
-- http://www.math.iupui.edu/~momran/m118/integers.html
-- http://en.wikipedia.org/wiki/Gaussian_elimination
-- http://en.literateprograms.org/Row_echelon_form_%28Haskell%29

--vsub :: (Fractional a) => [a] -> [a] -> [a]
vsub a b = zipWith (-) a b

--vscale  :: (Fractional a) => a -> [a] -> [a]
vscale c v = map (*c) v

--elementryRowOpperation :: (Fractional a) => [a] -> [a] -> [a]
elementryRowOpperation us vs = vsub vs (vscale (v/u) us)
     where u = head us
           v = head vs

--ref :: (Fractional a) => [[a]] -> [[a]]
ref (ms:[]) = [ms]
ref (ms:mss) = ms : zipWith (:) (map head nss) (ref (map tail nss))
      where nss = map (elementryRowOpperation ms) mss

m = [
    [2.0,1.0,3.0,5.0],
    [0.0,2.0,-1.0,5.0],
    [2.0,1.0,2.0,6.0]
  ]

-- example from http://en.wikipedia.org/wiki/Gaussian_elimination
m2 = [
    [2.0, 1.0, -1.0, 8],
    [-3.0, -1.0, 2.0, -11.0],
    [-2.0, 1.0, 2.0, -3.0]
  ]

-- [[2.0,1.0,-1.0,8.0],[0.0,0.5,0.5,1.0],[0.0,0.0,-1.0,1.0]]


-- https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/

type Number = Double
type Vector = [Number]
type Row    = [Number]
type Matrix = [Row]

mapMatrix :: Matrix -> Vector -> Vector
mapMatrix rows v = [sum (zipWith (*) row v) | row <- rows]

exampleA = [[1,1,0], [0,1,1], [1,0,1]] :: Matrix
exampleb = [2,3,4] :: Vector
--mapMatrix exampleA x == exampleb

gauss :: Matrix -> Vector -> Vector
gauss a b = x
   where
     b' = map (\y -> [y]) b
     a' = zipWith (++) a b'    -- combine with right-hand side
     x  = resubstitute $ triangular a'

triangular :: Matrix -> Matrix
triangular [] = []
triangular m  = row:(triangular rows')
    where
    (row:rows) = rotatePivot m    -- see discussion below
    rows' = map f rows
    f bs
        | (head bs) == 0 = drop 1 bs
        | otherwise      = drop 1 $ zipWith (-) (map (*c) bs) row
        where
        c = (head row)/(head bs)    -- suitable multiple

rotatePivot :: Matrix -> Matrix
rotatePivot (row:rows)
    | (head row) /= 0 = (row:rows)
    | otherwise       = rotatePivot (rows ++ [row])

resubstitute :: Matrix -> Vector
resubstitute = reverse . resubstitute' . reverse . map reverse

resubstitute' :: Matrix -> Vector
resubstitute' [] = []
resubstitute' (row:rows) = x:(resubstitute' rows')
    where
      x     = (head row)/(last row)
      rows' = map substituteUnknown rows
      substituteUnknown (a1:(a2:as')) = ((a1-x*a2):as')

--let x = gauss exampleA exampleb
--let yyyy = mapMatrix exampleA x == exampleb
