-- ghci AE2.hs

import Data.List

-- IN-17
fib :: (Num a, Ord a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib a = fib (a-1) + fib (a-2)

-- IN-18
ackermann :: (Num a, Ord a, Num t, Ord t) => a -> t -> t
ackermann m n
    | m == 0           = n + 1
    | m > 0 && n == 0  = ackermann (m-1) 1
    | m > 0 && n > 0   = ackermann (m-1) (ackermann m (n-1))

-- FT-14
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct [] [] = 0
scalarProduct (x:x0) (y:y0) = x * y + scalarProduct x0 y0

-- FT-18
-- a)
seq22 :: Num a => Int -> [a]
seq22 n = 1:( take (n -2) (repeat 2)) ++ [1]

-- b)
seq42 :: Num a => Int -> [a]
seq42 n = 1:( take (n -2) (cycle [4 ,2])) ++ [1]

-- FT-19
-- A função f procura o segundo maior elemento de uma lista


-- LI-13
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt a x
    | a > length x  = (x, [])
    | otherwise     = (take a x, drop (length x - a) x)

-- LI-14
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (x:xs) = takeWhile( == x) (x:xs) : myGroup (dropWhile( == x) (x:xs))

-- LI-15
-- a)
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits a = myInits (init a) ++ [a]
-- b)
myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails a = [a] ++ myTails (tail a)

-- LI-16
-- a)
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- b)
myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] _ _ = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myZip3 xs ys zs

-- LI-17
differentFromNext :: Eq a => [a] -> [a]
differentFromNext [] = []
differentFromNext [a] = []
differentFromNext (x1:x2:xs)
    | x1 == x2   = differentFromNext (x2:xs)
    | otherwise  = x1 : differentFromNext (x2:xs)

-- LI-18
myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose ([]:_) = []
myTranspose a = map head (filter (not . null) a) : myTranspose (map tail (filter (not . null) a))

-- LI-20
mySubsequences :: [a] -> [[a]]
mySubsequences [] = [[]]
mySubsequences (x:xs) = mySubsequences xs ++ map (x:) (mySubsequences xs)

-- mySubsequences xs: The subsequences that don't include x.
-- map (x:) (mySubsequences xs): The subsequences that include x, obtained by prepending x to each subsequence in subs


-- LI-29
-- a) [1,2,3,4,5,2,3,4,5,6,0,1,2]
-- b) ["buy␣the␣car","buy␣the␣house","loan␣the␣car","loan␣the␣house"]
-- c) [-2,-1,0,1,2]
-- d) [-4,-7,-8,-4,-7,-8,-4,-7,-8,-4]
-- e) [5,10,15,20,25,30,35,40,45,50]
-- f) [(2,10), (3,11), (4,12)]
-- g) [[4],[4],[4,3],[5,4],[4,3,2]]

-- LI-31
differentFromNext2 :: Eq a => [a] -> [a]
differentFromNext2 a = [x | (x, y) <- zip a (tail a), x /= y]

-- LI-32
conseqPairs :: [a] -> [(a, a)]
conseqPairs a = [(x, y) | (x, y) <- zip a (tail a)]

-- LI-33
myZip3_2 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3_2 a b c = [(x, y, z) | (x,(y,z)) <- zip a (zip b c)]

-- LI-35
checkMod3ThenOdd :: Integral a => [a] -> Bool
checkMod3ThenOdd a = and[mod x 2 == 1| x <- a, mod x 3 == 0]

-- LI-36
repeatNTimes :: Int -> [b] -> [b]
repeatNTimes n a = [x | x <- a, _ <- [1..n]]

-- LI-39
myPermutations :: Eq a => [a] -> [[a]]
myPermutations [] = [[]]
myPermutations a = [h:t | h <- a, t <- myPermutations (delete h a)]