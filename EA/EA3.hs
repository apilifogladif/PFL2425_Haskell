-- ghci AE3.hs

import Data.Char (isAlpha)

-- HO-1
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- HO-3
applyN :: (b -> b) -> Integer -> b -> b
applyN f 0 x = x
applyN f n x = applyN f (n - 1) (f x)

cipher :: Integer -> String -> String
cipher n [] = []
cipher n (x:xs)
   | isAlpha x = applyN succ n x : cipher n xs
   | otherwise = x : cipher n xs

-- HO-4
-- f g b a = g a (g a b)
-- The function f receives g, which takes an a and a b and returns a b. By applying g twice, it guarantees that the output type matches the second argument of f (b), thus satisfying the type signature (a -> b -> b) -> b -> a -> b.

-- HO-7
sortByCond :: [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond (x:xs) f = sortByCond lesser f ++ [x] ++ sortByCond greater f
  where
    lesser  = filter (`f` x) xs
    greater = filter (not . (`f` x)) xs

----------

-- HO-8
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = (\x y -> f y x)

----------

-- HO-10
myUncurry :: (Int -> Int -> Int) -> (Int, Int) -> Int
myUncurry f (x, y) = f x y

----------

-- HO-13
-- a) [[1,1,1],[2,2,2],[3,3,3]]
-- b) [0,0,1,1,1,2,2,2,3,3]
-- c) [4,16,36,64,100,...]
-- d) 220
-- e) [53.0,21.5,11.0,5.75,2.6]
-- f) [1,3,9,27,81]
-- g) 26

-- HO-14
orderedTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
orderedTriples a = filter (\(x, y, z) -> x <= y && y <= z) a

-- HO-15
-- HO-16
-- HO-17
-- HO-18
-- HO-22
-- HO-23

----------

-- HO-27
-- HO-29

----------

-- HO-32
-- HO-33
-- HO-35
-- HO-37
-- HO-40
-- HO-42
-- HO-43

----------

-- HO-47
-- HO-48
-- HO-49
-- HO-50
-- HO-51
-- HO-52
-- HO-53