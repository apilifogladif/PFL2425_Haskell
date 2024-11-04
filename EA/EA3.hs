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
myMap :: (a -> a) -> [a] -> [a]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

-- HO-16
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
        | f x         = x : myFilter f xs
        | otherwise   = myFilter f xs

-- HO-17
-- a)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (x:xs)
      | f x        = x : myTakeWhile f xs
      | otherwise  = []

-- b)
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f (x:xs)
      | f x        = myDropWhile f xs
      | otherwise  = x:xs

-- HO-18
-- a) using recursion
myAllA :: (a -> Bool) -> [a] -> Bool
myAllA f [] = True
myAllA f (x:xs)
      | f x  = myAllA f xs
      | otherwise = False

-- b) using list comprehension and the 'and' function, without recursion
myAllB :: (a -> Bool) -> [a] -> Bool
myAllB f (x:xs) = and [f y | y <- x:xs]

-- c) only using 'map
myAllC :: (a -> Bool) -> [a] -> Bool
myAllC f (x:xs) = and (map f (x:xs))

-- d) only using 'any'
myAllD :: (a -> Bool) -> [a] -> Bool
myAllD f (x:xs) = not (any (not . f) (x:xs))

-- HO-22
countVowels :: String -> Int
countVowels s = sum (map isVowel s)

isVowel :: Char -> Int
isVowel v 
    | v == 'a'  = 1
    | v == 'e'  = 1 
    | v == 'i'  = 1
    | v == 'o'  = 1
    | v == 'u'  = 1
    | otherwise = 0

-- HO-23
-- funtion f computes the largest common prefix of the input lists x and y.

----------

-- HO-27
-- The definition myAny = or . map is incorrect because map is missing the predicate function argument, causing a type mismatch. The correct approach is to define myAny with an additional argument for the predicate, like this:
-- myAny p = or . map p

-- HO-29
-- a) True
-- b) [6, 20, 64, 2]
-- c) [[4,5,6], [12,11,10]]
-- d) [8,8]

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