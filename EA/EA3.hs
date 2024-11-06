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
myMapB :: (a -> a) -> [a] -> [a]
myMapB f (x:xs) = foldr (\x acc -> (f x):acc) [] (x:xs)

-- HO-33
largePairs :: (Num a, Ord a) => a -> [(a, a)] -> [(a, a)]
largePairs m = foldr (\(x, y) acc -> if (x + y) >= m then (x, y) : acc else acc) []

-- HO-35
separateSingleDigits :: [Int] -> ([Int], [Int])
separateSingleDigits = foldr (\x (single, rest) -> if x < 10 then (x : single, rest) else (single, x : rest)) ([], [])

-- HO-37
-- a)
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x ( myFoldr f acc xs)

-- b)
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- HO-40
-- a)
myScanr  :: (a -> b -> b) -> b -> [a] -> [b]
myScanr _ acc [] = [acc]
myScanr f acc (x:xs) = f x (head n) : n
      where n = myScanr f acc xs

-- b) using 'foldr' instead of recursion
myScanr' :: (a -> b -> b) -> b -> [a] -> [b]
myScanr' f acc xs = foldr step [acc] xs
  where step x accs = f x (head accs) : accs

-- HO-42
myFoldlB :: (a -> b -> a) -> a -> [b] -> a
myFoldlB f acc l =
  foldr (\x newAcc z -> f (newAcc z) x) id l acc

-- HO-43
kadane :: (Ord a, Num a) => [a] -> a
kadane l =  maximum (scanl (\ acc x -> max x (acc + x)) 0 l)

----------

-- HO-47
f :: [a] -> [a] -> [a]
f = flip (foldr (:))

-- HO-48
myLastA :: [a] -> a
myLastA = head . reverse

myLastB :: [a] -> a
myLastB = foldl (flip const) (error "empty list")

-- HO-49
-- a)
countLetters :: String -> Int
countLetters = length . concat . words

--b)
countFirst :: String -> Int
countFirst = length . head . words

-- HO-50
-- a)
myReverseA :: [a] -> [a]
myReverseA = foldr (flip (++) . (:[])) []

-- b)
myReverseB :: [a] -> [a]
myReverseB = foldl (flip (:)) []

-- HO-51
-- a)
mySum :: (Num a) => [a] -> a
mySum = foldr (+) 0

-- b)
myProduct :: (Num a) => [a] -> a
myProduct = foldr (*) 1

-- c)
myLength :: (Num b) => [a] -> b
myLength = sum . map (const 1)

-- HO-52
extractDigits :: Integer -> [Integer]
extractDigits = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

-- HO-53
kadaneA :: (Ord a, Num a) => [a] -> a
kadaneA = maximum . scanl ((+) .( max 0)) 0
