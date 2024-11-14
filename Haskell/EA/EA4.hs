-- ghci AE4.hs

import Data.Char (ord, chr)
import System.Win32 (COORD(xPos, yPos))

-- LI-2
-- a) [5, -2, -9, -16]
-- b) "adgjmpsvy"
-- c) [(1,1), (2,3), (3,5), (4,7), (5,9), (6,11), (7,13), (8,15), (9,17), (10, 19), ...]
-- d) [10,12,14,16,18,20,22,24,26,28,...]
-- e) [1,2,3,4,5,1,2,3,4,5,...]

----------

-- LI-10
myCycle :: [a] -> [a]
myCycle a = a ++ myCycle a

----------

-- LI-34
dropN :: [a] -> Int -> [a]
dropN a 0 = a
dropN l n = [x | (x,p) <- zip l [1..] , mod p n /= 0]

-- LI-37
myCycle_2 :: [a] -> [a]
myCycle_2 a = [x | _ <- [1 ..], x <- a]

-- LI-38
-- a)
myE :: Int -> Double
myE n = sum [1 / fromIntegral (product [1..k]) | k <- [0 .. n-1]]

-- b)
myPi :: Int -> Double
myPi n = sqrt (12 * sum (take n [((-1) ** k) / (k + 1) ^ 2 | k <- [0..]]) )

-- LI-40
-- a) o exercicio está mal
-- b) fib = 1:1:[fst + snd | (fst, snd)<- zip fib (tail fib)]

-- LI-41
-- a)
cipher :: Int -> String -> String
cipher n s = [snd (cipherPairs n !! (ord x - ord 'a')) | x <- s]

cipherPairs :: Int -> [(Char, Char)]
cipherPairs n = zip ['a'..'z'] shiftedAlphabet
            where 
                shiftedAlphabet = [chr (ord c + n) | c <- ['a'..'z']]

-- LI-42
-- infinite list of primes

----------

-- HO-19
myIterate :: (Num a) => (a -> a) -> a -> [a]
myIterate f n = n:[f x | x <- myIterate f n]

-- HO-24
-- a) The function outputs all of the Hamming numbers. However, it does not
-- return them in order: it “gets stuck” in an infinite loop only producing the powers
-- of 2. Therefore, computations that test whether a relatively small number (for
-- instance, smaller than 100) is a Hamming number may not terminate.

----------

-- UT-3
type Pair a = (a,a)

-- a)
type Relation a = [Pair a]

-- b)
isReflexive :: (Eq a) => Relation a -> Bool
isReflexive r = isReflexiveAux r r

isReflexiveAux :: (Eq a) => Relation a -> Relation a -> Bool
isReflexiveAux [] _ = True
isReflexiveAux ((x, y):xs) r
        | (y, x) `elem` r  = isReflexiveAux xs r      
        | otherwise = False

-- c)
isTransitive :: (Eq a) => Relation a -> Bool
isTransitive r = isTransitiveAux r r r

isTransitiveAux :: (Eq a) => Relation a -> Relation a -> Relation a -> Bool
isTransitiveAux [] _ _= True
isTransitiveAux ((x, y):xs) [] r = isTransitiveAux xs r r
isTransitiveAux ((x, y1):xs) ((y2, z):zs) r
        | y1 == y2 && (x, z) `elem` r = isTransitiveAux ((x, y1):xs) zs r
        | y1 /= y2 = isTransitiveAux ((x, y1):xs) zs r
        | otherwise = False

-- UT-4
type Pos = (Integer, Integer)

queenMoves :: Pos -> [Pos]
queenMoves init = horizontal ++ vertical ++ diagonals
        where
            horizontal = [(x, snd init) | x <- [1..8], x /= fst init]
            vertical = [(fst init, y) | y <- [1..8], y /= snd init]
            diagonals = [(x,y) | dir <- [-1,1], offset <- [-7,-6.. -1] ++ [1..7] ,
                    let x = fst init + offset , let y = snd init + offset *dir ,
                        x >= 1, x <= 8, y >= 1, y <= 8]


----------

-- UT-6
data Shape = Circle Double Double Double | Rectangle Double Double Double Double

perimeter :: Shape -> Double
perimeter ( Circle _ _ r) = 2 * pi * r
perimeter ( Rectangle x1 y1 x2 y2) = 2 * abs(x1 - x2) + 2 * abs(y1 - y2)

-- UT-7
type HashMap k v = [(k,v)]

-- a)
mylookup :: (Eq v) => v -> HashMap k v -> Maybe k
mylookup _ [] = Nothing
mylookup s (x:xs) = if snd x == s then Just (fst x) else mylookup s xs 

-- b)
areEqual :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
areEqual map1 map2 = all (\(x, y) -> lookup x map2 == Just y) map1 
                     && all (\(x, y) -> lookup x map1 == Just y) map2

-- c)
ceilingKey :: (Eq k, Ord k) => k -> HashMap k v -> Maybe k
ceilingKey k m = ceilingKeyAux k m Nothing

ceilingKeyAux :: (Eq k, Ord k) => k -> HashMap k v -> Maybe k -> Maybe k
ceilingKeyAux _ [] acc = acc
ceilingKeyAux k ((k1 ,v1):xs) Nothing
        | k1 >= k = ceilingKeyAux k xs (Just k1)
        | otherwise = ceilingKeyAux k xs Nothing
ceilingKeyAux k ((k1 ,v1):xs) (Just kC)
        | k1 >= k && k1 < kC = ceilingKeyAux k xs (Just k1)
        | otherwise = ceilingKeyAux k xs (Just kC)

-- UT-8
-- a)
data NestedType a = Elem a | List [NestedType a]

-- b)
flatten :: NestedType a -> [a]
flatten (Elem x)     = [x]               
flatten (List xs)    = concatMap flatten xs        

----------

-- UT-9
-- a)
data Country = Ct {name :: String, population :: Int, area :: Double, continent :: String} deriving (Show ,Eq)

-- b)
populationDensity :: Country -> Double
populationDensity c = fromIntegral (population (c)) / area(c)

-- c)
countContinent :: String -> [Country] -> Int
countContinent cont [] = 0
countContinent cont (x:xs) = (if continent x == cont then 1 else 0) + countContinent cont xs


