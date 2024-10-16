-- ghci AE1.hs

-- IN-6
half :: Fractional a => a -> a
half a = a / 2

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (a /= b)

cbrt :: Floating a => a -> a
cbrt a = a ** (1/3)

heron :: Floating a => a -> a -> a -> a
heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- IN-13
myF :: (Ord a, Num a, Integral b) => a -> b
myF 0 = 0
myF x = if x > 0 then 1 else -1

-- IN-16
mPower :: ( Fractional a, Integral t) => a -> t -> a
mPower _ 0 = 1
mPower m n = m * mPower m (n - 1)

-- FT-3
mySwap :: (b, a) -> (a, b)
mySwap (b, a) = (a, b)

mySwap_2 :: (b, a) -> (a, b)
mySwap_2 a = (snd a, fst a)

-- FT-4
distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (ax, ay) (bx, by) = sqrt((ax - bx) ** 2 + (ay - by) ** 2)

distance2_2 :: Floating a => (a, a) -> (a, a) -> a
distance2_2 a b = sqrt((fst a - fst b) ** 2 + (snd a - snd b) ** 2)

distanceInf :: (Num a, Ord a) => (a, a) -> (a, a) -> a
distanceInf (ax ,ay) (bx ,by) = max (abs(ax - bx)) (abs(ay - by))

distanceInf_2 :: (Num a, Ord a) => (a, a) -> (a, a) -> a
distanceInf_2 a b = max (abs(fst a - fst b)) (abs(snd a - snd b))

-- FT-9
-- a) []
-- b) [[1,2],[],[3],[4,5]]
-- c) [[1,2],[],[3],[4,5]]
-- d) [4,5]
-- e) 3
-- f) error -> supostamente cria a lista [[1,2],[3,4,5],[6,7],8], o que não é possível dado que 8 não é do tipo List
-- g) [1,2]
-- h) [[],[],[]]
-- i) ["abc","","dce"]
-- j) [[],[2],[3]]
-- k) 12
-- l) True

-- FT-10
-- a) Function f returns a pair with the third element of the input list l and the sublist of l starting at the fourth element.
-- b)
myFunctionF :: [a] -> (a,[a])
myFunctionF a = (a !! 2, drop 3 a)

-- FT-11
-- a)
evaluateLength :: [a] -> String
evaluateLength a
    | length a < 2 = "short"
    | length a > 3 = "long"
    | otherwise    = "medium-sized"

-- FT-21
-- a) Write a valid type declaration for the following expressions without using type variables.
--   i) zip [1,2] "abc"
--     (Integer, Char)
--   ii) [[1],[2]]
--     [[Integer], [Integer]]
--   iii) [succ 'a']
--     [Char]
--   iv) [1,2,3,4,5.5]
--     [Floating]
--   v) [1,2] == [1,2]
--     Bool
--   vi) zip (zip "abc""abc")"abc"
--     [((Char,Char),Char)]
-- b) Write the most general type declaration for the previous expressions.
--   i) zip [1,2] "abc"
--     Num a => [(a, Char)]
--   ii) [[1],[2]]
--     Num a => [[a]]
--   iii) [succ ’a’]
--     [Char]
--   iv) [1,2,3,4,5.5]
--     Num a => [a] XXX (Fractional a => [a])
--   v) [1,2] == [1,2]
--     Bool
--   vi) zip (zip "abc""abc")"abc"
--     [((Char,Char),Char)]

