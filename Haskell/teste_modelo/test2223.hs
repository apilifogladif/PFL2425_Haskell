-- Teste 22/23

-- The Protecting Friends for Life (PFL) Zoo houses many species of animals, some of which were considered extinct in the past!
-- Consider the Species type synonym, which represents an animal species as a tuple composed of the species name followed by its
-- population in the zoo.
type Species = (String, Int)
type Zoo = [Species]

-- 1. Implement isEndangered :: Species -> Bool, which receives a species and determines if it is endangered. A species is considered endangered
-- if there are 100 or less individuals in the zoo.

isEndangered :: Species -> Bool
isEndangered species = snd species <= 100

-- 2. Implement updateSpecies :: Species -> Int -> Species, which, given a Species and an amount of newborn babies, returns a new instance of
-- Species with the updated population.

updateSpecies :: Species -> Int -> Species
updateSpecies species n = (fst species, snd species + n)

-- Note: In the following three exercises, there are constraints over the programming techniques. Solutions that do not follow the constraints
-- will receive at most 25% of the question score.

-- 3. Implement filterSpecies :: Zoo -> (Species -> Bool) -> Zoo, which, given the list species of a zoo and a predicate (i.e. a function that performs
-- a test on each species), returns the sublist of species that satisfy the predicate. The order of the species in the result must be the same as in
-- the input.
-- Constraint: You must solve this exercise using recursion. List comprehensions and higher-order functions (such as map and filter) are
-- prohibited.

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (species : others) f
    | f species  = species : filterSpecies others f
    | otherwise  = filterSpecies others f

-- 4. Implement countAnimals :: Zoo -> Int, which, given the list of species of a zoo, counts the total population of the zoo.
-- Constraint: You must solve this exercise using higher-order functions. Recursion and list comprehensions are prohibited.

countAnimals :: Zoo -> Int
countAnimals zoo = sum (map snd zoo)

-- 5. Implement substring :: (Integral a) => String -> a -> a -> String, which returns the substring of a given string between an initial and final
-- index (the character on the final index should also be included in the result; both indices are within bounds). Consider that the indices start
-- at 0.
-- Constraint: You must solve this exercise using a list comprehension. Recursion and higher-order functions are prohibited.
-- Use case example:
-- ghci> substring "arctic fox" 0 5
-- "arctic"

substring :: (Integral a) => String -> a -> a -> String
substring text i f = [text !! fromIntegral idx | idx <- [i..f]]

-- Note: In the following exercises, you are free to use the programming techniques you prefer.

-- 6. Implement hasSubstr :: String -> String -> Bool, which determines if the first string argument contains the second string argument (i.e. the
-- second argument is a substring of the first one).

hasSubstr :: String -> String -> Bool
hasSubstr str sub = hasSubstrAux str sub sub

hasSubstrAux :: String -> String -> String -> Bool
hasSubstrAux _ [] _ = True
hasSubstrAux [] _ _ = False
hasSubstrAux (x:xs) (y:ys) aux
    | x == y   = hasSubstrAux xs ys aux
    | otherwise = hasSubstrAux xs aux aux

-- 7. Implement sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo), which divides the species of the Zoo into a pair of sublists. The first sublist
-- stores all the species whose name contains the string argument, while the second list has the remaining species. The order of the species in
-- each list of the resulting pair must match the input list.
-- Use case example:
-- ghci> sortSpeciesWithSubstr [("arctic fox",30), ("polar bear",5), ("arctic wolf",12)] "arctic" 
-- ([("arctic fox",30),("arctic wolf",12)], [("polar bear",5)])

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr zoo str = sortSpeciesWithSubstrAux zoo str ([],[])

sortSpeciesWithSubstrAux :: Zoo -> String -> (Zoo, Zoo) -> (Zoo, Zoo)
sortSpeciesWithSubstrAux [] _ pair = pair
sortSpeciesWithSubstrAux (x:xs) str (zoo1, zoo2)
    | hasSubstr (fst x) str   = sortSpeciesWithSubstrAux xs str (zoo1 ++ [x], zoo2)
    | otherwise               = sortSpeciesWithSubstrAux xs str (zoo1, zoo2 ++ [x])

-- The rabbit population of the zoo increases every year. In year 0, there were 2 rabbits, while in year 1 there were 3 rabbits. In the following
-- years, the number of rabbits corresponds to the sum of the rabbit population of the two previous years.

-- 8. Implement rabbits :: (Integral a) => [a], which returns an infinite list with the rabbit population of each year (starting at year 0).
-- Use case example:
-- ghci> rabbits
-- [2,3,5,8 …]

rabbits :: (Integral a) => [a]
rabbits = 2:3:[x1 + x2 | (x1, x2) <- zip rab (tail rab)]
    where rab = rabbits

-- 9. Implement rabbitYears :: (Integral a) => a -> Int, which returns the number of years needed for the rabbit population to be greater or equal
-- to the input integral value.
-- Use case examples:
-- ghci> rabbitYears 2
-- 0
-- ghci> rabbitYears 6
-- 3

rabbitYears :: (Integral a) => a -> Int
rabbitYears n = length (takeWhile (<n) rabbits)


-- Consider a dendrogram as a binary tree where each path leads to a string. Each non-leaf node of the dendrogram specifies the horizontal
-- distance from the father node to each of the two child nodes. A father node is always at an equal horizontal distance from both its children.
data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram
myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

-- 10. Implement dendroWidth :: Dendrogram -> Int, which returns the width of a dendrogram (i.e., the horizontal distance between the leftmost
-- and rightmost leaf nodes).
-- Use case example:
-- ghci> dendroWidth myDendro
-- 13

dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf str) = 0
dendroWidth (Node den1 n den2) = 2*n + dendroWidthAux den1 1 + dendroWidthAux den2 2

dendroWidthAux :: Dendrogram -> Int -> Int
dendroWidthAux (Leaf str) dir = 0
dendroWidthAux (Node den1 n den2) dir
    | dir == 1  = n + dendroWidthAux den1 dir
    | otherwise = n + dendroWidthAux den2 dir

-- 11. Implement dendroInBounds :: Dendrogram -> Int -> [String], which returns the list of strings whose leaf nodes are up to a certain horizontal
-- distance from the root of the dendrogram. Any order of the output list will be accepted.
-- Use case examples:
-- ghci> dendroInBounds myDendro 5
-- ["cat","octopus"]
-- ghci> dendroInBounds myDendro 10
-- ["dog","cat","octopus"]

dendroInBounds :: Dendrogram -> Int -> [String]
dendroInBounds (Node den1 n den2) dist = dendroInBoundsAux den1 1 dist n ++ dendroInBoundsAux den2 2 dist n

dendroInBoundsAux :: Dendrogram -> Int -> Int -> Int -> [String]
dendroInBoundsAux (Leaf str) dir dist current = [str]
dendroInBoundsAux (Node den1 n den2) dir dist current
    | dir == 1 && current + n > dist  = dendroInBoundsAux den2 2 dist n
    | dir == 2 && current - n > dist  = dendroInBoundsAux den1 1 dist n
    | otherwise = dendroInBoundsAux den1 1 dist n ++ dendroInBoundsAux den2 2 dist n
