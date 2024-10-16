-- ghci exercises2.hs
-- TODO: 1, 2, 3, 4, 7, 8, 10

import Data.Char (isSpace)

-- 1. Mostre como a lista em compreensão [f x | x ← xs, p x] se pode escrever
-- como combinação das funções de ordem superior map e filter.
-- map f (filter p xs)

-- 2. Usando foldl , defina uma função dec2int :: [Int] → Int que converte uma
-- lista de dígitos decimais num inteiro. Exemplo: dec2int [2, 3, 4, 5] = 2345.

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0


-- 3. A função zipWith :: (a → b → c) → [a] → [b] → [c] do prelúdio-padrão
--  é uma variante de zip cujo primeiro argumento é uma função usada para com-
--  binar cada par de elementos. Podemos definir zipWith usando uma lista em
--  compreensão:
--     zipWith f xs ys = [f x y | (x, y) ← zip xs ys]
--  Escreva uma definição recursiva de zipWith.

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- 4. Mostre que pode definir função isort :: Ord a ⇒ [a] → [a] para ordenar
-- uma lista pelo método de inserção (ver a Folha 3) usando foldr e insert.

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

myIsort :: Ord a => [a] -> [a]
myIsort = foldr insert []

-- 7. Sem consultar a especificação do Haskell 98, escreva definições não-recursivas
--   das seguintes funções do prelúdio-padrão:
-- a) (++ ) :: [a] → [a] → [a], usando foldr;
(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

-- b) concat :: [[a]] → [a], usando foldr ;
concat :: [[a]] -> [a]
concat  = foldr (Main.++) []

-- c) reverse :: [a] → [a], usando foldr ;
reverseR :: [a] -> [a]
reverseR = foldr (\x acc -> acc Prelude.++ [x]) [] 

-- d) reverse :: [a] → [a], usando foldl ;
reverseL :: [a] -> [a]
reverseL = foldl (\acc x -> x : acc) []

-- e) elem :: Eq a ⇒ a → [a] → Bool , usando any.
elem :: Eq a => a -> [a] -> Bool
elem x = any (== x)

-- 8. Pretende-se que resolva este exercício sem usar words e unwords do prelúdio-
--   padrão (pois words = palavras e unwords = despalavras).
-- a) Escreva uma definição da função palavras :: String → [String] que decom-
--   põe uma linha de texto em palavras delimitadas por um ou mais espaços.
--   Exemplo: palavras “Abra- ca- drabra!” = [“Abra-”, “ca-”, “dabra!”].
palavras :: String -> [String]
palavras [] = []
palavras a
    | isSpace (head a)  =  palavras (tail a)
    | otherwise         = takeWhile (not . isSpace) a : palavras (dropWhile(not . isSpace) a)

-- b) Escreva uma definição da função despalavras :: [String] → String que
--   concatena uma lista de palavras juntando um espaço entre cada uma.
--   Note que despalavras não é a função inversa de palavras; encontre um
--   contra-exemplo que justifique esta afirmação.
despalavras :: [String] -> String
despalavras [] = []
despalavras a = foldr (\x y -> x Prelude.++ " " Prelude.++ y) [] a

-- 10. Usando a definição da lista infinita primos apresentada nas aulas teóricas,
--   escreva uma função factores :: Int → [Int] para factorizar um inteiro positivo
--   em primos. Exemplo: factores 100 = [2, 2, 5, 5] porque 100 = 2 × 2 × 5 × 5.