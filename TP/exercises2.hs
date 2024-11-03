-- ghci exercises2.hs
-- TODO: 2, 3, 4, 6, 7, 12, 15, 20, 21, 24

import Data.Char (isUpper, isLower)

-- 2. Escreva uma definição da função intersperse ∶∶ a → [a] → [a] do mó-
--    dulo Data.List que intercala um valor entre os elementos duma lista. Exemplo:
--    intersperse ’-’ “banana” = “b-a-n-a-n-a”.

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse y (x:xs) = x : y : intersperse y xs

-- run: intersperse '-' "banana"


-- 3 . O algoritmo de Euclides para calcular o máximo divisor comum de dois
--     inteiros a, b pode ser expresso de forma recursiva:
--        mdc(a, b) =  a , se b = 0
--                  =  mdc(b, a mod b) , caso contrário
--     Traduza esta definição recursiva para uma função mdc ∶∶ Integer → Integer →
--     Integer .

gdc :: Integer -> Integer -> Integer 
gdc x 0 = x
gdc x y = gdc y (x `mod` y)

-- 4. Ordenação de listas pelo método de inserção.
--    (a) Escreva definição recursiva da função insert ∶∶ Ord a ⇒ a → [a] → [a] da
--       biblioteca List para inserir um elemento numa lista ordenada na posição
--       correcta de forma a manter a ordenação. Exemplo: insert 2 [0, 1, 3, 5] =
--       [0, 1, 2, 3, 5].

myinsert :: Ord a => a -> [a] -> [a]
myinsert y [] = [y]
myinsert y (x:xs)
    | y <= x    = y : x : xs
    | otherwise = x : myinsert y xs

--    (b) Usando a função insert, escreva uma definição também recursiva da função
--       isort ∶∶ Ord a ⇒ [a] → [a] que implementa ordenação pelo método de
--       inserção:
--       • a lista vazia já está ordenada;
--       • para ordenar uma lista não vazia, recursivamente ordenamos a cauda
--       e inserimos a cabeça na posição correcta.

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = myinsert x (isort xs)

-- 6. Usando uma lista em compreensão, escreva uma expressão para calcular a
--    soma 1^2 + 2^2 + ⋯ + 100^2 dos quadrados dos inteiros de 1 a 100.

squareSum :: Integer -> Integer
squareSum 0 = 0
squareSum x = sum [i ^ 2 | i <- [1 .. x]]

-- 7. A constante matemática π pode ser aproximada usando expansão em séries
--    (i.e. somas infinitas), como por exemplo:
--    π/4 = 1 - 1/3 + 1/5 + ... + ((-1) ^ n)/(2n + 1) + ...
--    (a) Escreva uma função aprox ∶∶ Int → Double para aproximar π somando em
--       n parcelas da série acima (onde n é o argumento da função).

aprox :: Int -> Double
aprox x = (1 + sum [((-1) ^ i) / fromIntegral (2 * i + 1) | i <- [1 .. x]]) * 4

--    (b) A série anterior converge muito lentamente, pelo são necessário muitos
--       termos para obter uma boa aproximação; escreva uma outra função aprox ′
--       usando a seguinte expansão para π^2:
--       π^2/12 = 1 - 1/4 + 1/9 - ... + ((-1) ^ k)/(k + 1)^2 + ...
--       Compare os resultados obtidos somado 10, 100 e 1000 termos com a apro-
--       ximação pi pré-definida no prelúdio-padrão.

aprox2 :: Int -> Double
aprox2 x = (1 + sum [((-1) ^ i) / fromIntegral ((i + 1) ^2) | i <- [1 .. x]]) * 12

-- 12. Defina uma função primo ∶∶ Integer → Bool que testa primalidade: n é
--     primo se tem exactamente dois divisores, a saber, 1 e n. Sugestão: utilize a
--     função do exercício 2.9 para obter a lista dos divisores próprios.

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], n `mod` x == 0]

primo :: Integer -> Bool
primo n = divisores n == [1, n]

-- 15. A cifra de César é um dos métodos mais simples para codificar um
--    texto: cada letra é substituida pela que dista k posições à frente no alfabeto; se
--    ultrapassar a letra Z, volta à letra A. Por exemplo, para k = 3, a substituição
--    efectuada é
--       A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
--       D E F G H I J K L M N O P Q R S T U V W X Y Z A B C
--    e o texto “ATAQUE DE MADRUGADA” é transformado em “DWDTXH GH
--    PDGUXJDGD”.
--    Escreva uma função cifrar ∶∶ Int → String → String para cifrar uma cadeia
--    de caracteres usando um deslocamento dado. Note que cifrar (−n) é a fun-
--    ção inversa de cifrar n, pelo que a mesma função pode servir para codificar e
--    descodificar.

cifrar :: Int -> String -> String
cifrar n [] = []
cifrar n (x:xs)
    | isUpper x = toEnum ((fromEnum x - base + n) `mod` 26 + base) : cifrar n xs
    | isLower x = toEnum ((fromEnum x - baseLower + n) `mod` 26 + baseLower) : cifrar n xs
    | otherwise = x : cifrar n xs
  where
    base = fromEnum 'A'
    baseLower = fromEnum 'a'

-- 20. Escreva uma definição da função transpose ∶∶ [[a]] → [[a]] do módulo
--     Data.List para obter a transposta de uma matriz (isto é, a matriz simétrica
--     em relação à diagonal principal); a matriz dada e o resultante são representadas
--     como listas de linhas. Exemplo: transpose [[1, 2, 3], [4, 5, 6]] = [[1, 4], [2, 5], [3, 6]]

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- 21. Escreva uma definição da função algarismos ∶∶ Int → [Int] que obtém
--     os algarismos decimais de um inteiro positivo. Exemplo: algarismos 12345 =
--     [1, 2, 3, 4, 5].
--     Sugestão: Pode obter o algarismo das unidades usando o resto da divisão
--     por 10 e prosseguir recursivamente com o quociente da divisão. Começe por
--     definir uma função auxiliar que obtem os algarismos pela ordem inversa, i.e.
--     algarismosRev 12345 = [5, 4, 3, 2, 1].

algarismos :: Int -> [Int]
algarismos 0 = []
algarismos x = myinsert (x `mod` 10) (algarismos (x `div` 10))

-- 24. Ordenação de listas pelo método merge sort.
--     (a) Escreva uma definição recursiva da função merge ∶∶ Ord a ⇒ [a] → [a] →
--         [a] para juntar duas listas ordenadas numa só mantendo a ordenação.
--         Exemplo: merge [3, 5, 7] [1, 2, 4, 6] = [1, 2, 3, 4, 5, 6, 7].


--     (b) Usando a função merge, escreva uma definição recursiva da função msort ∶∶
--         Ord a ⇒ [a] → [a] que implementa o método merge sort:
--         • uma lista vazia ou com um só elemento já está ordenada;
--         • para ordenar uma lista com dois ou mais elementos, partimos em
--         duas metades, recursivamente ordenamos as duas parte e juntamos
--         os resultados usando merge.
--         Sugestão: começe por definir uma função metades ∶∶ [a] → ([a], [a]) para
--         partir uma lista em duas metades (ver a Folha 1).

