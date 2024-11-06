-- Teste 21/22

-- 1. Escreva uma definição da função maxpos :: [Int] -> Int que calcula o máximo dos inteiros
-- positivos numa lista; se a lista não tiver números positivos o resultado deve ser 0.
-- Exemplos:
-- maxpos [1,2,3,4,5] == 5
-- maxpos [-1,-2,-3,4,-5] == 4
-- maxpos [2] == 2
-- maxpos [] == 0

maxpos :: [Int] -> Int
maxpos l = maxposAux l 0

maxposAux :: [Int] -> Int -> Int
maxposAux [] m = m
maxposAux (x:xs) m
    | x > m     = maxposAux xs x
    | otherwise = maxposAux xs m

-- Escreva uma definição da função dups :: [a] -> [a] que duplica os valores em posições alternadas duma
-- lista (isto é, duplica o primeiro elemento, não duplica o segundo, duplica o terceiro, e assim sucessivamente).
-- Exemplos:
-- dups "abcdef" == "aabccdeef"
-- dups [0,1,2,3,4] == [0,0,1,2,2,3,4,4]
-- dups [] == []

dups :: [a] -> [a]
dups l = dupsAux l 1 []

dupsAux :: [a] -> Int -> [a] -> [a]
dupsAux [] _ f = f
dupsAux (x:xs) n f
    | n `mod` 2 == 0 = dupsAux xs (n + 1) (f ++ [x])
    | otherwise      = dupsAux xs (n + 1) (f ++ [x, x])

-- 3. A linguagem dos Ps é um jogo de palavras em que duplicamos cada vogal (letras 'a', 'e', 'i', 'o', 'u') e
-- colocamos um 'p' entre elas; todos os outros carateres e letras ficam inalterados. Assim, por exemplo, a frase
-- "ola, mundo!" fica "opolapa, mupundopo!". Escreva uma definição da função transforma :: String -> String
-- que transforma uma frase para a linguagem dos Ps. Para simplificar, assuma que a frase não contém letras
-- maiúsculas nem acentuadas.
-- Exemplos:
-- transforma "ola, mundo!" == "opolapa, mupundopo!"
-- transforma "4 gatos e 3 ratos" == "4 gapatopos epe 3 rapatopos"

transforma :: String -> String
transforma [] = []
transforma (x:xs) 
    | x `elem` ['a', 'e', 'i', 'o', 'u']  = [x, 'p', x] ++ transforma xs
    | otherwise = x:transforma xs

type Vector = [Int]
type Matriz = [[Int]]

-- 4. Defina a função transposta :: Matriz -> Matriz, que calcula a matriz transposta.
-- Exemplo:
-- transposta [[1,2], [3,4]] = [[1,3], [2,4]]
-- transposta [[1,2,3], [4,5,6]] = [[1,4], [2,5], [3,6]]

transposta :: Matriz -> Matriz
transposta ([]:_) = []
transposta matriz = map head matriz : transposta (map tail matriz) 

-- 5. Defina a função prodInterno :: Vector -> Vector -> Int, que calcula o produto interno de dois vectores
-- (a soma dos produtos de elementos com o mesmo índice). Assume-se que ambos os vetores têm o mesmo comprimento.
-- Exemplo:
-- prodInterno [1,2,3] [4,3,2] = 16

prodInterno :: Vector -> Vector -> Int
prodInterno [] [] = 0
prodInterno (x:xs) (y:ys) = x * y + prodInterno xs ys

-- 6. Defina a função prodMat :: Matriz -> Matriz -> Matriz, que calcula o produto de duas matrizes.
-- Exemplo:
-- prodMat [[1,2], [3,4]] [[2,3], [2,4]] = [[6,11], [14,25]]
-- (note que 6 = 1*2 + 2*2 e 11 = 1*3 + 2*4)

prodMat :: Matriz -> Matriz -> Matriz
prodMat [] _ = []
prodMat (x:xs) m2 = prodMatAux x m2 : prodMat xs m2

prodMatAux :: Vector -> Matriz -> Vector
prodMatAux v ([]:_) = []
prodMatAux v m2 = sum [x * y | (x, y) <- zip v (map head m2)] : prodMatAux v (map tail m2)

data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)

-- 7. Escreva uma definição da função alturas :: Arv a -> Arv Int que transforma uma árvore binária noutra com
-- a mesma estrutura mas em que o valor de cada nó é dado pela sua altura (isto é, o maior comprimento dum
-- caminho desse nó até uma folha).
-- Exemplo (com uma árvore de strings):
-- alturas (N "joão" (N "abel" F F) (N "pedro" F F)) = N 2 (N 1 F F) (N 1 F F)

alturas :: Arv a -> Arv Int
alturas arv = alturasAux arv (altura arv)

alturasAux :: Arv a -> Int -> Arv Int
alturasAux (N nome F F) n = N n F F
alturasAux (N nome arv1 arv2) n = N n (alturasAux arv1 (n-1)) (alturasAux arv2 (n-1))

altura :: Arv a -> Int
altura (N nome F F) = 1
altura (N nome arv1 arv2) = 1 + max (altura arv1) (altura arv2) 

-- 8. Defina uma outra função equilibrada :: Arv a -> Bool que exprima a condição duma árvore ser equilibrada,
-- isto é, as alturas das sub-árvores de cada nó diferem no máximo de 1 unidade.
-- Exemplo (com a mesma árvore):
-- equilibrada (N "joão" (N "abel" F F) (N "pedro" F F)) = True

equilibrada :: Arv a -> Bool
equilibrada (N nome F F) = True
equilibrada (N nome arv1 arv2) = (abs (altura arv1 - altura arv2) <= 1) && equilibrada arv1 && equilibrada arv2

-- Escreva a definição de uma função f, em Haskell, que tenha o tipo:
-- f :: (a -> b -> c) -> b -> a -> c
-- (nota: não complique - a função pretendida não é complicada!)

f :: (a -> b -> c) -> b -> a -> c
f function b a = function a b