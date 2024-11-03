-- ghci exercises1.hs

-- 1.3.
metades :: [a] -> ([a], [a])
metades [] = ([], [])
metades xs = (take ((length xs + 1) `div` 2) xs, drop ((length xs + 1) `div` 2) xs)

-- 1.4.a.
lastElement :: [a] -> a
lastElement xs = head (reverse xs)

-- 1.4.b.
initOneWay :: [a] -> [a]
initOneWay xs = take (length xs - 1) xs

initAnotherWay :: [a] -> [a]
initAnotherWay xs = reverse (tail (reverse xs))

-- 1.7.
-- a) [char]
-- b) (char, char, char)
-- c) [(bool, char)]
-- d) ([bool], [char])
-- e) [[a] -> [a]]
-- f) [a -> a]

-- 1.8
-- a) [a] -> a
-- b) (a, a) -> (a, a)
-- c) a -> b -> (a, b)
-- d) Num a => a -> a
-- e) Fractional => a -> a
-- f) a -> a
-- g) a -> a -> a -> Bool
-- h) a -> Bool
-- i) (a -> a) -> a -> a

-- 1.12
xor :: Bool -> Bool -> Bool
xor a b = not(a == b)

-- 1.16
-- trata dos 2 últimos digitos, ou seja, 0 - 99
converte2 :: Int -> String
converte2 n
    | n < 0 = "Número inválido"
    | n == 0 = "zero"
    | n < 10 = unidades !! n
    | n < 20 = teens !! (n - 10)
    | n < 100 = let (d, u) = divMod n 10
                 in if u == 0 
                    then dezenas !! d
                    else dezenas !! d ++ " e " ++ unidades !! u
    where
        unidades = ["zero", "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove"]
        teens = ["dez", "onze", "doze", "treze", "quatorze", "quinze", "dezesseis", "dezessete", "dezoito", "dezenove"]
        dezenas = ["", "", "vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]

-- trata do terceiro dígito, ou seja 100 - 999
converte3 :: Int -> String
converte3 n
    | n < 100 = converte2 n
    | otherwise = let (c, r) = divMod n 100
                  in if r == 0
                     then centenas !! c
                     else centenas !! c ++ " " ++ converte2 r
    where
        centenas = ["", "cem", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"]

-- divide o número em 2 partes (antes do mil e depois do mil)
converte :: Int -> String
converte n
    | n < 0 = "Número inválido"
    | n >= 1000000 = "Número fora do intervalo"
    | n < 1000 = converte3 n
    | otherwise = let (m, r) = divMod n 1000
                  in if r == 0
                     then converte3 m ++ " mil"
                     else converte3 m ++ " mil " ++ converte3 r

    
    