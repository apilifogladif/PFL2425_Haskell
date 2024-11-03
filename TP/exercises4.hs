-- ghci exercises4.hs

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

-- 4.1
sumTree :: Num a => Tree a -> a
sumTree Empty = 0
sumTree (Node v esq dir) = v + sumTree esq + sumTree dir

-- 4.2.
listRev :: Tree a -> [a]
listRev Empty = []
listRev (Node x esq dir) = listRev dir ++ [x] ++ listRev esq

-- 4.3.
list :: Tree a -> [a]
list Empty = []
list (Node x esq dir) = list esq ++ [x] ++ list dir

nivel :: Int -> Tree a -> [a]
nivel _ Empty = []
nivel 0 tree = list tree
nivel n (Node x esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

-- 4.4.

-- 4.5.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node x esq dir) = Node (f x) (mapTree f esq) (mapTree f dir)

-- 4.6. 
-- a)
mais_dir :: Tree a -> a
mais_dir Empty = error "Árvore vazia não possui um elemento mais à direita."
mais_dir (Node x _ Empty) = x
mais_dir (Node x esq dir) = mais_dir dir
