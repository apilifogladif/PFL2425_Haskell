import UtilsIO
import Stack
-- UT-11
checkParentheses :: String -> Bool
checkParentheses s = checkParenthesesAux s createEmptyStack

checkParenthesesAux :: String -> Stack Char -> Bool
checkParenthesesAux [] stack = isEmpty stack
checkParenthesesAux (x:xs) stack
    | x == '(' || x == '[' || x == '{' = checkParenthesesAux xs (push x stack)
    | x == ')' =
        not (isEmpty stack) && top stack == '(' && checkParenthesesAux xs (pop stack)
    | x == ']' =
        not (isEmpty stack) && top stack == '[' && checkParenthesesAux xs (pop stack)
    | x == '}' =
        not (isEmpty stack) && top stack == '{' && checkParenthesesAux xs (pop stack)
    | otherwise = checkParenthesesAux xs stack 

---

-- UT-12
data SyntaxTree a = Const a
                  | Unary (a -> a) ( SyntaxTree a)
                  | Binary (a -> a -> a) ( SyntaxTree a) ( SyntaxTree a)

-- UT-13
countConsts :: SyntaxTree a -> Int
countConsts (Const x) = 1
countConsts (Unary u tree) = countConsts tree
countConsts (Binary b tree1 tree2) = countConsts tree1 + countConsts tree2

-- UT-14
compute :: SyntaxTree a -> a
compute (Const x) = x
compute (Unary u tree) = u (compute tree)
compute (Binary b tree1 tree2) = b (compute tree1) (compute tree2)

---

-- UT-23
data AVLTree a = Empty
               | Node ( AVLTree a) a ( AVLTree a)
               deriving (Show ,Eq)

-- a)
height :: AVLTree a -> Int
height Empty = 0
height (Node tree1 x tree2) = 1 + max (height tree1) (height tree2)

-- b)
balance :: AVLTree a -> Int
balance Empty = 0
balance (Node tree1 x tree2) = height tree1 - height tree2

-- UT-24
isBalanced :: AVLTree a -> Bool
isBalanced Empty = True
isBalanced (Node tree1 x tree2) =   abs (balance (Node tree1 x tree2)) <= 1 && isBalanced tree1 && isBalanced tree2

-- UT-25
-- a)
rotateLeft :: AVLTree a -> AVLTree a
rotateLeft (Node t1 x (Node t2 y t3)) = Node (Node t1 x t2) y t3
rotateLeft tree = tree

-- b)
rotateRight :: AVLTree a -> AVLTree a
rotateRight (Node (Node t1 y t2) x t3) = Node t1 x (Node t2 y t3)
rotateRight tree = tree

-- UT-26
rebalance :: AVLTree a -> AVLTree a
rebalance Empty = Empty
rebalance t@(Node t1 v t2)
    | bal > 1 =
        if( balance t1 >= 0)
        then rotateRight (t)
        else rotateRight (Node ( rotateLeft t1) v t2)
    | bal < -1 =
        if( balance t2 <= 0)
        then rotateLeft (t)
        else rotateLeft (Node t1 v ( rotateRight t2))
    | otherwise = t
    where bal = balance t

-- UT-27
insert :: (Ord a, Eq a) => a -> AVLTree a -> AVLTree a
insert n Empty = Node Empty n Empty
insert n (Node t1 v t2)
    | n == v    = Node t1 v t2
    | n > v     = rebalance (Node t1 v (insert n t2))
    | otherwise = rebalance (Node (insert n t1) v t2)

-- UT-28
delete :: (Ord a, Eq a) => a -> AVLTree a -> AVLTree a
delete n Empty = Empty
delete n (Node t1 v t2)
    | n == v    = Node t1 v t2
    | n > v     = rebalance (Node t1 v (delete n t2))
    | otherwise = rebalance (Node (delete n t1) v t2)

---

-- IP-4
-- It attempts to concatenate a string with an IO action, which causes a type error

-- IP-6
-- a)
myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
        putChar x
        myPutStr xs

-- b)
myPutStrLn :: String -> IO ()
myPutStrLn str = myPutStr $ str ++ "\n"  -- this is equivalent to myPutStr (str ++ "\n")

-- IP-7
myGetLine :: IO (String)
myGetLine = do
    x <- getChar
    if(x == '\n')
    then return []
    else do
        xs <- myGetLine
        return (x:xs)

-- IP-8

drawBorders :: Int -> IO()
drawBorders n = writeListAt [(x,y) | x <- [1..n], y <- [1..n], (min x y == 1) || (max x y == n)] '.'
patrolPoint :: Int -> Int -> IO ()
patrolPoint nTicks n
    | nTicks >= 0 && n >= 0 = patrolPointAux nTicks n (1 ,1)
    | otherwise = error "Negative argument"
type Pos = (Int ,Int)
patrolPointAux :: Int -> Int -> Pos -> IO ()
patrolPointAux 0 _ _ = return ()
patrolPointAux nTicks n dot = do
    cls
    drawBorders n
    writeAt dot 'x'
    wait 200
    patrolPointAux (nTicks - 1) n ( nextPoint dot n)
nextPoint :: Pos -> Int -> Pos
nextPoint (x,y) n
    | y == 1 && x < n = (x+1,y)
    | x == n && y < n = (x,y+1)
    | y == n && x > 1 = (x-1,y)
    | otherwise = (x,y -1)

patrolSnake :: Int -> Int -> IO ()
patrolSnake nTicks n
    | nTicks >= 0 && n >= 0 = patrolSnakeAux nTicks n [(x ,1) | x <- [(div n 2) ,(div n 2 - 1).. 1]]
    | otherwise = error "Negative argument"

patrolSnakeAux :: Int -> Int -> [Pos] -> IO ()
patrolSnakeAux 0 _ _ = return ()
patrolSnakeAux nTicks n snake = do
    cls
    drawBorders n
    writeListAt snake 'x'
    wait 100
    patrolSnakeAux (nTicks - 1) n (nextSnake snake n)

nextSnake :: [Pos] -> Int -> [Pos]
nextSnake snake n = (nextPoint (head snake) n):(init snake)