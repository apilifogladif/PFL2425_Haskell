-- Stack.hs
module Stack (
    Stack, createEmptyStack, isEmpty,
    push, pop, top
) where

-- Define the Stack data type using a list
data Stack a = St [a]

-- Create an empty stack
createEmptyStack :: Stack a
createEmptyStack = St []

-- Check if the stack is empty
isEmpty :: Stack a -> Bool
isEmpty (St []) = True
isEmpty _ = False

-- Push an element onto the stack
push :: a -> Stack a -> Stack a
push x (St xs) = St (x:xs)

-- Pop an element from the stack (returns a new stack)
pop :: Stack a -> Stack a
pop (St []) = error "Empty stack"
pop (St (x:xs)) = St xs

-- Get the top element of the stack (without removing it)
top :: Stack a -> a
top (St []) = error "Empty stack"
top (St (x:xs)) = x
