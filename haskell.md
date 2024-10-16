# Haskell

Livro: [Learning Hakell](https://moodle2425.up.pt/pluginfile.php/95817/mod_resource/content/21/Learning%20Haskell%20by%20Solving%20Problems.pdf)

## 1. Introduction

### 1.2. Simple Expressions
| Operator   | Comment                                       |
|------------|-----------------------------------------------|
| +, -       | Addition, subtraction                         |
| *, /       | Multiplication, division (floating-point)     |
| ^, ^^, **  | Exponentiation (non-negative integer, integer, floating-point) |
| div        | Integer division quotient                     |
| mod        | Integer division remainder                    |
| ==         | Equality                                      |
| /=         | Inequality                                    |
| <, <=, >, >=| Comparison                                    |
| not        | Negation                                      |
| &&, \|\|   | Logical conjunction and disjunction           |

<p align="center">Table 1.1 Some basic numeric and logical operators in Haskell</p>

> **EA-1:** IN-3, IN-4

---
### 1.3. Simple functions

In Haskell, since functions behave like any other entity, they are defined using the same syntax as variables. In fact, variables (in the sense as those used in imperative languages) are a particular case of a function with no arguments.

The general syntax to define a function is:
`<function name > <argument 1> <argument 2> ... = <expression >`

Valid function and argument names begin with a lowercase letter, followed by letters, numbers, underscores (example: func_2 ) and apostrophes ( func' ). The names used cannot be any of the following reserved keywords: case class data default deriving do else if import in infix infixl infixr instance let module 
    → newtype of then type where .

> **EA-1:** IN-6

---
### 1.4. Conditional structures
There are various ways of writing conditional structures in Haskell, which, given a condition, determine which expression will be computed. The four main conditional structures are:
- if-then-else expressions.
- guards.
- pattern matching.
- case expressions.

> **EA-1:** IN-13

---
### 1.5. Recursion
In Haskell, there is not iteration, namely “for” and “while” cycles. To execute a fragment of code a certain number of times until a condition is met, one must use recursion, where a function's expression contains a call to itself.

> **EA-2:** IN-17, IN-18

## 2. Fundamentals on types

---
### 2.1. Elementary types
The type of any expression or function can be checked in GHCI using the `:type` command (or `:t` , for short). Examples:
```haskell
Prelude > :type True
True :: Bool
```
```haskell
Prelude > :type 'a'
'a' :: Char
```

| Type     | Comment                                                                 |
|----------|-------------------------------------------------------------------------|
| **Bool** | Boolean. Two values: True and False.                                    |
| **Char** | Character. Denoted by single quotes.                                    |
| **Int**  | Integer. Fixed-precision (i.e. has a maximum size) depending on the system's architecture. |
| **Integer** | Integer. Arbitrary precision.                                        |
| **Float** | Floating-point. Single precision (32 bits).                            |
| **Double** | Floating-point. Double precision (64 bits).                           |
<p align="center">Table 2.1 Most common elementary types in Haskell</p>

The Int type has the advantage over Integral of being more efficient in terms of time and space, since the variable's size is fixed. However, if not handled properly the former may lead to silent overflow issues.

---
### 2.2. Tuples
A tuple is a sequence of elements with a fixed size. The elements do not have to be all of the same type. They are denoted by parenthesis.

Tuples with one element do not exist as they have the type of the actual element.There is a single type for a tuple with zero elements, the unit type ().

```haskell
Prelude > :type (True ,'a')

(True ,'a') :: (Bool , Char)

Prelude > :type ('x',(True ,False))

('x',(True ,False)) :: (Char , (Bool , Bool))
```

| Function | Comment                                                   | Example            |
|----------|------------------------------------------------------------|--------------------|
| **fst**  | Returns the first element of a pair (binary tuple).         | `fst (3, 8) -> 3`  |
| **snd**  | Returns the second element of a pair (binary tuple).        | `snd (3, 8) -> 8`  |
<p align="center">Table 2.2 Some Prelude functions for tuples</p>

> **EA-1:** FT-3, FT-4

---
### 2.3. Lists
A list is a variable-sized sequence of elements of the same type. It has several key differences from tuples:
- Lists have a variable size, which means that a function that receives a list as input can handle lists with 2 or 1000 elements.
- They are homogeneous: all of the elements in a list must have the same type.
- A list can have one element (singleton list)

Empty lists are represented as [] . A String is a particular case of a list. It corresponds to an array of characters, [Char] .

Lists can be defined in various ways:
- Using square-brackets. This is used to represent lists of fixed size.
  
  `[1,4,7,10,13]`

- Using the "cons" operator ( : ).
  
  `1:4:7:10:13:[]`
  
- Using ranges.
  
  `[1,4..13]`

- Using list comprehensions
  
  `[x | x <- [1..15], mod x 3 == 1]` 

| Function | Comment                                                                                          | Example                        |
|----------|--------------------------------------------------------------------------------------------------|--------------------------------|
| **(++)**    | Appends two lists.                                                                                | `[1,2] ++ [3,4] -> [1,2,3,4]`  |
| **head**    | Extracts the first element of non-empty lists.                                                    | `head [1,2,3] -> 1`            |
| **tail**    | Removes the first element of non-empty lists (output can be [] if input is a singleton).          | `tail [1,2,3] -> [2,3]`        |
| **last**    | Extracts the last element of non-empty lists.                                                     | `last [1,2,3] -> 3`            |
| **init**    | Removes the last element of non-empty lists.                                                      | `init [1,2,3] -> [1,2]`        |
| **elem**    | Checks if a value is contained in a list.                                                         | `elem 3 [1,2,3] -> True`       |
| **(!!)**    | Returns the n-th element of a list (with indices starting at 0).                                  | `[1,2,3] !! 1 -> 2`            |
| **length**  | Returns the number of elements in a list.                                                         | `length [1,2,3] -> 3`          |
| **reverse** | Inverts the order of the elements in a list.                                                      | `reverse [1,2,3] -> [3,2,1]`   |
| **take**    | Extracts the first n elements of a list.                                                          | `take 2 [1,2,3] -> [1,2]`      |
| **drop**    | Removes the first n elements of a list.                                                           | `drop 2 [1,2,3] -> [3]`        |
| **repeat**  | Creates an infinite list with x as the value of all elements.                                     | `repeat 1 -> [1,1,1,1,1,1...]` |
| **cycle**   | Creates an infinite repetition of a list.                                                         | `cycle [1,2,3] -> [1,2,3,1,2,3...]` |
| **zip**     | Creates a list with the corresponding pairs of two lists.                                         | `zip [1,2,3] "abc" -> [(1,'a'),(2,'b'),(3,'c')]` |
| **sum**     | Adds all of the numbers in the list.                                                              | `sum [1,2,3,4] -> 10`          |
| **product** | Multiplies all of the numbers in the list.                                                        | `product [1,2,3,4] -> 24`      |

<p align="center">Table 2.3 Some Prelude functions for lists</p>

There is also a module with more useful functions to work with lists, Data.List. `import Data.List`

> **EA-1:** FT-9, FT-10, FT-11
> **EA-2:** FT-14, FT-18, FT-19

---
### 2.4. Typeclasses
Certain functions operate over a certain group of types but not over every single type. To allow this behavior, Haskell defines typeclasses which group a set of types by a common property. Types are instances of typeclasses, just like classes are implementations of interfaces in object-oriented programming languages (like Java).

| Typeclass     | Comment |
|---------------|---------|
| **Num**       | Numeric types. |
| **Integral**  | Integer types. |
| **Fractional**| Floating-point types. Supports real number division with `(/)`. |
| **Floating**  | Floating-point types. Includes a type for complex numbers, `Complex`. Defines certain functions with irrational numbers, such as `sqrt`, `log`, `sin`, `asin` and `sinh`. |
| **RealFloat** | Another floating-point typeclass that does not include complex numbers. |
| **Eq**        | Types for which the equality and inequality operators (`==`, `/=`) are defined. |
| **Ord**       | Types for which the comparison operators (`>`, `<`, `>=`, `<=`) are defined. |
| **Enum**      | Types that can be enumerated. |

<p align="center">Table 2.4 Some relevant typeclasses</p>

Simplified hierarchy of the **most common** typeclasses and types in Haskell:
![alt text](assets/image2.png)

---
### 2.5. Type variables
When documenting the type of a variable, instead of making the commitment of assigning a variable to a certain type, one could instead associate a variable to a typeclass (a more general type declaration). This can be achieved using the notation:
`e :: TC a =>a` .
This line denotes that e is of type a, which is an instance of typeclass TC. a is a type variable: e belongs to any data type a that is an instance of typeclass TC. a is intentionally left undefined. The arrow `=>` denotes a class constraint.

For example:
```haskell
zip [1,2] "abc" :: Num a => [(a, Char)]
```

When asked about the type of numbers (using the `:type` command), GHCI typically responds with a type variable to link it to a type class (usually Num ) rather than a specific type.

> **EA-1:** FT-21

---
### 2.6. Functional Types
- **Type Declarations**: In Haskell, it's good practice to declare a function's type above its definition. This has several advantages:
  - **Error Reduction**: Helps programmers reason about their functions, reducing programming errors.
  - **Documentation**: Provides useful documentation for users to understand the function better.
  - **Clear Error Messages**: Assists in generating clearer error messages when functions are used with incorrect argument types.

- **Type Inference**: Haskell compilers can infer types for most expressions and functions, which means explicit type declarations are not always necessary.

- **Function Type Structure**: A function `f` with `n` arguments of types `t1, t2, ..., tn` produces an output of type `T`, represented as:

`f :: t1 -> t2 -> ... -> tn -> T`

If there are class constraints, they appear before the function name.

- **Polymorphic Functions**: Functions that have type declarations containing type variables are known as polymorphic functions.

> **EA-1:** FT-23, FT-24

## 3. Lists

---
### 3.1. Lists by range

Lists in Haskell can be defined using various range formats:

- **Basic Range with Step**:
  - Format: `[<value1>, <value2> .. <valueN>]`
  - Produces a list starting from `<value1>`, with each element incrementing by `<value2> - <value1>`, until exceeding `<valueN>`. 
  - If the step is negative, `<value1>` must be greater than `<valueN>`. If the signs conflict, an empty list is returned.

- **Simple Incrementing Range**:
  - Format: `[<value1> .. <final value>]`
  - Assumes a step of `1` (i.e., `<value2> = <value1> + 1`).

- **Infinite List with Specified Step**:
  - Format: `[<value1>, <value2> ..]`
  - Creates an infinite list starting at `<value1>` with a step of `<value2> - <value1>`.

- **Infinite Incrementing List**:
  - Format: `[<value1>, ..]`
  - Assumes a step of `1`.

**Examples:**
```haskell
Prelude > [1..]
[1,2,3,4,5,6,7,8,9,10...]
Prelude > [1..5]
[1,2,3,4,5]
Prelude > [1,3..10]
[1,3,5,7,9]
Prelude > [1,0..]
[1,0,-1,-2,-3,-4,-5,-6,-7,-8...]
Prelude > [1,3..]
[1,3,5,7,9,11,13,15,17,19...]
Prelude > ['a'..]
"abcdefghijklmnopqrstuvwxyz{|}~\DEL\128\129..."
Prelude > [0.1, 0.2 .. 1]
[0.1, 0.2, 0.30000000000000004, 0.4, 0.5, 0.6, 0.7000000000000001, 0.8, 0.9, 1.0]
```

**Important Notes:**

- When using ranges with floating-point values, be cautious of numeric imprecision.
- Elements of a list range must belong to a type that is an instance of the Enum typeclass:
```haskell
Prelude > f a b = [a .. b]
Prelude > :t f
f :: Enum a => a -> a -> [a]
```

**Lazy Evaluation**
Haskell handles infinite lists through lazy evaluation, computing values only when needed. For example, using `take` to retrieve the first N elements from an infinite list results in finite computation:

```haskell
Prelude > take 5 [1..]
[1,2,3,4,5]
```
Working with infinite lists separates the logic of generating a list from processing it, making certain functions easier to implement and more readable.

---

> **EA-4:** LI-2

### 3.2. Lists by recursion
The previous chapter introduced some examples of recursive functions with lists. This section contains exercises to implement recursive functions with lists that return new lists.

> **EA-2:**  LI-13, LI-14, LI-15, LI-16, LI-17, LI-18, LI-20
> **EA-4:** LI-10

---
### 3.3. Lists by comprehension
List comprehensions are a concise way to build lists using other lists. The general structure is:

`[<pattern> | <generator 1>, <generator 2>, ..., <guard 1>, <guard 2> ...]`

- **Generators**: Each generator has the format `<pattern> <- <list>`. They iterate through their respective lists and produce values for each element visited.
- **Guards**: Conditions that must be met for an instance of the pattern to be included in the output list. They function as filters.

**Examples:**

1. **Basic List Comprehension**:
    ```haskell
    Prelude > [x^2 | x <- [1..10]]
    [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    ```

2. **Using Guards**:
    ```haskell
    Prelude > [x^2 | x <- [1..10], odd x]
    [1, 9, 25, 49, 81]
    
    Prelude > [x^2 | x <- [1..10], odd x, mod x 3 == 0]
    [9, 81]
    ```

3. **Patterns in Generators**:
    ```haskell
    Prelude > [x | (x:_) <- [[1, 2], [3, 4]]]
    [1, 3]
    
    Prelude > [(a, b) | (a, b) <- zip [1..3] [1..]]
    [(1, 1), (2, 2), (3, 3)]
    ```

**Multiple Generators:**

Using multiple generators behaves like nested loops: for each value of the leftmost generator, all combinations of values from the generators to the right are produced. Changing the order of generators affects the resulting list.

*Examples:*

1. **Order Matters**:
    ```haskell
    Prelude > [(x, y) | x <- [1, 2], y <- "ab"]
    [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
    
    Prelude > [(x, y) | y <- "ab", x <- [1, 2]]
    [(1, 'a'), (2, 'a'), (1, 'b'), (2, 'b')]
    ```

2. **Nested List Comprehension**:
    ```haskell
    Prelude > [[x, y] | x <- "ab", y <- x:"ab"]
    ["aa", "aa", "ab", "bb", "ba", "bb"]
    ```

**Conclusion**

List comprehensions provide a powerful and expressive way to create and manipulate lists in Haskell, allowing for concise code that can replace more verbose looping constructs.

> **EA-2:** LI-29, LI-31, LI-32, LI-33, LI-35, LI-36, LI-39
> **EA-4:** LI-34, LI-37, LI-38, LI-40, LI-41, LI-42

## 4. Higher-order functions

This chapter covers **higher-order functions**, central to functional programming for enabling greater abstraction and flexibility by allowing functions to take or return other functions. It introduces *lambdas* (anonymous functions), *currying* (transforming multi-argument functions into single-argument ones), and *key Prelude functions* like function *composition* (.), *function application* ($), and *folds* (foldl, foldr). The chapter concludes with *point-free style*, a concise way of writing functions without explicit arguments for cleaner, more readable code.

---
### 4.1. Fundamentals on higher-order functions
In functional type declarations, the `->` symbol is right-associative, meaning `a -> b -> c` is equivalent to `a -> (b -> c)`. Parentheses are used to clarify when an argument is a function. For example, a function f with a functional argument and returning another function would be declared as `f :: (a -> b) -> c -> (d -> e)`. To use such a function, the usual prefix notation can be applied: `f xyz`, where `x`, `y`, and `z` are the function's arguments.

> **EA-3:** HO-3, HO-4, HO-7

---
### 4.2. Lambdas
Lambdas, or anonymous functions, are a convenient way to define functions on the fly without giving them a name. They are particularly useful when the function is only needed once or in a small scope. The general form of a lambda in functional programming languages, such as Haskell, is:

```haskell
\x1 x2 ... xN -> f x1 x2 ... xN
```

Here, the backslash (`\`) indicates the start of the lambda function, followed by the arguments, and then the function's expression after the `->`.

**Anonymous**: Lambdas are functions without names.

**On-the-fly**: They are typically written where needed, such as within an expression.

**Single clause**: Unlike named functions, lambdas must be defined in a single clause, which can limit their use for more complex recursive cases.

**Pattern matching**: You can use pattern matching in lambdas, but if the pattern fails, a runtime error will occur.

Examples:

```haskell
(\x -> x + 1) 2 evaluates to 3 (adds 1 to the argument).
(\(x:xs) -> x) [1..10] extracts the first element of a list (1 from [1..10]).
```

Lambdas are commonly used in places where defining a full function is unnecessary.

> **EA-3:** HO-8

---
### 4.3. Currying
In Haskell, all functions actually only accept one argument. Functions with multiple arguments can be considered as a series of functions which receive an argument and return a function which receives the second argument, and so on. This is known as currying, a reference to the mathematician Haskell Curry, who shares his first name with the programming language.
- **Aplication:** Arguments are passed to functions, one-by-one, by putting spaces between the
function’s name and the name of each argument. 
- **Partially applied**: If fewer arguments are passed than required, the function doesn't immediately compute a result; instead, it returns another function that waits for the remaining arguments. This can help reduce the need for defining new functions and makes the code more concise.
```haskell
map (drop 2) [[1,2,3],[4,5,6]]
-- In this case, drop 2 is partially applied, resulting in a new function that removes the first two elements of any list passed to it.
```
- **Sections:** Infix operators can also be partially applied by enclosing them in parentheses, which is known as sections. This feature allows for concise function definitions using operators.
```haskell
map (*2) [1..5]    -- Multiplies each element by 2
map (2*) [1..5]    -- Also multiplies each element by 2
(`elem` [1..3]) 3  -- Checks if 3 is in the list [1..3]
```
- **Curried vs. Tupled Arguments:** While functions can alternatively take a tuple as an argument (unary function with a tuple), this is discouraged. Curried functions offer more flexibility than tuple arguments. Tuples should only be used when the argument itself is inherently a tuple.
```haskell
-- Example of a curried function:
add :: Int -> Int -> Int
add x y = x + y

-- Example of a function with a tuple argument:
add' :: (Int, Int) -> Int
add' (x, y) = x + y
```
> **EA-3:** HO-10

---
### 4.4. Common higher-order functions
| Function    | Comment                                                                                     | Example                              |
|-------------|---------------------------------------------------------------------------------------------|--------------------------------------|
| **map**       | Applies a function to each element of a list. | `map succ [1,2,3] -> [2,3,4]` |
| **filter**    | Returns a sublist with the elements that satisfy a predicate (i.e. a function that returns a boolean). | `filter odd [1..5] -> [1,3,5]` |
| **any**       | Checks if at least one element of a list satisfies a predicate. | `any even [1,1,1,3,1] -> False` |
| **all**       | Checks if all the elements of a list satisfy a predicate. | `all odd [1,1,1,3,1] -> True` |
| **takeWhile** | Returns the longest prefix of a list that satisfies a predicate. | `takeWhile odd [1,1,1,2,3] -> [1,1,1]` |
| **dropWhile** | Returns the remainder of a list after calling `takeWhile`. | `dropWhile odd [1,1,1,2,3] -> [2,3]` |
| **iterate**   | Returns an infinite list where the i-th element is the application of a function `f` on a value `x` i times (with indices starting at 0). | `iterate succ 0 -> [0,1,2,3,4,5,6,7,8,9,...]` |
| **zipWith**   | Zips two lists, then combines each pair using a binary function. | `zipWith (+) [1,2,3,4] [3,2,4,1] -> [4,4,7,5]` |
| **flip**      | Swaps the order of the arguments in a binary function. | `flip (/) 2 0 -> 2.0` |
| **(.)**       | Function composition. | (Check section 4.5) |
| **($)**       | Function application. | (Check section 4.5) |
| **foldr**     | Right-associative fold of a structure. | (Check section 4.6) |
| **foldl**     | Left-associative fold of a structure. | (Check section 4.6) |

> **EA-3:** HO-13, HO-14, HO-15, HO-16, HO-17, HO-18, HO-22, HO-23
> **EA-4:** HO-19, HO-24

---
### 4.5 Application and composition
In Haskell, **function application** is done by placing the function name followed by its arguments, separated by spaces. `f x y` is interpreted as `f x y = (f x) y`. By default, function application is *left-associative*, meaning it is processed from left to right. Application has a very high precedence, so it occurs before other operations. 
- **Dollar Sign ($):** This operator is used to apply a function with lower precedence than regular application. This allows for reduced parentheses in expressions. 
```haskell
succ (succ 1)         -- 3
succ succ 1           -- error
succ $ succ 1         -- 3
(*3) $ succ $ (*8) 2  -- 51
```

**Function composition** allows the output of one function to be used as the input for another, creating a new function. It is denoted by `(g . f)`, where `g` is applied to the result of `f`. Composition is *right-associative*, meaning `h . g . f` is evaluated as `h . (g . f)`.
```haskell
(head . tail) [1..5] -- 2
(succ . (*8)) 4      -- 33
```

> **EA-3:** HO-27, HO-29

---
### 4.6 Folds

**Folds** are higher-order functions that process data structures (typically lists) in a specified order and return a value. 
Folds have usually **two ingredients**: 
- **Combining Function:** A binary function that takes two inputs(an accumulator and an element from the data structure).
- **Accumulator:** A value that accumulates results as the fold processes the list.

Folding functions receive three arguments (in order): the *combining function*, the *initial value of the accumulator* and the *list*:
- *Accumulator:* must be the same type as the return value type of the fold.
- *Initial value:* usually the identity/neutral element of the combining function.

The **two main folding functions** are:
- `foldr` (Right Fold):
  - it recursively combines the result of the list's head and accumulator with the result combining with the tail.
- `foldl` (Left Fold):
  - it recursively combines the result of combining all but the list's last element and the accumulator with the last element.

Using left or right folds gives the same result if the operation of the combining f function is *associative*: f(fab)c = fa(fbc).

Difference between left and right folds:
```haskell
foldr (-) 0 [1..5] -- = (1 - (2 - (3 - (4 - (5 - 0))))) = 3
foldl (-) 0 [1..5] -- = (((((0 - 1) - 2) - 3) - 4) - 5) = -15
```

Folds have the *advantage* of allowing for more compact code, relative to recursive functions.

**`scanr` and `scanl`:** work mostly like `foldr`/`foldl` but instead return a list with all the intermidiate values of the computations.
```haskell
scanr (-) 0 [1..5] -- = [3,-2,4,-1,5,0]
scanl (-) 0 [1..5] -- = [0,-1,-3,-6,-10,-15]
```

> **EA-3:** HO-32, HO-33, HO-35, HO-37, HO-40, HO-42, HO-43

---
### 4.7. Point-free style

**Point-free style:** the arguments of the function are omitted from its definition.

The main tools to program in point-free style are using composition and other higher-order functions (namely maps, filters and folds), rather than application. 

*Advantage:* function definitions are more readable, elegant and concise.

Example:
```haskell
import Data.Char
capitalize :: [Char] -> [Char]
capitalize = map toUpper

-- map is used to apply toUpper to each element of the string. The function is
-- defined in point-free style by leaving map partially applied: only the functional
-- argument is provided, while the list is left to be applied by those who call
-- capitalize .
-- This solution shows an example of how to write an unary function in point-
-- free style.
```

> **EA-3:** O-47, HO-48, HO-49, HO-50, HO-51, HO-52,
HO-53

---

## 5. User-defined types

### 5.1. Creating type synonyms with the type keyword

The `type` keywork can be used to define **type synonyms**.
- *Advantage:* increase the readability of Haskell code by providing syntatic sugar.

```haskell
type <synonym name > <type variable 1> <type variable 2> ... = <expression >
```

- Synonym's name must start with an uppercase.
- Synonyms cannot have recursive definitions.

```haskell
type String = [Char]   -- from Prelude
type Pair a = (a,a)
type HashMap k v = [(k,v)]
```

> **EA-4:** UT-3, UT-4

---

### 5.2. Creating algebraic data types with the data keyword

If one wants to define a structure for a person with two strings: one with their name and another with their email:
```haskell
type Person = (String,String)
```

However, the declaration above does not allow one to distinguish a person from any other pair composed of two strings.

If one deines a function that receives as input a pair representing a country and its capital (`type CountryCapital = (String,String)`) one could also pass a Person as input, which is semantically wrong, even though it is syntactically correct.

The `data` keyword circumvents this problem by defining new algebric data types. 

- *Advantages:* better structured code, readability and improvestype safety.

```haskell
data <type name > <type variable 1> <type variable 2> ... =
<value constructor 1> <type 1> <type 2> ... |
<value constructor 2> <type 1> <type 2> ... |
...
```

Examples:
```haskell
data Bool = False | True           -- Prelude
data Maybe a = Just a | Nothing    -- Prelude
data Shape = Circle Double Double Double | Rectangle Double Double Double Double
```

- **Maybe:** can be used as an alternative to errors in functions that may fail (e. g. `head` with an empty list)
  - *Just:* has a variable type
  - *Nothing:* has no arguments/fields

Each value constructor must only be used once in a data declaration. They can be used in two different ways: as functions or in patterns. Examples:

```haskell
Prelude > data Shape = Circle Double Double Double | Rectangle Double Double Double Double
Prelude > :type Circle
  Circle :: Double -> Double -> Double -> Shape
Prelude > :type ( Circle 2.0)
  ( Circle 2.0) :: Double -> Double -> Shape
Prelude > let area (Circle _ _ r) = pi*r^2
Prelude > :type area
  area :: Shape -> Double
Prelude > area( Circle 1.0 2.0 1.0)
  3.141592653589793
Prelude > map (area .( Circle 1.0 2.0)) [1..5]
  [3.141592653589793 ,12.566370614359172 ,28.274333882308138 ,50.26548245743669 ,78.53981633974483]
```

The type of the area function is Shape -> Double rather than Circle -> Double , since Shape is the actual type's name, while Circle is the name of one of its value constructors. 

- Unlike type synonyms, type definitions can be recursive.
```haskell
Prelude > data MyList a = List a ( MyList a) | EmptyList
*Main > :type (List 4 (List 6 EmptyList ))
  (List 4 (List 6 EmptyList )) :: Num a => MyList a
```

> **EA-4:** UT-6, UT-7, UT-8

---

### 5.3. Derived types

Consider the `Shape` type defined in the previous section. To print a shape or compare two shapes one must define that it derives the typeclass Eq, using the `deriving`keyword.

```haskell
Prelude > data Shape = Circle Double Double Double | Rectangle Double Double Double Double
Prelude > Rectangle 1.0 2.0 3.0 4.0
<interactive >:2:1: error:
* No instance for (Show Shape) arising from a use of 'print'
* In a stmt of an interactive GHCi command : print it
Prelude > Circle 3 4 5 == Circle 3 5 5
<interactive >:3:1: error:
* No instance for (Eq Shape) arising from a use of '=='
* In the expression : Circle 3 4 5 == Circle 3 5 5
In an equation for 'it': it = Circle 3 4 5 == Circle 3 5 5
```
```haskell
Prelude > data Shape = Circle Double Double Double | Rectangle Double Double Double Double deriving (Show ,Eq)
Prelude > Rectangle 1.0 2.0 3.0 4.0
Rectangle 1.0 2.0 3.0 4.0
Prelude > Circle 3 4 5 == Circle 3 5 5
False
Prelude > Circle 3 4 5 == Circle 3 4 5
True
```

Other examples:
```haskell
Prelude > data Tempo = Adagio | Andante | Moderato | Allegro | Presto deriving (Eq ,Ord ,Show ,Enum)

-- Ord
Prelude > Andante < Allegro
True
Prelude > Adagio >= Moderato
False

-- Enum
Prelude > [ Adagio ..]
[Adagio ,Andante ,Moderato ,Allegro , Presto]
Prelude > [Adagio , Moderato ..]
[Adagio ,Moderato , Presto ]
Prelude > [Presto , Allegro ..]
[Presto ,Allegro ,Moderato ,Andante , Adagio]
```

For types T with parameters that derive Ord , a value A is less than a value B if the value constructor of A comes before the one for B in the definition of T. 
```haskell
Prelude > data Shape = Circle Double Double Double | Rectangle Double Double Double Double deriving (Eq ,Ord)
Prelude > Circle 1 2 3 < Rectangle 1 2 3 4
True
Prelude > Circle 1 2 3 < Circle 1 2 3
False
Prelude > Circle 1 50 3 < Circle 1 2 3
False
Prelude > Circle 1 2 3 < Circle 1 3 3
True
```

---

### 5.4. Named fields

When defining a new type using data , the fields of a value can be given names using the record syntax. If a class with named fields is an instance of Show, then they are printed in a different manner. 

```haslell
Prelude > data Date = Date { day :: Int , month :: Int , year :: Int} deriving (Show)
Prelude > Date 18 6 2006
Date {day = 18, month = 6, year = 2006}
Prelude > Date {day = 18, year = 2006 , month = 6}
Date {day = 18, month = 6, year = 2006}
```

- *Advantage:* allowing one to forget their order in the definition of a value constructor. 

It also avoids the need to write "getter" functions, which retrieve a field of a value. 

```haskell
Prelude > let d = Date {day = 18, year = 2006 , month = 6}
Prelude > month(d)
6
```

> **EA-4:** UT-9

---