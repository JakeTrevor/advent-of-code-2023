length :: [a] -> Int
-- length (x:xs) = 1 + length xs
-- length [] = 0;

-- foldr (a->b->b)->b-> a -> b
length = foldr (\_ b -> b + 1) 0

mySum :: [Int] -> Int
-- mySum [] = 0
-- mySum (x : xs) = x + sum xs

mySum = foldr (+) 0

ones = repeat 1

-- haskell has two significant features that allow it to operate with infinite data structures.
-- Firstly, haskell allows values to be recursively defined; for example, we can define an infinite list of 1s with `ones = 1 : ones`. or with `ones = repeat 1`. This is very important as without it infinite structures are very difficult to define.

-- The second major feature of haskell that facilitates dealing with infinite structures is that it is lazy; expressions are only evaluated when they are needed. This is opposed to eager evaluation

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [x] = x
myConcat ([] : xss) = concat xss
myConcat ((x : xs) : xss) = x : myConcat (xs : xss)

data Tree a = Leaf | Node a (Tree a) (Tree a)

countNodes :: Tree a -> Int
countNodes Leaf = 1
countNodes (Node _ l r) = 1 + countNodes l + countNodes r

mirror :: Tree a -> Tree a
mirror Leaf = Leaf
mirror (Node a l r) = Node a (mirror r) (mirror l)

class Twistable t where
  twist :: t -> t
  size :: t -> Int

instance Twistable (Tree a)