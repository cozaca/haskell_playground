-- | Recursive data structures

-- | we can create recursive data types, where one value of some type contains values of that type,
-- | which in turn contain more values of the same type and so on.

-- | data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
-- |  fixity declarations. When we define functions as operators, we can use that to give them a fixity
-- | (but we don't have to). A fixity states how tightly the operator binds and whether it's left-associative or right-associative.
-- | For instance, *'s fixity is infixl 7 * and +'s fixity is infixl 6
data MyList a = MyEmpty | a :-: (MyList a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: MyList a -> MyList a -> MyList a
MyEmpty .++ ys    = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- | BST
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

insertT :: (Ord a) => a -> Tree a -> Tree a
insertT x EmptyTree = singleton x
insertT x (Node y left right)
    | x == y = Node x left right
    | x < y  = Node y (insertT x left) right
    | x > y  = Node y left (insertT x right)

elemT :: (Ord a) => a -> Tree a -> Bool
elemT _ EmptyTree = False
elemT x (Node y left right)
    | x == y = True
    | x < y  = elemT x left
    | x > y  = elemT x right

myTree :: (Ord a) => [a] -> Tree a
myTree a = foldr insertT EmptyTree a