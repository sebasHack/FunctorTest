module FunctorInstances where


-- Identity Functor

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)


-- Pair Functor

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)



-- Three Functor

data Three a b = Three a b b deriving (Eq, Show)

instance Functor (Three a) where
  fmap f (Three x y z) = Three x (f y) (f z)


-- Sum Functor

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First e) = First e
  fmap f (Second b) = Second (f b)

  
-- List Functor

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


-- Ordered Tree Functor

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap _  Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r) 

insert :: (Ord a) => a -> Tree a -> Tree a
insert x empt@Empty = Node x empt empt
insert x node@(Node a l r)
  | x == a = node
  | x < a = Node a (insert x l) r
  | otherwise = Node a l (insert x r)
 
