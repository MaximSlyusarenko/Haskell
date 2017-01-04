module FifthHW where

import           Prelude hiding (Either, Left, Right)

newtype Identity a = Identity { runIdentity :: a }

--fmap :: (a -> b) -> f a -> f b

instance Functor Identity where
    fmap f id1 = Identity (f $ runIdentity id1)

--pure :: a -> f a
--(<*>) :: f (a -> b) -> f a -> f b

instance Applicative Identity where
    pure = Identity
    fid <*> id1 = fmap (runIdentity fid) id1

--foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable Identity where
    foldr f z id1 = f (runIdentity id1) z

--traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable Identity where
    traverse f id1 = fmap Identity (f $ runIdentity id1)

-------------------------------------------------------------------------------

data Either a b = Left a | Right b

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right x) = Right (f x)

instance Applicative (Either a) where
    pure = Right
    (Left x) <*> _ = Left x
    (Right _) <*> (Left t) = Left t
    (Right f) <*> (Right t) = Right $ f t 

instance Foldable (Either a) where
    foldr _ z (Left _) = z
    foldr f z (Right x) = f x z

instance Traversable (Either a) where
    traverse _ (Left x) = pure $ Left x
    traverse f (Right x) = fmap Right (f x)

-------------------------------------------------------------------------------

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Applicative Tree where
    pure x = Node x Leaf Leaf
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node f left1 right1) <*> (Node x left2 right2) = Node (f x) (left1 <*> left2) (right1 <*> right2)

instance Foldable Tree where
    foldr _ z Leaf = z
    foldr f z (Node x left right) = foldr f (foldr f (f x z) left) right

instance Traversable Tree where
    traverse _ Leaf = pure Leaf
    traverse f (Node x left right) = Node <$> f x <*> traverse f left <*> traverse f right

-------------------------------------------------------------------------------

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
    pure x = Const mempty
    _ <*> (Const a) = Const a

instance Foldable (Const a) where
    foldr _ z _ = z

instance Traversable (Const a) where
    traverse _ (Const a) = pure $ Const a

-------------------------------------------------------------------------------