{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module PairInstances where

import           Prelude (undefined, Monoid, mempty, ($))

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor ((,) a) where
    fmap f (a, b) = (a, f b)

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Monoid a => Applicative ((,) a) where
    pure b = (mempty, b)
    (_, f) <*> (a, b) = (a, f b)

class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable ((,) a) where
    foldr f z (a, b) = f b z

class Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable ((,) a) where
    traverse f (a, b) = fmap ((,) a) $ f b 