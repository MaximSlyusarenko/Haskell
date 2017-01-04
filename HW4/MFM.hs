{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module MFM where

    import           Prelude (id)

    class MonadFish m where
        returnFish :: a -> m a
        (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

    instance MonadFish m => Monad m where
        return     = returnFish
        x >>= g    = (id >=> g) x 