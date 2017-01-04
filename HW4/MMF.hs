{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module MMF where
    class MonadFish m where
        returnFish :: a -> m a
        (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    instance Monad m => MonadFish m where
        returnFish = return
        f >=> g    = \x -> f x >>= g