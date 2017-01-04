{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module MMJ where
    class MonadJoin m where
        returnJoin :: a -> m a
        join       :: m (m a) -> m a

    instance Monad m => MonadJoin m where
        returnJoin = return
        join x     = x >>= id