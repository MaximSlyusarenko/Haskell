{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}

module SMImpl where

    import           Prelude (fst, snd)

    newtype State s a = State { runState :: s -> (a, s) }

    class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

    instance Monad (State s) where
        return x = State (\s -> (x, s))
        h >>= f  = State (\s -> let newStateFunction           = runState (f oldValue)
                                    oldValue                   = fst runRes
                                    newState                   = snd runRes
                                    oldStateFunction           = runState h
                                    runRes                     = oldStateFunction s 
                                in newStateFunction newState)