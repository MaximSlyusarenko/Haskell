{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EighthHW where

class MonadTrans t where
    lift :: Monad m => m a -> t m a

class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
 
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= \a -> return (f a)
 
 
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
newtype EitherT l m r = EitherT { runEitherT :: m (Either l r) }
newtype ReaderT e m v = ReaderT { runReaderT :: e -> m v}
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance Functor (StateT s m) where
    fmap = undefined

instance Applicative (StateT s m) where
    pure = undefined
    (<*>) = undefined

instance Functor (WriterT w m) where
    fmap = undefined

instance Applicative (WriterT w m) where
    pure = undefined
    (<*>) = undefined

instance Functor (EitherT l m) where
    fmap = undefined

instance Applicative (EitherT l m) where
    pure = undefined
    (<*>) = undefined

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= f  = StateT $ \s -> do
        (a, newState) <- runStateT m s
        runStateT (f a) newState

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= f  = WriterT $ do
        (a, output)  <- runWriterT m
        (b, output1) <- runWriterT (f a)
        return (b, output `mappend` output1)

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)

instance Monad m => Monad (EitherT l m) where
    return a = EitherT $ return (Right a)
    m >>= f  = EitherT $ do
        a <- runEitherT m
        case a of
            Left l  -> return $ Left l
            Right r -> runEitherT (f r)

instance MonadTrans (EitherT l) where
    lift m = EitherT $ do
        a <- m
        return $ Right a

-------------------------------------------------------------------------------

instance Functor (ReaderT r m) where
    fmap = undefined

instance Applicative (ReaderT r m) where
    pure = undefined
    (<*>) = undefined

instance Functor (MaybeT m) where
    fmap = undefined

instance Applicative (MaybeT m) where
    pure = undefined
    (<*>) = undefined

instance Monad (ReaderT r m) where
    return = undefined
    (>>=) = undefined

instance MonadTrans (ReaderT r) where
    lift = undefined

instance Monad (MaybeT m) where
    return = undefined
    (>>=) = undefined

instance MonadTrans (MaybeT) where
    lift = undefined


instance Monad m => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put = lift . put