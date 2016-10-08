module StateTExperiments where

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
    fmap = undefined

instance Applicative m => Applicative (StateT s m) where
    pure = undefined
    (<*>) = undefined

instance Monad m => Monad (StateT s m) where
    return a = StateT (\s -> return (a,s))
    m >>= f = StateT (\s -> do (a, s') <- runStateT m s
                               runStateT (f a) s')
class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
    lift m = StateT (\s -> do a <- m
                              return (a,s))

lift2 :: Monad m => m a -> StateT s m a
lift2 m = StateT (\s -> do a <- m
                           return (a,s))
