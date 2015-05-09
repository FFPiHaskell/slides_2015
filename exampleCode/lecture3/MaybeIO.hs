module MaybeIO where

import Control.Applicative

data MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
  fmap f = MaybeIO . fmap (fmap f) . runMaybeIO

instance Applicative MaybeIO where
  pure    = MaybeIO . pure . pure
  f <*> x = MaybeIO $ (<*>) <$> f' <*> x'
            where
              f' = runMaybeIO f
              x' = runMaybeIO x

instance Monad MaybeIO where
  return  = pure
  x >>= f = MaybeIO $ x' >>= runMaybeIO . mb . fmap f
            where
              x' = runMaybeIO x
              mb :: Maybe (MaybeIO a) -> MaybeIO a
              mb (Just a) = a
              mb Nothing  = MaybeIO $ return Nothing
