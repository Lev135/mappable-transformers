{-# LANGUAGE FlexibleContexts #-}

module Doc.Reader where

{- ORMOLU_DISABLE -}
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Mappable (mapTReader, MappableReader)
import Control.Monad.State.Class (MonadState)

data SomeState

data FooEnv
foo :: (MonadState SomeState m, MonadReader FooEnv m) => m Int
foo = undefined

data BarEnv
bar :: (MonadState SomeState m, MonadReader BarEnv m) => m Int
bar = undefined

data FooBarEnv = FooBarEnv {fooEnv :: FooEnv, barEnv :: BarEnv}
foobar ::
 (MappableReader FooEnv FooBarEnv m1 m,
  MappableReader BarEnv FooBarEnv m2 m,
  MonadState SomeState m1, MonadState SomeState m2) => m Int
foobar = do
  fooRes <- mapTReader fooEnv foo
  barRes <- mapTReader barEnv bar
  return $ fooRes + barRes
