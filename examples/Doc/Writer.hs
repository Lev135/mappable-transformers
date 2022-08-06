{-# LANGUAGE FlexibleContexts #-}

module Doc.Writer where

{- ORMOLU_DISABLE -}
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer.Mappable (mapTWriter, MappableWriter)
import Control.Monad.State.Class (MonadState)

data SomeState

data FooLogMsg
foo :: (MonadState SomeState m, MonadWriter [FooLogMsg] m) => m Int
foo = undefined

data BarLogMsg
bar :: (MonadState SomeState m, MonadWriter [BarLogMsg] m) => m Int
bar = undefined

data FooOrBarLogMsg = FooLogMsg FooLogMsg | BarLogMsg BarLogMsg
foobar ::
 (MappableWriter [FooLogMsg] [FooOrBarLogMsg] m1 m,
  MappableWriter [BarLogMsg] [FooOrBarLogMsg] m2 m,
  MonadState SomeState m1, MonadState SomeState m2) => m Int
foobar = do
  fooRes <- mapTWriter (map FooLogMsg) foo
  barRes <- mapTWriter (map BarLogMsg) bar
  return $ fooRes + barRes
