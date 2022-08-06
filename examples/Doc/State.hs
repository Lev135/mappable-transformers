{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Doc.State where

{- ORMOLU_DISABLE -}
import Control.Monad.State (MonadState)
import Control.Monad.State.Mappable (mapTState, MappableState)
import Control.Monad.Writer.Class (MonadWriter)

data SomeLogMsg

data FooState
foo :: (MonadWriter [SomeLogMsg] m, MonadState FooState m) => m Int
foo = undefined

data BarState
bar :: (MonadWriter [SomeLogMsg] m, MonadState BarState m) => m Int
bar = undefined

data FooBarState = FooBarState {fooState :: FooState, barState :: BarState}
foobar ::
 (MappableState FooState FooBarState m1 m,
  MappableState BarState FooBarState m2 m,
  MonadWriter [SomeLogMsg] m1, MonadWriter [SomeLogMsg] m2) => m Int
foobar = do
  fooRes <- mapTState fooState (\fooState s -> s{fooState}) foo
  barRes <- mapTState barState (\barState s -> s{barState}) bar
  return $ fooRes + barRes
