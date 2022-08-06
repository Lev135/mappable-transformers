{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module State where

import Control.Monad.State (MonadState)
import Control.Monad.State.Mappable (mapTState)
import Control.Monad.Writer.Class (MonadWriter)

data SomeLog

instance Semigroup SomeLog where
  (<>) = undefined

instance Monoid SomeLog where
  mempty = undefined

data FooState

foo :: (MonadWriter SomeLog m, MonadState FooState m) => m Int
foo = undefined

data BarState

bar :: (MonadWriter SomeLog m, MonadState BarState m) => m Int
bar = undefined

data FooBarState = FooBarState {fooState :: FooState, barState :: BarState}

foobar :: (MonadWriter SomeLog m, MonadState FooBarState m, _) => m Int
foobar = do
  fooRes <- mapTState fooState (\fooState s -> s {fooState}) foo
  barRes <- mapTState barState (\barState s -> s {barState}) bar
  return $ fooRes + barRes
