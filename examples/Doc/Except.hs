{-# LANGUAGE FlexibleContexts #-}

module Doc.Except where

{- ORMOLU_DISABLE -}
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Mappable (mapTError, MappableError)
import Control.Monad.State.Class (MonadState)

data SomeState

data FooErr
foo :: (MonadState SomeState m, MonadError FooErr m) => m Int
foo = undefined

data BarErr
bar :: (MonadState SomeState m, MonadError BarErr m) => m Int
bar = undefined

data FooOrBarErr = FooErr FooErr | BarErr BarErr
foobar ::
 (MappableError FooErr FooOrBarErr m1 m,
  MappableError BarErr FooOrBarErr m2 m,
  MonadState SomeState m1, MonadState SomeState m2) => m Int
foobar = do
  fooRes <- mapTError FooErr foo
  barRes <- mapTError BarErr bar
  return $ fooRes + barRes
