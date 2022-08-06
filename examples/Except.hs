--  'AllowAmbiguousTypes' extension needed only to map writer's log using 'show'
-- (I don't clearly understand why it is so)
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Except where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Except.Mappable (MappableError (mapTError))
import Control.Monad.State (MonadState (..), modify, runState, runStateT)
import Control.Monad.Trans.Except (runExcept, runExceptT)
import Control.Monad.Writer (MonadWriter (tell), runWriter, runWriterT)
import Control.Monad.Writer.Mappable (MappableWriter (mapTWriter))

-- * Two functions, each with its own error type

newtype FooErr = EvenFooCall Int
  deriving (Show)

foo :: (MonadState Int m, MonadError FooErr m, MonadWriter [Int] m) => m String
foo = do
  n <- get
  tell [n]
  if even n
    then throwError $ EvenFooCall n
    else do
      modify succ
      n' <- get
      tell [n']
      return $ show n'

newtype BarErr = BarOddCall Int
  deriving (Show)

bar :: (MonadState Int m, MonadError BarErr m, MonadWriter [Int] m) => m String
bar = do
  n <- get
  tell [n]
  if odd n
    then throwError $ BarOddCall n
    else do
      modify pred
      n' <- get
      tell [n']
      return $ show n'

-- | Composed error type
data FooBarErr = FooErr FooErr | BarErr BarErr
  deriving (Show)

foobar ::
  _ =>
  -- There is awful constraint with 5 monad type variables here, but we can
  -- leave a hole and everything will work fine
  -- (it won't if we omit type signature
  -- --- in that case ghc won't understand, that we want to use abstract
  -- classes instead of concrete transformers)
  m String
foobar = do
  x <- get
  -- The order of maps calls ('mapTWriter'/'mapTError') doesn't matter
  -- even if these affects doesn't commute
  fooRes <- mapTWriter (map (("in foo: " <>) . show)) . mapTError FooErr $ foo
  put x
  -- We'll see the same here, if the first call succeeded
  _ <- mapTError FooErr . mapTWriter (map (("foo again: " <>) . show)) $ foo
  modify (+ x `div` 2)
  barRes <- mapTError BarErr . mapTWriter (map (("in bar: " <>) . show)) $ bar
  return $ unwords [fooRes, barRes]

main :: IO ()
main = forM_ ([1 .. 5] :: [Int]) $ \n -> do
  print n
  -- Mappable classes as well as simple classes from mtl
  -- let us run transformer in arbitrary order
  -- (with different results, naturally)
  print . runWriter . flip runStateT n . runExceptT $ foobar
  print . runWriter . runExceptT . flip runStateT n $ foobar
  print . flip runState n . runWriterT . runExceptT $ foobar
  print . runExcept . runWriterT . flip runStateT n $ foobar
  print . flip runState n . runExceptT . runWriterT $ foobar
  print . runExcept . flip runStateT n . runWriterT $ foobar
  putStrLn ""
