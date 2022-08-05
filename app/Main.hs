{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..), runExcept, runExceptT)
import Control.Monad.Except.Mappable (MappableError (mapError))
import Control.Monad.State (MonadState (..), modify, runState, runStateT)
import Control.Monad.Writer (MonadWriter (tell), runWriter, runWriterT)
import Control.Monad.Writer.Mappable (MappableWriter (mapWriter))

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
      return $ show n

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
      return $ show n

data FooBarErr = FooErr FooErr | BarErr BarErr
  deriving (Show)

foobar ::
  _ =>
  m String
foobar = do
  x <- get
  fooRes <- mapWriter (map (("in foo: ",))) . mapError FooErr $ foo
  modify (+ x `div` 2)
  barRes <- mapError BarErr . mapWriter (map (("in bar: ",))) $ bar
  return $ unwords [fooRes, barRes]

tmp :: MappableWriter [Int] [String] m m' => m ()
tmp = pure ()

main :: IO ()
main = forM_ ([1 .. 5] :: [Int]) $ \n -> do
  print n
  print . runWriter . flip runStateT n . runExceptT $ foobar
  print . runWriter . runExceptT . flip runStateT n $ foobar
  print . flip runState n . runWriterT . runExceptT $ foobar
  print . runExcept . runWriterT . flip runStateT n $ foobar
  print . flip runState n . runExceptT . runWriterT $ foobar
  print . runExcept . flip runStateT n . runWriterT $ foobar
  putStrLn ""
