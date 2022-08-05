{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Reader where

import Control.Monad.Reader (MonadReader, asks, runReader, runReaderT)
import Control.Monad.Reader.Mappable (MappableReader (mapTReader))
import Control.Monad.Writer (MonadWriter (tell), runWriter, runWriterT)
import Control.Monad.Writer.Mappable (MappableWriter (mapTWriter))

-- | Environment for 'foo' function
data Foo = Foo {fooInt :: Int, fooDouble :: Double}

-- | 'foo' function uses only its part of complex 'FooBar' data structure
foo :: (MonadWriter [String] m, MonadReader Foo m) => m Int
foo = do
  n <- asks fooInt
  m <- asks $ floor . fooDouble
  tell [show n, show m]
  return $ n + m

-- | Environment for 'bar' function
data Bar = Bar {barString :: String, barDouble :: Double}

-- | 'bar' function uses only its part of complex 'FooBar' data structure
bar :: (MonadWriter [String] m, MonadReader Bar m) => m Int
bar = do
  str <- asks barString
  d <- asks barDouble
  tell [str, show d]
  return $ round d

data FooBar = FooBar {fooData :: Foo, barData :: Bar}

-- | 'foobar' function take whole 'FooBar' environment
foobar ::
  -- We could omit this constraint and use wildcard as in Main.hs,
  -- it's explicit here only to make clearer how it works
  ( MappableReader Foo FooBar m1 m,
    MappableReader Bar FooBar m2 m,
    MonadWriter [String] m1,
    MonadWriter [String] m2
  ) =>
  m Int
foobar = do
  fooRes <- mapTReader fooData foo
  barRes <- mapTReader barData bar
  return $ fooRes + barRes

main :: IO ()
main = do
  print . runWriter . flip runReaderT env $ foobar
  print . flip runReader env . runWriterT $ foobar
  where
    env =
      FooBar
        { fooData = Foo {fooInt = 42, fooDouble = 3.14},
          barData = Bar {barString = "Hello", barDouble = 36.6}
        }
