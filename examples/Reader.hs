{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Reader where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Reader.Mappable (MappableReader (mapReader))
import Control.Monad.Writer (MonadWriter (tell))
import Control.Monad.Writer.Mappable (MappableWriter (mapWriter))

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
  fooRes <- mapReader fooData foo
  barRes <- mapReader barData bar
  return $ fooRes + barRes
