module Control.Monad.Reader.Mappable where

import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S

{- ORMOLU_DISABLE -}
{- | Type class for monad transformers with environment, that can be mappable
  from @r@ to @r'@, changing composed monad type from m to m'.

  Usually (as in example below), @r@ is a part of @r'@ used by subcomputation

==== __Example__

  > :set -XFlexibleContexts

  > import Control.Monad.Reader (MonadReader)
  > import Control.Monad.Reader.Mappable (mapTReader, MappableReader)
  > import Control.Monad.State.Class (MonadState)
  >
  > data SomeState
  >
  > data FooEnv
  > foo :: (MonadState SomeState m, MonadReader FooEnv m) => m Int
  > foo = undefined
  >
  > data BarEnv
  > bar :: (MonadState SomeState m, MonadReader BarEnv m) => m Int
  > bar = undefined
  >
  > data FooBarEnv = FooBarEnv {fooEnv :: FooEnv, barEnv :: BarEnv}
  > foobar ::
  >  (MappableReader FooEnv FooBarEnv m1 m,
  >   MappableReader BarEnv FooBarEnv m2 m,
  >   MonadState SomeState m1, MonadState SomeState m2) => m Int
  > foobar = do
  >   fooRes <- mapTReader fooEnv foo
  >   barRes <- mapTReader barEnv bar
  >   return $ fooRes + barRes
-}
{- ORMOLU_ENABLE -}
class
  (MonadReader r m, MonadReader r' m') =>
  MappableReader (r :: *) (r' :: *) (m :: * -> *) (m' :: * -> *)
    | m -> r,
      m' -> r',
      r m' -> m,
      r' m -> m'
  where
  -- | Map transformer's environment from @r@ to @r'@.
  --
  -- Default realization just maps inner monad of transformer using
  -- 'MappableTrans' instance
  mapTReader ::
    forall a.
    -- | get initial environment from mapped one
    (r' -> r) ->
    -- | initial computation
    m a ->
    -- | transformed computation
    m' a
  default mapTReader ::
    (MappableTrans t, MappableReader r r' n n', m ~ t n, m' ~ t n') =>
    forall a. (r' -> r) -> m a -> m' a
  mapTReader f = mapTrans (mapTReader f)

instance Monad m => MappableReader r r' (ReaderT r m) (ReaderT r' m) where
  mapTReader f (ReaderT ka) = ReaderT $ ka . f

instance
  (MappableReader r r' m m') =>
  MappableReader r r' (S.StateT s m) (S.StateT s m')

instance
  (MappableReader r r' m m') =>
  MappableReader r r' (L.StateT s m) (L.StateT s m')

instance
  (Monoid w, MappableReader r r' m m') =>
  MappableReader r r' (S.WriterT w m) (S.WriterT w m')

instance
  (Monoid w, MappableReader r r' m m') =>
  MappableReader r r' (L.WriterT w m) (L.WriterT w m')

instance
  (MappableReader r r' m m') =>
  MappableReader r r' (ExceptT e m) (ExceptT e m')

instance
  (MappableReader e e' m m') =>
  MappableReader e e' (MaybeT m) (MaybeT m')

instance
  (MappableReader e e' m m') =>
  MappableReader e e' (IdentityT m) (IdentityT m')
