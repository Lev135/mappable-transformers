module Control.Monad.Except.Mappable where

import Control.Monad.Except (MonadError)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Data.Bifunctor (first)

{- ORMOLU_DISABLE -}
{- | Type class for monad transformers with errors, that can be mappable
  from @e@ to @e'@, changing composed monad type from m to m'.

  Usually (as in example below), @e'@ is a sum type contains @e@ as one of
  possible error message types

==== __Example__

  > :set -XFlexibleContexts

  > import Control.Monad.Except (MonadError)
  > import Control.Monad.Except.Mappable (mapTError, MappableError)
  > import Control.Monad.State.Class (MonadState)
  >
  > data SomeState
  >
  > data FooErr
  > foo :: (MonadState SomeState m, MonadError FooErr m) => m Int
  > foo = undefined
  >
  > data BarErr
  > bar :: (MonadState SomeState m, MonadError BarErr m) => m Int
  > bar = undefined
  >
  > data FooOrBarErr = FooErr FooErr | BarErr BarErr
  > foobar ::
  >  (MappableError FooErr FooOrBarErr m1 m,
  >   MappableError BarErr FooOrBarErr m2 m,
  >   MonadState SomeState m1, MonadState SomeState m2) => m Int
  > foobar = do
  >   fooRes <- mapTError FooErr foo
  >   barRes <- mapTError BarErr bar
  >   return $ fooRes + barRes
-}
{- ORMOLU_ENABLE -}
class
  (MonadError e m, MonadError e' m') =>
  MappableError (e :: *) (e' :: *) (m :: * -> *) (m' :: * -> *)
    | m -> e,
      m' -> e',
      e m' -> m,
      e' m -> m'
  where
  -- | Map transformer's error from @e@ to @e'@.
  --
  -- Default realization just maps inner monad of transformer using
  -- 'MappableTrans' instance
  mapTError ::
    forall a.
    -- | map error value
    (e -> e') ->
    -- | initial computation
    m a ->
    -- | mapped computation
    m' a
  default mapTError ::
    (MappableTrans t, MappableError e e' n n', m ~ t n, m' ~ t n') =>
    forall a. (e -> e') -> m a -> m' a
  mapTError f = mapTrans (mapTError f)

instance Monad m => MappableError e e' (ExceptT e m) (ExceptT e' m) where
  mapTError f = mapExceptT (fmap $ first f)

instance
  (MappableError e e' m m') =>
  MappableError e e' (S.StateT s m) (S.StateT s m')

instance
  (MappableError e e' m m') =>
  MappableError e e' (L.StateT s m) (L.StateT s m')

instance
  (Monoid w, MappableError e e' m m') =>
  MappableError e e' (S.WriterT w m) (S.WriterT w m')

instance
  (Monoid w, MappableError e e' m m') =>
  MappableError e e' (L.WriterT w m) (L.WriterT w m')

instance
  (MappableError e e' m m') =>
  MappableError e e' (ReaderT w m) (ReaderT w m')

instance
  (MappableError e e' m m') =>
  MappableError e e' (MaybeT m) (MaybeT m')

instance
  (Monoid w, MappableError e e' m m') =>
  MappableError e e' (AccumT w m) (AccumT w m')

instance
  (MappableError e e' m m') =>
  MappableError e e' (IdentityT m) (IdentityT m')
