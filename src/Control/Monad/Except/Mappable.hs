module Control.Monad.Except.Mappable where

import Control.Monad.Except (ExceptT, MonadError, mapExceptT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Data.Bifunctor (first)

class
  (MonadError e m, MonadError e' m') =>
  MappableError (e :: *) (e' :: *) (m :: * -> *) (m' :: * -> *)
    | m -> e,
      m' -> e',
      e m' -> m,
      e' m -> m'
  where
  mapError :: forall a. (e -> e') -> m a -> m' a
  default mapError ::
    (MappableTrans t, MappableError e e' n n', m ~ t n, m' ~ t n') =>
    forall a. (e -> e') -> m a -> m' a
  mapError f = mapTrans (mapError f)

instance Monad m => MappableError e e' (ExceptT e m) (ExceptT e' m) where
  mapError f = mapExceptT (fmap $ first f)

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
