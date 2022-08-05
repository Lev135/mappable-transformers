module Control.Monad.Writer.Mappable where

import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Data.Bifunctor (second)

class
  (MonadWriter w m, MonadWriter w' m') =>
  MappableWriter (w :: *) (w' :: *) (m :: * -> *) (m' :: * -> *)
    | m -> w,
      m' -> w',
      w m' -> m,
      w' m -> m'
  where
  mapTWriter :: forall a. (w -> w') -> m a -> m' a
  default mapTWriter ::
    (MappableTrans t, MappableWriter w w' n n', m ~ t n, m' ~ t n') =>
    forall a. (w -> w') -> m a -> m' a
  mapTWriter f = mapTrans (mapTWriter f)

instance
  (Monoid w, Monoid w', Monad m) =>
  MappableWriter w w' (S.WriterT w m) (S.WriterT w' m)
  where
  mapTWriter f = S.mapWriterT (fmap $ second f)

instance
  (Monoid w, Monoid w', Monad m) =>
  MappableWriter w w' (L.WriterT w m) (L.WriterT w' m)
  where
  mapTWriter f = L.mapWriterT (fmap $ second f)

instance
  (MappableWriter w w' m m') =>
  MappableWriter w w' (S.StateT s m) (S.StateT s m')

instance
  (MappableWriter w w' m m') =>
  MappableWriter w w' (L.StateT s m) (L.StateT s m')

instance
  (MappableWriter w w' m m') =>
  MappableWriter w w' (ReaderT r m) (ReaderT r m')

instance
  (MappableWriter w w' m m') =>
  MappableWriter w w' (ExceptT e m) (ExceptT e m')

instance
  (MappableWriter e e' m m') =>
  MappableWriter e e' (MaybeT m) (MaybeT m')

instance
  (MappableWriter e e' m m') =>
  MappableWriter e e' (IdentityT m) (IdentityT m')
