module Control.Monad.Writer.Mappable where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
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
  mapWriter :: forall a. (w -> w') -> m a -> m' a
  default mapWriter ::
    (MappableTrans t, MappableWriter w w' n n', m ~ t n, m' ~ t n') =>
    forall a. (w -> w') -> m a -> m' a
  mapWriter f = mapTrans (mapWriter f)

instance
  (Monoid w, Monoid w', Monad m) =>
  MappableWriter w w' (S.WriterT w m) (S.WriterT w' m)
  where
  mapWriter f = S.mapWriterT (fmap $ second f)

instance
  (Monoid w, Monoid w', Monad m) =>
  MappableWriter w w' (L.WriterT w m) (L.WriterT w' m)
  where
  mapWriter f = L.mapWriterT (fmap $ second f)

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
