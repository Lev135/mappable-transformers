module Control.Monad.Reader.Mappable where

import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S

class
  (MonadReader r m, MonadReader r' m') =>
  MappableReader (r :: *) (r' :: *) (m :: * -> *) (m' :: * -> *)
    | m -> r,
      m' -> r',
      r m' -> m,
      r' m -> m'
  where
  mapTReader :: forall a. (r' -> r) -> m a -> m' a
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
  (Monoid w, MappableReader e e' m m') =>
  MappableReader e e' (AccumT w m) (AccumT w m')

instance
  (MappableReader e e' m m') =>
  MappableReader e e' (IdentityT m) (IdentityT m')
