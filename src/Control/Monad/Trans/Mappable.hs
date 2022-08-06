module Control.Monad.Trans.Mappable where

import Control.Monad.Identity (IdentityT, mapIdentityT)
import Control.Monad.Reader (ReaderT, mapReaderT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S

class (MonadTrans t) => MappableTrans t where
  mapTrans :: forall m m' a. Monad m => (forall x. m x -> m' x) -> t m a -> t m' a

instance MappableTrans (S.StateT s) where
  mapTrans = S.mapStateT

instance MappableTrans (L.StateT s) where
  mapTrans = L.mapStateT

instance Monoid w => MappableTrans (S.WriterT w) where
  mapTrans = S.mapWriterT

instance Monoid w => MappableTrans (L.WriterT w) where
  mapTrans = L.mapWriterT

instance MappableTrans (ReaderT r) where
  mapTrans = mapReaderT

instance MappableTrans (ExceptT e) where
  mapTrans = mapExceptT

instance MappableTrans MaybeT where
  mapTrans = mapMaybeT

instance MappableTrans IdentityT where
  mapTrans = mapIdentityT
