{- ORMOLU_DISABLE -}
{- |
Module       : Control.Monad.Trans.Mappable
Copyright    : (c) Lev Dvorkin, 2022
License      : MIT
Maintainer   : lev_135@mail.ru
Stability    : Experimental

This module contains type class for transformers with mappable inner monad.
It is used for default instances of Mappable classes
-}
{- ORMOLU_ENABLE -}
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

-- | Functor class for 'MonadTrans'.
-- As it is marked in 'Control.Monad.Trans.Class'
-- transformers define a mapping from transformations between base monads
-- to transformations between transformed monads called @mapXXXT@.
-- However, transformers package doesn't provide any class to work with
-- this transformation in an abstract way. 'MappableTrans' allows it
class (MonadTrans t) => MappableTrans t where
  mapTrans ::
    forall m m' a.
    -- | Transformation in inner monad. We are not able to fix @x@ type
    -- here, because should be different, depending on transformer's type
    -- (just @a@ for @ReaderT@, pairs for @StateT@ and @WriterT@ etc.)
    (forall x. m x -> m' x) ->
    t m a ->
    t m' a

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
