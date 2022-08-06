module Control.Monad.State.Mappable where

import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (MonadState)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Mappable (MappableTrans (mapTrans))
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Data.Bifunctor (Bifunctor (..))

{- ORMOLU_DISABLE -}
{- | Type class for monad transformers with state, that can be mappable
  from @s@ to @s'@, changing composed monad type from m to m'.

  Usually (as in example below), @s@ is a part of @s'@ used by subcomputation

==== __Example__

  > :set -XFlexibleContexts -XNamedFieldPuns

  > import Control.Monad.State (MonadState)
  > import Control.Monad.State.Mappable (mapTState, MappableState)
  > import Control.Monad.Writer.Class (MonadWriter)
  >
  > data SomeLog
  > instance Semigroup SomeLog where (<>) = undefined
  > instance Monoid SomeLog where mempty = undefined
  >
  > data FooState
  > foo :: (MonadWriter SomeLog m, MonadState FooState m) => m Int
  > foo = undefined
  >
  > data BarState
  > bar :: (MonadWriter SomeLog m, MonadState BarState m) => m Int
  > bar = undefined
  >
  > data FooBarState = FooBarState {fooState :: FooState, barState :: BarState}
  > foobar ::
  >  (MappableState FooState FooBarState m1 m,
  >   MappableState BarState FooBarState m2 m,
  >   MonadWriter SomeLog m1, MonadWriter SomeLog m2) => m Int
  > foobar = do
  >   fooRes <- mapTState fooState (\fooState s -> s{fooState}) foo
  >   barRes <- mapTState barState (\barState s -> s{barState}) bar
  >   return $ fooRes + barRes
-}
{- ORMOLU_ENABLE -}
class
  (MonadState s m, MonadState s' m') =>
  MappableState (s :: *) (s' :: *) (m :: * -> *) (m' :: * -> *)
    | m -> s,
      m' -> s',
      s m' -> m,
      s' m -> m'
  where
  -- | Lift computation from transformer with state @s@ into transformer
  -- with state @s'@
  --
  -- Default realization just maps inner monad of transformer using
  -- 'MappableTrans' instance
  mapTState ::
    forall a.
    -- | get initial state from mapped one
    (s' -> s) ->
    -- | change mapped state with respect to changed initial state
    (s -> s' -> s') ->
    -- | initial computation
    m a ->
    -- | mapped computation
    m' a
  default mapTState ::
    (MappableTrans t, MappableState s s' n n', m ~ t n, m' ~ t n') =>
    forall a. (s' -> s) -> (s -> s' -> s') -> m a -> m' a
  mapTState get set = mapTrans (mapTState get set)

instance Monad m => MappableState s s' (S.StateT s m) (S.StateT s' m) where
  mapTState get set ma = S.StateT $ \s' ->
    fmap (second (`set` s')) (S.runStateT ma (get s'))

instance Monad m => MappableState s s' (L.StateT s m) (L.StateT s' m) where
  mapTState get set ma = L.StateT $ \s' ->
    fmap (second (`set` s')) (L.runStateT ma (get s'))

instance
  (Monoid w, MappableState s s' m m') =>
  MappableState s s' (S.WriterT w m) (S.WriterT w m')

instance
  (Monoid w, MappableState s s' m m') =>
  MappableState s s' (L.WriterT w m) (L.WriterT w m')

instance
  (MappableState s s' m m') =>
  MappableState s s' (ReaderT r m) (ReaderT r m')

instance
  (MappableState s s' m m') =>
  MappableState s s' (ExceptT e m) (ExceptT e m')

instance
  (MappableState e e' m m') =>
  MappableState e e' (MaybeT m) (MaybeT m')

instance
  (Monoid w, MappableState e e' m m') =>
  MappableState e e' (AccumT w m) (AccumT w m')

instance
  (MappableState e e' m m') =>
  MappableState e e' (IdentityT m) (IdentityT m')
