{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SeeReason.Errors.Handle
  ( oneOf
  , mapError
  , tryError
  , throwMember
  , handleError
  , liftMemberT
  , liftMember
  , catchMember
  , tryMember
  ) where

import Control.Monad.Except
import Control.Lens (Prism', prism')
import SeeReason.Errors.Types

oneOf :: (Put1 e es, Get1 e es) => Prism' (OneOf es) e
oneOf = prism' put1 get1

liftMember :: forall e (es :: [*]) m a. (Put1 e es, MonadError (OneOf es) m) => Either e a -> m a
liftMember = either throwMember return

liftMemberT :: (Put1 e es, MonadError (OneOf es) m) => ExceptT e m a -> m a
liftMemberT action = liftMember =<< runExceptT action

-- | MonadError analogue of the 'mapExceptT' function.
mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f action = f (tryError action) >>= liftEither

-- | MonadError analog to the 'try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | MonadError analog of 'Control.Exception.handle'.
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

catchMember ::
  forall e (es :: [*]) m a. (MonadError (OneOf es) (m :: * -> *), Get1 e es)
  => m a
  -> (e -> m a)
  -> m a
catchMember action handle =
  catchError action handleOneOf
  where
    handleOneOf :: OneOf es -> m a
    handleOneOf es = maybe (throwError es) handle (get1 es)

tryMember :: forall e es m a. (MonadError (OneOf es) m, Get1 e es) => m a -> m (Either e a)
tryMember action = (Right <$> action) `catchMember` (pure . Left)
