{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -ddump-minimal-imports #-}

module SeeReason.UIO
  ( NonIOException
  , NonIOException'(NonIOException)
  , HasNonIOException'(nonIOException)
  , HasNonIOException
  , liftUIO
  , liftUIO'
  , runExceptUIO
  , splitException
  , splitExceptT
  , fromExceptT
  , module UnexceptionalIO.Trans
  ) where

import SeeReason.Errors.Handle (oneOf, throwMember)
import SeeReason.Errors.Types (Member, OneOf)
import Control.Exception (fromException, IOException, toException)
import Control.Lens (Prism', review)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError, withExceptT)
import Control.Monad.Trans (MonadIO)
import Data.Data (Data)
import qualified Data.Serialize as S (Serialize)
import GHC.Generics
import UnexceptionalIO.Trans -- (fromIO, run, SomeNonPseudoException, UIO, Unexceptional)

-- | If 'fromIO' throws a SomeNonPseudoException, 'splitException'
-- decides whether it was an 'IOException' or something else, this
-- wrapper indicates it was something else.
newtype NonIOException' a = NonIOException a {-SomeNonPseudoException-} deriving (Eq, Ord, Generic, Data, Show, S.Serialize, Functor)
type NonIOException = NonIOException' String
class HasNonIOException' a e where nonIOException :: Prism' e (NonIOException' a)
instance HasNonIOException' a (NonIOException' a) where nonIOException = id
type HasNonIOException e = HasNonIOException' String e

liftUIO' ::
  (Unexceptional m,
   Member (NonIOException' s) e,
   Member IOException e,
   MonadError (OneOf e) m)
  => (SomeNonPseudoException -> s)
  -> IO a
  -> m a
liftUIO' f io =
  runExceptT (fromIO io) >>= either (either throwMember throwMember . splitException f) return

liftUIO ::
  (Unexceptional m, Member NonIOException e, Member IOException e, MonadError (OneOf e) m)
  => IO a
  -> m a
liftUIO = liftUIO' show

-- | Version of runExceptT that merges the error back into the result
-- value rather than rethrowing it.
runExceptUIO :: MonadIO m => (e -> a) -> ExceptT e UIO a -> m a
runExceptUIO f io = either f id <$> run (runExceptT io)

-- splitException :: (Member NonIOException e, Member IOException e) => (SomeNonPseudoException -> s) -> IOException -> OneOf e
splitException ::
  forall a.
     (SomeNonPseudoException -> a)
  -> SomeNonPseudoException
  -> Either (NonIOException' a) IOException
splitException f e =
  maybe
    (Left (NonIOException (f e)))
    Right
    (fromException (toException e) :: Maybe IOException)

splitExceptT ::
  (Member IOException e, Member NonIOException e, Monad m)
  => ExceptT SomeNonPseudoException m a
  -> ExceptT (OneOf e) m a
splitExceptT f =
  withExceptT (either (review oneOf) (review oneOf) . splitException show) f

fromExceptT ::
  (Unexceptional m, Member IOException e, Member NonIOException e)
  => ExceptT (OneOf e) IO a
  -> ExceptT (OneOf e) m a
fromExceptT action =
  either throwError (either throwError pure)
    =<< (lift $ runExceptT $ splitExceptT $ fromIO $ runExceptT $ action)
