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

module SeeReason.UIO {-# DEPRECATED "Retiring the unexceptional packages" #-}
  ( NonIOException
  , NonIOException'(NonIOException)
  , HasNonIOException'(nonIOException)
  , HasNonIOException
  , liftUIO
  , liftUIO'
  , runExceptUIO
  , module UnexceptionalIO.Trans
  ) where

import SeeReason.Errors
import Control.Exception (fromException, IOException, toException)
import Control.Lens (preview, Prism', prism', review)
import Control.Monad.Except (ap, catchError, Except, ExceptT, liftEither, mapExceptT, MonadError,
                             MonadIO, runExcept, runExceptT, throwError, withExceptT)
import Data.Coerce (coerce, Coercible)
import Data.Data (Data)
import Data.Type.Bool
--import Data.Type.Equality
import Data.Word (Word8)
import Data.SafeCopy
import qualified Data.Serialize as S (Serialize(get, put), getWord8, Put, PutM, Get)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy
import Debug.Trace (trace)
import Data.Type.Equality
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits
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
  runExceptT (fromIO io) >>= either (either throwMember throwMember . splitException) return
  where
    splitException e = maybe (Left (NonIOException (f e))) Right (fromException (toException e) :: Maybe IOException)

liftUIO ::
  (Unexceptional m, Member NonIOException e, Member IOException e, MonadError (OneOf e) m)
  => IO a
  -> m a
liftUIO = liftUIO' show

-- | Version of runExceptT that merges the error back into the result
-- value rather than rethrowing it.
runExceptUIO :: MonadIO m => (e -> a) -> ExceptT e UIO a -> m a
runExceptUIO f io = either f id <$> run (runExceptT io)
