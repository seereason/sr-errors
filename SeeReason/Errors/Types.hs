{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SeeReason.Errors.Types
  ( OneOf(Empty, Val, NoVal)
  , Delete
  , Put1(put1)
  , throwMember
  , Get1(get1), Remove(remove)
  , Member
  , FindError(findError)
  , throwJust
  , ConvertError(convertError)
  , liftMembers
  , MonadHandle(handleMember)
  ) where

import Control.Exception (SomeException)
import Control.Monad.Except (ExceptT, lift, MonadError, MonadTrans,  runExceptT, throwError)
--import Data.Type.Bool
import Data.Word (Word8)
import Data.SafeCopy
import qualified Data.Serialize as S (Serialize(get, put), getWord8, Put, PutM, Get)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy
import SeeReason.Errors.Sort
  (Delete, MemberTest(Found), MemberP)

{-
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': ys) = If (MemberP x ys == 'Found) ys (x ': Nub ys)
-}

-- * OneOf

-- | Similar to a set, but can never have more than one element.
data OneOf (n :: [k]) where
  Empty :: OneOf s
  Val   :: e -> OneOf (e ': s) -- ^ One element of type e
  NoVal :: OneOf s -> OneOf (e ': s) -- ^ One element of some in s

deriving instance Typeable k => Typeable (OneOf (n :: [k]))

instance Show (OneOf '[]) where
  show Empty = "{}"

-- > runExcept (throwMember (1.5 :: Float) :: ExceptT (OneOf '[String, Float]) Identity ())
-- Left (1.5 :: Float)

instance (Show e, Typeable e, Show (OneOf s)) => Show (OneOf (e ': s)) where
  show (Val e) = "(Val (" <> show e <> " :: " <> show (typeOf e) <> "))"
  show (NoVal o) = show o
  show Empty  = "{}"

instance S.Serialize (OneOf '[]) where
  get :: S.Get (OneOf s)
  get = return Empty
  put :: OneOf s -> S.PutM ()
  put Empty = return ()
  put _ = error "fix warning"

instance (S.Serialize e, S.Serialize (OneOf s)) => S.Serialize (OneOf (e ': s)) where
  put :: OneOf (e ': s) -> S.PutM ()
  put (NoVal o) = S.put (0 :: Word8) >> S.put o
  put (Val e) = S.put (1 :: Word8) >> S.put e
  put _ = error "impossible"
  get :: S.Get (OneOf (e ': s))
  get = S.getWord8 >>= \case
    0 -> NoVal <$> S.get
    1 -> Val <$> S.get
    _ -> error "impossible"

instance (Ord (OneOf s)) => Eq (OneOf s) where
  a == b = compare a b == EQ

instance Ord (OneOf '[]) where
  compare _ _ = EQ

instance (Ord e, Ord (OneOf s)) => Ord (OneOf (e ': s)) where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Val e1) (Val e2) = compare e1 e2
  compare (Val _) _ = LT
  compare _ (Val _) = GT
  compare (NoVal o1) (NoVal o2) = compare o1 o2 -- right?

instance Typeable k => SafeCopy (OneOf ('[] :: [k])) where
  version = 1
  kind = base
  getCopy :: S.Serialize (OneOf s) => Contained (S.Get (OneOf s))
  getCopy = contain S.get
  putCopy :: S.Serialize (OneOf s) => OneOf s -> Contained S.Put
  putCopy = contain . S.put
  errorTypeName _ = "()"

instance (SafeCopy e, S.Serialize e, Typeable e,  S.Serialize (OneOf s), Typeable s) => SafeCopy (OneOf (e ': s)) where
  version = 1
  kind = base
  getCopy :: Contained (S.Get (OneOf (e ': s)))
  getCopy = contain S.get
  putCopy :: OneOf (e ': s) -> Contained S.Put
  putCopy = contain . S.put
  errorTypeName = show . typeOf

-- * Put1

class (MemberP e es ~ 'Found) => Put1 e es where
  put1 :: e -> OneOf es

instance {-# OVERLAPS #-} Put1 e (e ': xs) where
  put1 e = Val e

instance {-# OVERLAPS #-} (MemberP e (f:xs) ~ 'Found, Put1 e xs, Delete e (f:xs) ~ (f : Delete e xs)) => Put1 e (f ': xs) where
  put1 e = NoVal (put1 e)

throwMember :: forall e (es :: [*]) m a. (Put1 e es, MonadError (OneOf es) m) => e -> m a
throwMember = throwError . put1

-- * Get1

class (MemberP e es ~ 'Found) => Get1 e es where
  get1 :: OneOf es -> Maybe e

instance {-# OVERLAPS #-} Get1 e (e ': xs) where
  get1 (Val e) = Just e
  get1 (NoVal _) = Nothing
  get1 Empty = error "impossible"

instance {-# OVERLAPS #-} (MemberP e (f:xs) ~ 'Found, Get1 e xs, Delete e (f:xs) ~ (f : Delete e xs)) => Get1 e (f ': xs) where
  get1 (NoVal o) = get1 o
  get1 (Val _e) = Nothing
  get1 Empty = error "impossible"

-- * Remove

class (MemberP e es ~ 'Found) => Remove e es where
  remove :: Proxy e -> OneOf es -> OneOf (Delete e es)
instance {-# OVERLAPS #-} Remove e (e ': xs) where
  remove _ (Val _e) = Empty
  remove _ (NoVal o) = o
  remove _ Empty = Empty

instance {-# OVERLAPS #-} (MemberP e (f:xs) ~ 'Found, Remove e xs, Delete e (f:xs) ~ (f : Delete e xs)) => Remove e (f ': xs) where
  remove _p (Val v) = (Val v) -- :: OneOf (f ': (Delete e xs))
  remove p (NoVal o) = NoVal (remove p o)
  remove _p Empty = Empty

-- | This used to be a class rather than three separate classes, which
-- gave better error messages.  Might be worth switching it back.
type Member e es = (Put1 e es, Get1 e es, Remove e es)

-- * FindError

-- | Look at a SomeException and see if it can be turned into an error
-- of type es.  This is being used in cases where es is a OneOf.
class FindError es m where
  findError :: MonadError es m => (SomeException -> m a) -> SomeException -> m a

-- | No members to find, just rethrow.
instance FindError (OneOf '[]) h where
  findError rethrow e = rethrow e

-- | Helper function for building FindError instances:
--     findError rethrow e =
--       throwJust (fromException e :: Maybe ErrorCall) $
--       throwJust (fromException e :: Maybe IOException) $
--       rethrow e
throwJust :: (Put1 e es, MonadError (OneOf es) m) => Maybe e -> m a -> m a
throwJust this next = maybe next throwMember this

-- | Convert between OneOf error types, assuming that the @OneOf es@
-- is capable of representing the error that the @OneOf es'@ argument
-- contains.  It should be possible to write this for any error types,
-- but I haven't managed it.  Therefore I write custom instances as
-- needed.
class ConvertError old new where
  convertError :: OneOf old -> OneOf new

-- | Simple case
instance ConvertError es' '[] where
  convertError _ = Empty

instance ConvertError es es where
  convertError es = es

-- | Error set version of 'Control.Monad.Except.liftEither'.
liftMembers ::
  (MonadError (OneOf e2) m, ConvertError e1 e2)
  => Either (OneOf e1) a
  -> m a
liftMembers (Left e) = throwError (convertError e)
liftMembers (Right a) = pure a

-- | Handle one of the errors in set es.
class MonadHandle es t where
  handleMember ::
    forall e t' a m. (MonadTrans t',
                      Monad m,
                      Put1 e es, Get1 e es, Remove e es,
                      ConvertError es (Delete e es),
                      MonadError (OneOf es) (t m),
                      MonadError (OneOf (Delete e es)) (t' m))
    => t m a -> t' m (Either e a)

instance MonadHandle es (ExceptT (OneOf es)) where
  handleMember action =
    lift (runExceptT action) >>= \case
      Left es -> maybe (throwError (convertError es)) (pure . Left) (get1 es)
      Right a -> pure (Right a)
