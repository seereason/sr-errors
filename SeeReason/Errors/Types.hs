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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SeeReason.Errors.Types
  ( OneOf(Empty, Val, NoVal)
  , DeleteList
  , Set(set)
  , throwMember
  , Get(get), Delete(delete)
  , Member
  , FindError(findError)
  , throwJust
  , ConvertErrors(convertErrors)
  , MonadHandle(handleMember)
  ) where

import Control.Exception (SomeException)
import Control.Monad.Except (ExceptT, lift, MonadError, MonadTrans,  runExceptT, throwError)
import Data.Type.Bool
import Data.Word (Word8)
import Data.SafeCopy
import qualified Data.Serialize as S (Serialize(get, put), getWord8, Put, PutM, Get)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

data MemberTest
  = NotFound ErrorMessage
  | Found

type family IsMember x ys where
  IsMember x '[] = 'NotFound ('Text "Not found: " ':<>: 'ShowType x)
  IsMember x (x ': ys) = 'Found
  IsMember x (y ': ys) = IsMember x ys
  IsMember x ys = IsMember x ys

type family IsJust x where
  IsJust ('Just x) = 'True
  IsJust 'Nothing = 'False

type family Nub xs where
  Nub '[] = '[]
  Nub (x ': ys) = If (IsMember x ys == 'Found) ys (x ': Nub ys)

data OneOf (n :: [k]) where
  Empty :: OneOf s
  Val   :: e -> OneOf (e ': s)
  NoVal :: OneOf s -> OneOf (e ': s)

deriving instance Typeable k => Typeable (OneOf (n :: [k]))

instance Show (OneOf '[]) where
  show Empty = "{}"

type family DeleteList e xs where
  DeleteList x '[] = '[]
  DeleteList x (x ': ys) = ys
  DeleteList x (y ': ys) = (y ': (DeleteList x ys))

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

class (IsMember e es ~ 'Found) => Set e es where
  set :: e -> OneOf es

instance {-# OVERLAPS #-} Set e (e ': xs) where
  set e = Val e

instance {-# OVERLAPS #-} (IsMember e (f:xs) ~ 'Found, Set e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Set e (f ': xs) where
  set e = NoVal (set e)

throwMember :: forall e (es :: [*]) m a. (Set e es, MonadError (OneOf es) m) => e -> m a
throwMember = throwError . set

class (IsMember e es ~ 'Found) => Get e es where
  get :: OneOf es -> Maybe e

class (IsMember e es ~ 'Found) => Delete e es where
  delete :: Proxy e -> OneOf es -> OneOf (DeleteList e es)

instance {-# OVERLAPS #-} Get e (e ': xs) where
  -- set e = Val e
  get (Val e) = Just e
  get (NoVal _) = Nothing
  get Empty = error "impossible"

instance {-# OVERLAPS #-} Delete e (e ': xs) where
  delete _ (Val _e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty

instance {-# OVERLAPS #-} (IsMember e (f:xs) ~ 'Found, Get e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Get e (f ': xs) where
  -- set e = NoVal (set e)
  get (NoVal o) = get o
  get (Val _e) = Nothing
  get Empty = error "impossible"

instance {-# OVERLAPS #-} (IsMember e (f:xs) ~ 'Found, Delete e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Delete e (f ': xs) where
  delete _p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
  delete p (NoVal o) = NoVal (delete p o)
  delete _p Empty = Empty

{-
type Member e es = (IsMember e es ~ 'Found, Get e es, Set e es, Delete e es)

class (IsMember e es ~ 'Found) => Set e es where
  set :: e -> OneOf xs

instance Set e (e ': xs) where
  set e = Val e

instance {-# OVERLAPS #-} (IsMember e xs ~ 'Found, Set e xs) => Set e (f ': xs) where
  set e = NoVal (set e)

class Get e xs where
  get :: OneOf xs -> Maybe e

instance {-# OVERLAPS #-} Get e (e ': xs) where
  get (Val e) = Just e
  get (NoVal _) = Nothing
  get Empty = error "impossible"

instance (IsMember e xs ~ 'Found, Get e xs) => Get e (f ': xs) where
  get (NoVal o) = get o
  get (Val _e) = Nothing
  get Empty = error "impossible"

class Delete e xs where
  delete :: Proxy e -> OneOf xs -> OneOf (DeleteList e xs)

instance Delete e (e ': xs) where
  delete _ (Val _e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty

instance {-# OVERLAPS #-} forall e f xs. (Delete e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Delete e (f ': xs) where
   delete _p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
   delete p (NoVal o) = NoVal (delete p o)
   delete _p Empty = Empty
-}

type Member e es = (Set e es, Get e es, Delete e es)

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
throwJust :: (Set e es, MonadError (OneOf es) m) => Maybe e -> m a -> m a
throwJust this next = maybe next throwMember this

-- | Convert between OneOf error types, assuming that the @OneOf es@
-- is capable of representing the error that the @OneOf es'@ argument
-- contains.  It should be possible to write this for any error types,
-- but I haven't managed it.  Therefore I write custom instances as
-- needed.
class ConvertErrors es es' where
  convertErrors :: OneOf es' -> OneOf es

-- | Simple case
instance ConvertErrors '[] es' where
  convertErrors _ = Empty

instance ConvertErrors es es where
  convertErrors es = es

-- | Handle one of the errors in set es.
class MonadHandle es t where
  handleMember ::
    forall e t' a m. (MonadTrans t',
                      Monad m,
                      Set e es, Get e es, Delete e es,
                      ConvertErrors (DeleteList e es) es,
                      MonadError (OneOf es) (t m),
                      MonadError (OneOf (DeleteList e es)) (t' m))
    => t m a -> t' m (Either e a)

instance MonadHandle es (ExceptT (OneOf es)) where
  handleMember action =
    lift (runExceptT action) >>= \case
      Left es -> maybe (throwError (convertErrors es)) (pure . Left) (get es)
      Right a -> pure (Right a)
