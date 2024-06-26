{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SeeReason.Errors
  ( OneOf(Empty, Val, NoVal)
  , oneOf
  , Member(set, get, delete)
  , tryError
  , handleError
  , mapError
  , FindError(findError)
  , throwJust
  , traceOver
  , liftExceptT
  , liftMemberT
  , liftMember
  , tryMemberOld2
  , tryMember
  , tryMemberNew
  , catchMemberOld2
  , catchMember
  , catchMemberNew
  , throwMember
  , mapMember
  , runOneOf
  , runNullExceptT
  , runNullExcept
  , test
  ) where

import Control.Exception (SomeException)
import Control.Lens (preview, Prism', prism', review)
import Control.Monad.Except (ap, catchError, Except, ExceptT, liftEither, mapExceptT, MonadError,
                             runExcept, runExceptT, throwError, withExceptT)
import Data.Coerce (coerce, Coercible)
import Data.Type.Bool
import Data.Word (Word8)
import Data.SafeCopy
import qualified Data.Serialize as S (Serialize(get, put), getWord8, Put, PutM, Get)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy
import Debug.Trace (trace)
import Data.Type.Equality
import GHC.Stack (callStack, HasCallStack)
import GHC.TypeLits

-- | MonadError analog to the 'try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | MonadError analog of 'Control.Exception.handle'.
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

-- | MonadError analogue of the 'mapExceptT' function.
mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f action = f (tryError action) >>= liftEither

traceOver :: Show a => (String -> String) -> a -> a
traceOver f a = trace (f (show a)) a

#if 0
-- Omitted to avoid dependency on sr-extra.
decodeM' :: forall a e m. (MonadError (OneOf e) m, Member DecodeError e, Serialize a, Typeable a) => ByteString -> m a
decodeM' bs = liftMember =<< tryError (decodeM bs)
#endif

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

--type family Member x es where
--  Member' x (OneOf xs) = Member' x xs

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

type family DeleteOneOf e xs where
  DeleteOneOf x (OneOf ys) = OneOf (DeleteList x ys)

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

#if 1
class (IsMember e es ~ 'Found) => Member e es where
  set :: e -> OneOf es
  get :: OneOf es -> Maybe e
  delete :: Proxy e -> OneOf es -> DeleteOneOf e (OneOf es)

instance {-# OVERLAPS #-} Member e (e ': xs) where
  set e = Val e
  get (Val e) = Just e
  get (NoVal _) = Nothing
  get Empty = error "impossible"
  delete _ (Val _e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty

instance {-# OVERLAPS #-} (IsMember e (f:xs) ~ 'Found, Member e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Member e (f ': xs) where
  set e = NoVal (set e)
  get (NoVal o) = get o
  get (Val _e) = Nothing
  get Empty = error "impossible"
  delete _p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
  delete p (NoVal o) = NoVal (delete p o)
  delete _p Empty = Empty

#else
type Member e es = (IsMember e es ~ 'Found, Get e es, Set e es, Delete e es)

class (IsMember e es ~ 'Found) => Set e xs where
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
  delete :: Proxy e -> OneOf xs -> DeleteOneOf e (OneOf xs)

instance Delete e (e ': xs) where
  delete _ (Val _e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty

instance {-# OVERLAPS #-} forall e f xs. (Delete e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Delete e (f ': xs) where
   delete _p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
   delete p (NoVal o) = NoVal (delete p o)
   delete _p Empty = Empty
#endif

oneOf :: (Member e es) => Prism' (OneOf es) e
oneOf = prism' set get

throwMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => e -> m a
throwMember = throwError . review oneOf

liftMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => Either e a -> m a
liftMember = either throwMember return

-- | Should be liftMemberT
liftExceptT :: (Member e es, MonadError (OneOf es) m) => ExceptT e m a -> m a
liftExceptT = liftMemberT
{-# DEPRECATED liftExceptT "Renamed liftMemberT" #-}

liftMemberT :: (Member e es, MonadError (OneOf es) m) => ExceptT e m a -> m a
liftMemberT action = liftMember =<< runExceptT action

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
throwJust :: (Member e es, MonadError (OneOf es) m) => Maybe e -> m a -> m a
throwJust this next = maybe next throwMember this

-- | Run an action with @e@ added to the current error set @es@.
-- Typically this is used by forcing the action into ExceptT with
-- the augmented error type:
-- @@
--   catchMemberOld withExceptT (fileIO @(FileError ': e) (Right <$> query st (LookValue key))) (return . Left)
-- @@
catchMemberOld ::
  forall esplus e es m n a.
  (Member e esplus, es ~ DeleteList e esplus,
   MonadError (OneOf esplus) m, MonadError (OneOf es) n)
  => (forall b. (OneOf esplus -> OneOf es) -> m b -> n b)
  -> m a -> (e -> n a) -> n a
catchMemberOld helper ma f =
  -- The idea here is that we use tryError to bring a copy of e into
  -- the return value, then we can just delete e from error monad.
  helper (delete @e Proxy) (tryError ma) >>= either handle return
  where handle :: OneOf esplus -> n a
        handle es = maybe (throwError (delete @e Proxy es)) f (get es :: Maybe e)

{-
catchMember'' ::
  forall e (es :: [*]) m a.
  (Monad m)
  => ExceptT (OneOf (e ': es)) m a
  -> ExceptT (OneOf es) m (Either e a)
catchMember'' m = do
  lift (runExceptT m) >>= either (\es -> maybe (throwError (delete (Proxy @e) es :: OneOf es)) (pure . Left) (preview oneOf es)) (pure . Right)

-- FFS I just rewrote this.
catchMember' ::
  forall e (es :: [*]) m a.
  (MonadError (OneOf es) m)
  => ExceptT (OneOf (e ': es)) m a
  -> (e -> m a)
  -> m a
catchMember' action handle =
  either throwError (either handle pure) =<< runExceptT (catchMember'' @e action)
-}

{-
type family IsSubset xs ys where
  IsSubset '[] _ = 'True
  IsSubset xs '[] = 'False
  IsSubset (x ': xs) ys = (IsMember x ys) && (IsSubset xs ys)

liftExceptTs :: (IsSubset es' es ~ 'True, MonadError (OneOf es) m) => ExceptT (OneOf es') m a -> m a
liftExceptTs action = liftMembers =<< runExceptT action

liftMembers :: forall es' es m a. (IsSubset es' es ~ 'True, MonadError (OneOf es) m) => Either (OneOf es') a -> m a
liftMembers = either throwMembers return

-- Special case of what we would really like.
throwMembers :: forall e es' es m a. (IsSubset es' es ~ 'True, MonadError (OneOf es) m) => OneOf es' -> m a
throwMembers Empty = error "throwMembers"
throwMembers (Val e) = throwError (review oneOf e :: OneOf es)
throwMembers (NoVal o) = throwMembers o
-}

-- | @m@ and @m2@ are the same type, but only @m@ has the constraint
-- @Member e es@.  This means code that calls 'tryMemberNew' will not
-- have that constraint.
tryMemberNew ::
  forall e m2 m es es2 a.
  (MonadError (OneOf es) m,
   MonadError (OneOf es2) m2,
   Member e es,
   Coercible (OneOf es) (OneOf es2),
   Coercible m m2,
   HasCallStack)
  => m a
  -> m2 (Either e a)
tryMemberNew ma =
  m2 (either (Left . e2) Right <$> tryError ma) >>=
    either (\(es :: OneOf es) -> case preview (oneOf :: Prism' (OneOf es) e) es of
                                   Nothing -> throwError (coerce es :: OneOf es2)
                                   Just e -> pure $ Left e)
           (\a -> pure (Right a))
  where
    m2 :: m (Either (OneOf es) a) -> m2 (Either (OneOf es2) a)
    m2 = coerce
    e2 :: OneOf es -> OneOf es2
    e2 = coerce
    _ = callStack
{-# WARNING tryMemberNew "Experimental" #-}

catchMemberNew ::
  forall e m2 m es es2 a.
  (Member e es,
   MonadError (OneOf es) m,
   MonadError (OneOf es2) m2,
   Coercible m m2,
   Coercible (OneOf es) (OneOf es2),
   HasCallStack)
  => m a
  -> (e -> m a)
  -> m2 a
catchMemberNew ma handler =
  either (\e -> m2 (handler e)) pure =<< tryMemberNew ma
  where
    m2 :: m a -> m2 a
    m2 = coerce
{-# WARNING catchMemberNew "Experimental" #-}

-- | Run an action in monad @ExceptT (OneOf (e ': es)) m@, where @m@ has error type @OneOf es@.
-- This is essentially eliminating one of the error types from the action parameter.
tryMemberOld2 :: forall e es m a. (MonadError (OneOf es) m) => ExceptT (OneOf (e ': es)) m a -> m (Either e a)
tryMemberOld2 ma = either throwError pure =<< (runExceptT @(OneOf es) $ dropMember @e $ tryMemberOld @e ma)

catchMemberOld2 :: forall e es m a. (MonadError (OneOf es) m) => ExceptT (OneOf (e ': es)) m a -> (e -> m a) -> m a
catchMemberOld2 ma handler = either handler pure =<< tryMemberOld2 @e ma

catchMember ::
  forall e (es :: [*]) m a. (MonadError (OneOf es) (m :: * -> *), Member e es)
  => m a
  -> (e -> m a)
  -> m a
catchMember action handle =
  catchError action handleOneOf
  where
    handleOneOf :: OneOf es -> m a
    handleOneOf es = maybe (throwError es) handle (get es)

tryMember :: forall e es m a. (MonadError (OneOf es) m, Member e es) => m a -> m (Either e a)
tryMember action = (Right <$> action) `catchMember` (pure . Left)

-- | Simplified (and actually usable) 'catchMember' where the monad
-- doesn't change.  The result will have the same error typed, but it
-- can be assumed that the value of the error type is not @e@.  So you
-- could then use dropMember to eliminate that error if you know
-- enough about the error type.
-- @@
-- λ> runExceptT (tryMember @Char @'[Char, Int] (pure "ok"))
-- Right (Right "ok")
-- λ> runExceptT (tryMember @Char @'[Char, Int] (throwMember 'x'))
-- Right (Left 'x')
-- λ> runExceptT (tryMember @Char @'[Char, Int] (throwMember (3 :: Int)))
-- Left (Val (3 :: Int))
-- λ> :type dropMember @Char @Identity (tryMember @Char @'[Char, Int] (throwMember (3 :: Int)))
-- ExceptT (OneOf '[Int]) Identity (Either Char a)
-- @@
tryMemberOld :: forall e es m a. (MonadError (OneOf es) m, Member e es) => m a -> m (Either e a)
tryMemberOld ma = tryMemberOlder (Right <$> ma) (pure . Left)

-- λ> runExceptT (tryMember' @Char @String @'[Char, Int] (pure "ok") (\c -> pure ("Caught " <> show c)))
-- Right "ok"
-- λ> runExceptT (tryMember' @Char @String @'[Char, Int] (throwMember 'x') (\c -> pure ("Caught " <> show c)))
-- Right "Caught 'x'"
-- λ> runExceptT (tryMember' @Char @String @'[Char, Int] (throwMember (3 :: Int)) (\c -> pure ("Caught " <> show c)))
-- Left 3
tryMemberOlder :: forall e a es m. (Member e es, MonadError (OneOf es) m) => m a -> (e -> m a) -> m a
tryMemberOlder ma f = tryError ma >>= either (\es -> maybe (throwError es) f (get es :: Maybe e)) return

-- | Annotate a member error that has been thrown.
mapMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => (e -> m e) -> m a -> m a
mapMember f ma =
  tryError ma >>= either (\es -> maybe (throwError es) (\e -> f e >>= throwMember) (get es :: Maybe e)) return

-- | Discard the head error type from a OneOf error.
dropMember :: forall e m es a. Monad m => ExceptT (OneOf (e : es)) m a -> ExceptT (OneOf es) m a
dropMember ma = mapExceptT (ap (pure (either (Left . dropMember' @e) Right))) ma

dropMember' :: OneOf (e ': es) -> OneOf es
dropMember' Empty = Empty
dropMember' (Val _) = Empty
dropMember' (NoVal es) = es

runNullExceptT :: Functor m => ExceptT (OneOf '[]) m a -> m a
runNullExceptT m = (\(Right a) -> a) <$> runExceptT m

runNullExcept :: Except (OneOf '[]) a -> a
runNullExcept m = (\(Right a) -> a) (runExcept m)

-- | Catch any FileError thrown and put it in the return value.
runOneOf'' ::
  forall (esplus :: [*]) e (es :: [*]) m a.
  (es ~ DeleteList e esplus, Member e esplus, Monad m)
  => ExceptT (OneOf esplus) m a
  -> ExceptT (OneOf es) m (Either e a)
runOneOf'' action = catchMemberOld withExceptT (Right <$> action) (return . Left)

runOneOf' ::
  forall (esplus :: [*]) e (es :: [*]) m a r.
  (es ~ DeleteList e esplus, Member e esplus, MonadError (OneOf es) m)
  => ExceptT (OneOf esplus) m a
  -> (Either e a -> m r)
  -> m r
runOneOf' action final = runExceptT (runOneOf'' action) >>= either throwError final

runOneOf ::
  forall (esplus :: [*]) e (es :: [*]) m a.
  (es ~ DeleteList e esplus, Member e esplus, MonadError (OneOf es) m)
  => ExceptT (OneOf esplus) m a
  -> m (Either e a)
runOneOf action = runOneOf' action return
{-# DEPRECATED runOneOf "Use tryMember" #-}

-- ** Example

data ErrorFoo = Foo deriving Show
data ErrorBar = Bar deriving Show
data ErrorBaz = Baz deriving Show

type AppErrors = OneOf '[ErrorFoo, ErrorBar, ErrorBaz]

handleBar :: (Member ErrorBar errors) => OneOf errors -> IO (DeleteOneOf ErrorBar (OneOf errors))
handleBar err =
  do case get err of
       Nothing -> putStrLn "no Bar error to handle"
       (Just Bar) -> putStrLn "took care of Bar"
     pure (delete @ErrorBar Proxy err)

handleFoo :: Member ErrorFoo errors => OneOf errors -> IO (DeleteOneOf ErrorFoo (OneOf errors))
handleFoo err =
  do case get err of
       Nothing -> putStrLn "no Bar error to handle"
       (Just Foo) -> putStrLn "took care of Bar"
     pure (delete @ErrorFoo Proxy err)

{-
Generates the ouput:

current error = Foo
no Bar error to handle
current error = Foo
took care of Bar
current error = {}
-}
test :: IO ()
test =
  do let errAll = set Foo :: AppErrors
     putStrLn $ "current error = " ++ show errAll

     errNoBar <- handleBar errAll
     putStrLn $ "current error = " ++ show errNoBar

     errNoFoo <- handleFoo errNoBar
     putStrLn $ "current error = " ++ show errNoFoo

     -- this will not compile because ErrorFoo has already been handled
--     errNoFoFoo <- handleFoo errNoFoo

     pure ()
