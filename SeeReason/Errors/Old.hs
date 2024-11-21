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

module SeeReason.Errors.Old {-# DEPRECATED "Inline all" #-} where

import Control.Lens (preview, Prism')
import Control.Monad.Except
  (ap, Except, ExceptT, liftEither, mapExceptT, MonadError,
   runExcept, runExceptT, throwError, withExceptT)
import Data.Coerce (coerce, Coercible)
import Data.Proxy
import Debug.Trace (trace)
import GHC.Stack (callStack, HasCallStack)
import SeeReason.Errors.Handle (oneOf, throwMember, liftMemberT, tryError)
import SeeReason.Errors.Types (Get1(get1), Put1(put1), Remove(remove), Delete, OneOf(Empty, Val, NoVal))

-- | Absorb an ExceptT e' action into another MonadError instance.
withError :: MonadError e m => (e' -> e) -> ExceptT e' m a -> m a
withError f action = runExceptT (withExceptT f action) >>= liftEither

-- | Modify the value (but not the type) of an error
withError' :: MonadError e m => (e -> e) -> m a -> m a
withError' f action = tryError action >>= either (throwError . f) return
{-# DEPRECATED withError' "withError might to be able to do this job" #-}

traceOver :: Show a => (String -> String) -> a -> a
traceOver f a = traceWith (f . show) a

-- Added to base-4.18
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

-- | Should be liftMemberT
liftExceptT :: (Put1 e es, MonadError (OneOf es) m) => ExceptT e m a -> m a
liftExceptT = liftMemberT
{-# DEPRECATED liftExceptT "Renamed liftMemberT" #-}

-- | Run an action with @e@ added to the current error set @es@.
-- Typically this is used by forcing the action into ExceptT with
-- the augmented error type:
-- @@
--   catchMemberOld withExceptT (fileIO @(FileError ': e) (Right <$> query st (LookValue key))) (return . Left)
-- @@
catchMemberOld ::
  forall esplus e es m n a.
  (Get1 e esplus, Remove e esplus, es ~ Delete e esplus,
   MonadError (OneOf esplus) m, MonadError (OneOf es) n)
  => (forall b. (OneOf esplus -> OneOf es) -> m b -> n b)
  -> m a -> (e -> n a) -> n a
catchMemberOld helper ma f =
  -- The idea here is that we use tryError to bring a copy of e into
  -- the return value, then we can just delete e from error monad.
  helper (remove @e Proxy) (tryError ma) >>= either handle return
  where handle :: OneOf esplus -> n a
        handle es = maybe (throwError (remove @e Proxy es)) f (get1 es :: Maybe e)

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
   Put1 e es, Get1 e es,
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
  (Put1 e es, Get1 e es,
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
tryMemberOld :: forall e es m a. (MonadError (OneOf es) m, Get1 e es) => m a -> m (Either e a)
tryMemberOld ma = tryMemberOlder (Right <$> ma) (pure . Left)

-- λ> runExceptT (tryMember' @Char @String @'[Char, Int] (pure "ok") (\c -> pure ("Caught " <> show c)))
-- Right "ok"
-- λ> runExceptT (tryMember' @Char @String @'[Char, Int] (throwMember 'x') (\c -> pure ("Caught " <> show c)))
-- Right "Caught 'x'"
-- λ> runExceptT (tryMember' @Char @String @'[Char, Int] (throwMember (3 :: Int)) (\c -> pure ("Caught " <> show c)))
-- Left 3
tryMemberOlder :: forall e a es m. (Get1 e es, MonadError (OneOf es) m) => m a -> (e -> m a) -> m a
tryMemberOlder ma f = tryError ma >>= either (\es -> maybe (throwError es) f (get1 es :: Maybe e)) return

-- | Annotate a member error that has been thrown.
mapMember :: forall e es m a. (Put1 e es, Get1 e es, MonadError (OneOf es) m) => (e -> m e) -> m a -> m a
mapMember f ma =
  tryError ma >>= either (\es -> maybe (throwError es) (\e -> f e >>= throwMember) (get1 es :: Maybe e)) return

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
  (es ~ Delete e esplus, Get1 e esplus, Remove e esplus, Monad m)
  => ExceptT (OneOf esplus) m a
  -> ExceptT (OneOf es) m (Either e a)
runOneOf'' action = catchMemberOld withExceptT (Right <$> action) (return . Left)

runOneOf' ::
  forall (esplus :: [*]) e (es :: [*]) m a r.
  (es ~ Delete e esplus, Get1 e esplus, Remove e esplus, MonadError (OneOf es) m)
  => ExceptT (OneOf esplus) m a
  -> (Either e a -> m r)
  -> m r
runOneOf' action final = runExceptT (runOneOf'' action) >>= either throwError final

runOneOf ::
  forall (esplus :: [*]) e (es :: [*]) m a.
  (es ~ Delete e esplus, Get1 e esplus, Remove e esplus, MonadError (OneOf es) m)
  => ExceptT (OneOf esplus) m a
  -> m (Either e a)
runOneOf action = runOneOf' action return
{-# DEPRECATED runOneOf "Use tryMember" #-}

{-
dropMemberNew :: forall e es es'. OneOf es -> Either e (OneOf es')
dropMemberNew Empty = Right Empty
dropMemberNew (Val e) =
  case cast e :: Maybe e of
    Just e' -> Left e'
    Nothing -> Right (Val e :: OneOf es')
dropMemberNew (Val e) = Val e
dropMemberNew (NoVal es) = foo
-}

-- Added to 2.3.1
-- modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
-- modifyError f m = runExceptT m >>= either (throwError . f) pure

-- ** Example

data ErrorFoo = Foo deriving Show
data ErrorBar = Bar deriving Show
data ErrorBaz = Baz deriving Show

type AppErrors = OneOf '[ErrorFoo, ErrorBar, ErrorBaz]

handleBar :: (Get1 ErrorBar errors, Remove ErrorBar errors) => OneOf errors -> IO (OneOf (Delete ErrorBar errors))
handleBar err =
  do case get1 err of
       Nothing -> putStrLn "no Bar error to handle"
       (Just Bar) -> putStrLn "took care of Bar"
     pure (remove @ErrorBar Proxy err)

handleFoo :: (Get1 ErrorFoo errors, Remove ErrorFoo errors) => OneOf errors -> IO (OneOf (Delete ErrorFoo errors))
handleFoo err =
  do case get1 err of
       Nothing -> putStrLn "no Bar error to handle"
       (Just Foo) -> putStrLn "took care of Bar"
     pure (remove @ErrorFoo Proxy err)

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
  do let errAll = put1 Foo :: AppErrors
     putStrLn $ "current error = " ++ show errAll

     errNoBar <- handleBar errAll
     putStrLn $ "current error = " ++ show errNoBar

     errNoFoo <- handleFoo errNoBar
     putStrLn $ "current error = " ++ show errNoFoo

     -- this will not compile because ErrorFoo has already been handled
--     errNoFoFoo <- handleFoo errNoFoo

     pure ()
