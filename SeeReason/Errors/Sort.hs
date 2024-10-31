{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SeeReason.Errors.Sort
  ( Nub
  , Sort
  , TypeId
  , AsSet
  , MemberTest(NotFound, Found)
  , MemberP
  , Delete
  , Insert
  , Intersection
  , Difference
  ) where

import GHC.TypeLits
-- import Data.Proxy (Proxy(Proxy))
import Data.Type.Bool
import Data.Type.Equality

-- :kind! Nub (Sort '[Int, Char, Int, Double, [Float]])
-- Nub (Sort '[Int, Char, Int, Double, [Float]]) :: [*]
-- = '[Char, Double, Int, [Float]]

-- | Uniquify a sorted list.
type family Nub xs where
  Nub '[] = '[]
  Nub '[x] = '[x]
  Nub (x ': x ': ys) = Nub (x ': ys)
  Nub (x ': y ': ys) = x ': Nub (y ': ys)

-- | List append (essentially set disjoint union)
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
            '[]       :++ xs = xs
            (x ': xs) :++ ys = x ': (xs :++ ys)

-- | Type-level quick sort for normalising the representation of sets
type family Sort (xs :: [k]) :: [k] where
            Sort '[]       = '[]
            Sort (x ': xs) = ((Sort (Filter 'FMin x xs)) :++ '[x]) :++ (Sort (Filter 'FMax x xs))

data Flag = FMin | FMax

-- | Open-family for the ordering operation in the sort

-- | Compute a symbol which uniquely identifies a type.  This is used
-- to avoid having to write 'Cmp' instances for every pair of types.
-- Add instances for all the types you want to use.  You are
-- responsible for ensuring that they are all distinct and unique.
type family TypeId (a :: *) :: Symbol
type instance TypeId Char = "Char"
type instance TypeId Double = "Double"
type instance TypeId Int = "Int"
type instance TypeId Float = "Float"
type instance TypeId [a] = AppendSymbol "[" (AppendSymbol (TypeId a) "]")

type family Cmp (a :: k) (b :: k) :: Ordering
type instance Cmp (k :: Symbol) (k' :: Symbol) = CmpSymbol k k'
type instance Cmp (a :: *) (b :: *) = CmpSymbol (TypeId a) (TypeId b)

type family Filter (f :: Flag) (p :: k) (xs :: [k]) :: [k] where
            Filter f p '[]       = '[]
            Filter 'FMin p (x ': xs) = If (Cmp x p == 'LT) (x ': (Filter 'FMin p xs)) (Filter 'FMin p xs)
            Filter 'FMax p (x ': xs) = If (Cmp x p == 'GT || Cmp x p == 'EQ) (x ': (Filter 'FMax p xs)) (Filter 'FMax p xs)

type family DeleteFromList (e :: elem) (list :: [elem]) where
    DeleteFromList elem '[] = '[]
    DeleteFromList elem (x ': xs) = If (Cmp elem x == 'EQ)
                                       xs
                                       (x ': DeleteFromList elem xs)

type AsSet s = Nub (Sort s)

type Union s t = AsSet (s :++ t)

type family Insert a s where
  Insert x xs = Union '[x] xs

data MemberTest
  = NotFound ErrorMessage
  | Found

{-
type family IsJust x where
  IsJust ('Just x) = 'True
  IsJust 'Nothing = 'False
-}

type family MemberP x ys where
  MemberP x '[] = 'NotFound ('Text "Member " ':<>: 'ShowType x ':<>: 'Text " not found in " ':<>: 'Text "ShowType ys")
  MemberP x (x ': ys) = 'Found
  MemberP x (y ': ys) = MemberP x ys
  -- MemberP x ys = MemberP x ys

type family Delete e xs where
  Delete x '[] = '[]
  Delete x (x ': ys) = ys -- Assuming AsSet was applied
  Delete x (y ': ys) = (y ': (Delete x ys))

type family Intersection a b where
  Intersection '[] s = '[]
  Intersection s '[] = '[]
  Intersection (x ': xs) ys =
    Union (If (MemberP x ys == 'Found) '[x] '[]) (Intersection xs (Delete x ys))

type family Difference a b where
  Difference a '[] = a
  Difference '[] a = '[]
  Difference (x ': xs) ys =
    Union (If (MemberP x ys == 'Found) '[] '[x]) (Difference xs ys)

{-
type family Delete elem set where
    Delete elem (Set xs) = Set (DeleteFromList elem xs)

-- | Value-level quick sort that respects the type-level ordering
class Sortable xs where
    quicksort :: Set xs -> Set (Sort xs)

instance Sortable '[] where
    quicksort Empty = Empty

instance (Sortable (Filter FMin p xs),
          Sortable (Filter FMax p xs), FilterV FMin p xs, FilterV FMax p xs) => Sortable (p ': xs) where
    quicksort (Ext p xs) = ((quicksort (less p xs)) `append` (Ext p Empty)) `append` (quicksort (more p xs))
                           where less = filterV (Proxy::(Proxy FMin))
                                 more = filterV (Proxy::(Proxy FMax))

-- | Filter out the elements less-than or greater-than-or-equal to the pivot
class FilterV (f::Flag) p xs where
    filterV :: Proxy f -> p -> Set xs -> Set (Filter f p xs)

instance FilterV f p '[] where
    filterV _ _ Empty      = Empty

instance (Conder ((Cmp x p) == LT), FilterV FMin p xs) => FilterV FMin p (x ': xs) where
    filterV f@Proxy p (Ext x xs) = cond (Proxy::(Proxy ((Cmp x p) == LT)))
                                        (Ext x (filterV f p xs)) (filterV f p xs)

instance (Conder (((Cmp x p) == GT) || ((Cmp x p) == EQ)), FilterV FMax p xs) => FilterV FMax p (x ': xs) where
    filterV f@Proxy p (Ext x xs) = cond (Proxy::(Proxy (((Cmp x p) == GT) || ((Cmp x p) == EQ))))
                                        (Ext x (filterV f p xs)) (filterV f p xs)

class Conder g where
    cond :: Proxy g -> Set s -> Set t -> Set (If g s t)

instance Conder True where
    cond _ s _ = s

instance Conder False where
    cond _ _ t = t
-}
