{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Registry.Hedgehog
  ( -- creation / tweaking functions
    Gen,
    Chooser (..),
    genFun,
    genVal,
    genWith,
    setGen,
    specializeGen,
    tweakGen,
    makeNonEmpty,
    genListOf,
    genListOfMinMax,
    genNonEmptyOfMinMax,
    genNonEmptyOf,
    genMaybeOf,
    genOneOf,
    genPairOf,
    genTripleOf,
    genTuple4Of,
    genMapOf,
    genNonEmptyMapOf,
    genHashMapOf,
    setDistinctPairOf,
    setDistinctPairOfOn,
    setDistinctTripleOf,
    setDistinctTripleOfOn,
    -- combinators to compose different types of generators
    distinctPairOf,
    distinctPairOfOn,
    distinctTripleOf,
    distinctTripleOfOn,
    eitherOf,
    hashMapOf,
    listOf,
    listOfMinMax,
    nonEmptyOfMinMax,
    mapOf,
    maybeOf,
    nonEmptyMapOf,
    nonEmptyOf,
    pairOf,
    setOf,
    setOfMinMax,
    tripleOf,
    tuple4Of,
    tuple5Of,
    -- choosing constructors in an ADT
    choiceChooser,
    chooseOne,
    -- sampling for Gen generators
    sampleIO,
  )
where

import Data.HashMap.Strict as HashMap (HashMap, fromList)
import qualified Data.List as L
import Data.List.NonEmpty as NonEmpty hiding (cycle, nonEmpty, (!!))
import Data.Map as Map (fromList)
import Data.Maybe as Maybe
import Data.Registry
import Data.Registry.Internal.Hedgehog
import Data.Set as Set (fromList)
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range
import Protolude as P

-- * CREATION / TWEAKING OF REGISTRY GENERATORS

-- | Create a Gen a for a given constructor of type a
genFun :: forall a b. (ApplyVariadic Gen a b, Typeable a, Typeable b) => a -> Typed b
genFun = funTo @Gen

-- | Create a Gen a for a given constructor of type a
genVal :: forall a. (Typeable a) => Gen a -> Typed (Gen a)
genVal = fun

-- | Extract a generator from a registry
--   We use makeUnsafe assuming that the registry has been checked before
genWith :: forall a ins out. (Typeable a) => Registry ins out -> Gen a
genWith = make @(Gen a)

-- | Modify the value of a generator in a given registry
tweakGen :: forall a ins out. (Typeable a) => (a -> a) -> Registry ins out -> Registry ins out
tweakGen f = tweak @(Gen a) (f <$>)

-- | Set a specific generator on the registry the value of a generator in a given registry
setGen :: forall a ins out. (Typeable a) => Gen a -> Registry ins out -> Registry ins out
setGen = tweak @(Gen a) . const

-- | Specialize a generator in a given context
specializeGen :: forall (a :: Type) b ins out. (Typeable a, Typeable b) => Gen b -> Registry ins out -> Registry ins out
specializeGen = specialize @(Gen a) @(Gen b)

-- | Add a generator for a list of elements
genListOf :: forall a. (Typeable a) => Typed (Gen a -> Gen [a])
genListOf = fun (listOf @a)

-- | Add a generator for a bounded list of elements
genListOfMinMax :: forall a. (Typeable a) => Int -> Int -> Typed (Gen a -> Gen [a])
genListOfMinMax mn mx = fun (listOfMinMax @a mn mx)

-- | Add a generator for a non-empty list of elements
genNonEmptyOf :: forall a. (Typeable a) => Typed (Gen a -> Gen (NonEmpty a))
genNonEmptyOf = fun (nonEmptyOf @a)

-- | Add a generator for a bounded non-empty list of elements
genNonEmptyOfMinMax :: forall a. (Typeable a) => Int -> Int -> Typed (Gen a -> Gen (NonEmpty a))
genNonEmptyOfMinMax mn mx = fun (nonEmptyOfMinMax @a mn mx)

-- | Add a generator for an optional element
genMaybeOf :: forall a. (Typeable a) => Typed (Gen a -> Gen (Maybe a))
genMaybeOf = fun (maybeOf @a)

-- | Add a generator for a element picked from a list
genOneOf :: (Typeable a, Show a) => [a] -> Typed (Gen a)
genOneOf as = genVal (Gen.element as)

-- | Add a generator for a pair of elements
genPairOf :: forall a b. (Typeable a, Typeable b) => Typed (Gen a -> Gen b -> Gen (a, b))
genPairOf = fun (pairOf @a @b)

-- | Add a generator for a triple of elements
genTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Gen a -> Gen b -> Gen c -> Gen (a, b, c))
genTripleOf = fun (tripleOf @a @b @c)

-- | Add a generator for 4 elements
genTuple4Of :: forall a b c d. (Typeable a, Typeable b, Typeable c, Typeable d) => Typed (Gen a -> Gen b -> Gen c -> Gen d -> Gen (a, b, c, d))
genTuple4Of = fun (tuple4Of @a @b @c @d)

-- | Add a generator for a map of elements
genMapOf :: forall k v. (Ord k, Typeable k, Typeable v) => Typed (Gen k -> Gen v -> Gen (Map k v))
genMapOf = fun (mapOf @k @v)

-- | Add a generator for a non empty map of elements
genNonEmptyMapOf :: forall k v. (Ord k, Typeable k, Typeable v) => Typed (Gen k -> Gen v -> Gen (Map k v))
genNonEmptyMapOf = fun (nonEmptyMapOf @k @v)

-- | Add a generator for a hashmap of elements
genHashMapOf :: forall k v. (Ord k, Hashable k, Typeable k, Typeable v) => Typed (Gen k -> Gen v -> Gen (HashMap k v))
genHashMapOf = fun (hashMapOf @k @v)

-- | Add the generation of a pair of distinct elements
setDistinctPairOf :: forall a. (Typeable a, Eq a) => Registry _ _ -> Registry _ _
setDistinctPairOf r = fun (distinctPairOf @a) +: r

-- | Add the generation of a pair of distinct elements, according to one of their part
setDistinctPairOfOn :: forall a b. (Typeable a, Eq b) => (a -> b) -> Registry _ _ -> Registry _ _
setDistinctPairOfOn f r = fun (distinctPairOfOn @a f) +: r

-- | Add the generation of a triple of distinct elements
setDistinctTripleOf :: forall a. (Typeable a, Eq a) => Registry _ _ -> Registry _ _
setDistinctTripleOf r = fun (distinctTripleOf @a) +: r

-- | Add the generation of a triple of distinct elements, according to one of their part
setDistinctTripleOfOn :: forall a b. (Typeable a, Eq b) => (a -> b) -> Registry _ _ -> Registry _ _
setDistinctTripleOfOn f r = fun (distinctTripleOfOn @a f) +: r

-- | Make sure there is always one element of a given type in a list of elements
makeNonEmpty :: forall (a :: Type) ins out. (Typeable a) => Registry ins out -> Registry ins out
makeNonEmpty r =
  -- extract a generator for one element only
  let genA = genWith @a r
   in -- add that element in front of a list of generated elements
      tweak @(Gen [a]) (\genAs -> (:) <$> genA <*> genAs) r

-- * CONTAINERS COMBINATORS

-- | Create a generator for a pair
pairOf :: forall a b. Gen a -> Gen b -> Gen (a, b)
pairOf ga gb = (,) <$> ga <*> gb

-- | Create a generator for a triple
tripleOf :: forall a b c. Gen a -> Gen b -> Gen c -> Gen (a, b, c)
tripleOf ga gb gc = (,,) <$> ga <*> gb <*> gc

-- | Create a generator for a quadruple
tuple4Of :: forall a b c d. Gen a -> Gen b -> Gen c -> Gen d -> Gen (a, b, c, d)
tuple4Of ga gb gc gd = (,,,) <$> ga <*> gb <*> gc <*> gd

-- | Create a generator for a quintuple
tuple5Of :: forall a b c d e. Gen a -> Gen b -> Gen c -> Gen d -> Gen e -> Gen (a, b, c, d, e)
tuple5Of ga gb gc gd ge = (,,,,) <$> ga <*> gb <*> gc <*> gd <*> ge

-- | Create a default generator for a small list of elements
listOf :: forall a. Gen a -> Gen [a]
listOf = Gen.list (linear 0 10)

-- | Create a default generator for a list of elements of min elements and max elements
listOfMinMax :: forall a. Int -> Int -> Gen a -> Gen [a]
listOfMinMax min' max' = Gen.list (linear min' max')

-- | Create a default generator for a small non-empty list of elements
nonEmptyOf :: Gen a -> Gen (NonEmpty a)
nonEmptyOf = Gen.nonEmpty (linear 1 10)

-- | Create a default generator for a Maybe, choosing evenly between Nothing and Just
maybeOf :: forall a. Gen a -> Gen (Maybe a)
maybeOf genA = choice [pure Nothing, Just <$> genA]

-- | Create a default generator for a Either, choosing evenly between Left and Right
eitherOf :: forall a b. Gen a -> Gen b -> Gen (Either a b)
eitherOf genA genB = choice [Left <$> genA, Right <$> genB]

-- | Create a default generator for a small set of elements
setOf :: forall a. (Ord a) => Gen a -> Gen (Set a)
setOf = fmap Set.fromList . listOf

-- | Create a default generator for a set with a minimum and a maximum number of elements
--   The implementation uses Gen.filter to make sure that the elements are unique
setOfMinMax :: forall a. (Ord a) => Int -> Int -> Gen a -> Gen (Set a)
setOfMinMax mi mx = fmap Set.fromList . Gen.filter (\as -> L.nub as == as) . listOfMinMax @a mi mx

-- | Create a default generator for map of key/values
mapOf :: forall k v. (Ord k) => Gen k -> Gen v -> Gen (Map k v)
mapOf gk gv = Map.fromList <$> listOf (pairOf gk gv)

-- | Create a default generator for HashMap of key/values
hashMapOf :: forall k v. (Ord k, Hashable k) => Gen k -> Gen v -> Gen (HashMap k v)
hashMapOf gk gv = HashMap.fromList <$> listOf (pairOf gk gv)

-- | Create a default generator for a small non-empty map of elements
nonEmptyMapOf :: forall k v. (Ord k) => Gen k -> Gen v -> Gen (Map k v)
nonEmptyMapOf gk gv = do
  h <- pairOf gk gv
  t <- listOf (pairOf gk gv)
  pure (Map.fromList (h : t))

-- | Make a generator for a non empty list of elements of a given type
nonEmptyOfMinMax :: Int -> Int -> Gen a -> Gen (NonEmpty a)
nonEmptyOfMinMax mi ma g = NonEmpty.fromList <$> listOfMinMax mi ma g

-- | Make a generator for a pair of distinct values
distinctPairOf :: forall a. (Eq a) => Gen a -> Gen (a, a)
distinctPairOf = distinctPairOfOn identity

-- | Make a generator for a pair of distinct values according to one of their part
distinctPairOfOn :: forall a b. (Eq b) => (a -> b) -> Gen a -> Gen (a, a)
distinctPairOfOn f genA = Gen.filterT (\(a1, a2) -> f a1 /= f a2) $ (,) <$> genA <*> genA

-- | Make a generator for a triple of distinct values
distinctTripleOf :: forall a. (Eq a) => Gen a -> Gen (a, a, a)
distinctTripleOf = distinctTripleOfOn identity

-- | Make a generator for a triple of distinct values according to one of their part
distinctTripleOfOn :: forall a b. (Eq b) => (a -> b) -> Gen a -> Gen (a, a, a)
distinctTripleOfOn f genA = Gen.filterT (\(a1, a2, a3) -> f a1 /= f a2 && f a2 /= f a3 && f a1 /= f a3) $ (,,) <$> genA <*> genA <*> genA
