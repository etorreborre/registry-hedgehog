{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Registry.Hedgehog
  ( -- creation / tweaking functions
    Gen,
    Chooser (..),
    forallS,
    forAllT, -- re-export of forAllT for convenience purpose since we are working in Gen
    filterGenS,
    genFun,
    genVal,
    genWith,
    modifyGenS,
    setGen,
    setGenS,
    setGenT,
    specializeGen,
    specializeGenS,
    specializeGenT,
    tweakGen,
    tweakGenS,
    makeNonEmpty,
    makeNonEmptyS,
    -- combinators to compose different types of generators
    eitherOf,
    hashMapOf,
    listOf,
    listOfMinMax,
    mapOf,
    maybeOf,
    nonEmptyMapOf,
    nonEmptyOf,
    pairOf,
    setOf,
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
import Data.List.NonEmpty hiding (cycle, nonEmpty, (!!))
import Data.Map as Map (fromList)
import Data.Maybe as Maybe
import Data.Registry
import Data.Registry.Internal.Hedgehog
import Data.Registry.Internal.Types
import Data.Set as Set (fromList)
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (forAllT)
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

-- | Modify the registry for a given generator in a State monad
tweakGenS :: forall a m ins out. (Typeable a, MonadState (Registry ins out) m) => (a -> a) -> m ()
tweakGenS f = modify (tweakGen f)

-- | Set a specific generator on the registry the value of a generator in a given registry
setGen :: forall a ins out. (Typeable a) => Gen a -> Registry ins out -> Registry ins out
setGen = tweak @(Gen a) . const

-- | Set a specific generator on the registry the value of a generator in a given registry
setGenT :: forall m a ins out. (Typeable a, Typeable m) => GenT m a -> Registry ins out -> Registry ins out
setGenT = tweak @(GenT m a) . const

-- | Set a specific generator on the registry the value of a generator in a given registry in a State monad
setGenS :: forall a m ins out. (Typeable a, MonadState (Registry ins out) m) => Gen a -> m ()
setGenS genA = modify (setGen genA)

-- | Specialize a generator in a given context
specializeGen :: forall a b ins out. (Typeable a, Typeable b) => Gen b -> Registry ins out -> Registry ins out
specializeGen = specialize @(Gen a) @(Gen b)

-- | Specialize a generator in a given context
specializeGenT :: forall m a b ins out. (Typeable a, Typeable b, Typeable m) => GenT m b -> Registry ins out -> Registry ins out
specializeGenT = specialize @(GenT m a) @(GenT m b)

-- | Specialize a generator in a given context
specializeGenS :: forall a b m ins out. (Typeable a, Typeable b, MonadState (Registry ins out) m) => Gen b -> m ()
specializeGenS g = modify (specializeGen @a @b g)

-- | Modify a generator
modifyGenS :: forall a ins out. (Typeable a) => (Gen a -> Gen a) -> PropertyT (StateT (Registry ins out) IO) ()
modifyGenS f = modify (tweak @(Gen a) f)

-- | Filter a generator
filterGenS :: forall a ins out. (Typeable a) => (a -> Bool) -> PropertyT (StateT (Registry ins out) IO) ()
filterGenS = modifyGenS . Gen.filterT

-- | Get a value generated from one of the generators in the registry and modify the registry
--   using a state monad
forallS :: forall a m out. (Typeable a, Show a, MonadIO m) => PropertyT (StateT (Registry _ out) m) a
forallS = do
  r <- P.lift get
  withFrozenCallStack $ forAll (genWith @a r)

-- | Make sure there is always one element of a given type in a list of elements
makeNonEmpty :: forall a ins out. (Typeable a) => Registry ins out -> Registry ins out
makeNonEmpty r =
  -- extract a generator for one element only
  let genA = genWith @a r
   in -- add that element in front of a list of generated elements
      tweak @(Gen [a]) (\genAs -> (:) <$> genA <*> genAs) r

-- | Make sure there is always one element of a given type in a list of elements in a State monad
makeNonEmptyS :: forall a m ins out. (Typeable a, MonadState (Registry ins out) m) => m ()
makeNonEmptyS = modify (makeNonEmpty @a)

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
