{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Hedgehog where

import           Data.HashMap.Strict          as HashMap (HashMap, fromList)
import           Data.List.NonEmpty           hiding (nonEmpty)
import           Data.Map                     as Map (fromList)
import           Data.Registry
import           Data.Set                     as Set (fromList)
import           Hedgehog
import           Hedgehog.Gen                 as Gen
import           Hedgehog.Range
import           Protolude

-- * COMBINATORS

-- | Extract a generator from a registry
--   We use makeUnsafe assuming that the registry has been checked before
genWith :: forall a ins out . (Typeable a) => Registry ins out -> Gen a
genWith = makeUnsafe @(Gen a)

-- | Modify the value of a generator in a given registry
tweakGen :: forall a ins out . (Typeable a) => (a -> a) -> Registry ins out -> Registry ins out
tweakGen f = tweakUnsafe @(Gen a) (\genA -> f <$> genA)

-- | Modify the registry for a given generator in a State monad
tweakGenS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => (a -> a) -> m ()
tweakGenS f = modify (tweakGen f)

-- | Set a specific generator on the registry the value of a generator in a given registry
setGen :: forall a ins out . (Typeable a) => Gen a -> Registry ins out -> Registry ins out
setGen genA = tweakUnsafe @(Gen a) (const genA)

-- | Set a specific generator on the registry the value of a generator in a given registry in a State monad
setGenS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => Gen a -> m ()
setGenS genA = modify (setGen genA)

-- | Get a value generated from one of the generators in the registry and modify the registry
--   using a state monad
forallS :: forall a m out . (Typeable a, Show a, Monad m) => PropertyT (StateT (Registry _ out) m) a
forallS = do
  r <- get
  withFrozenCallStack $ forAll (genWith @a r)

-- | Make sure there is always one element of a given type in a list of elements
makeNonEmpty :: forall a ins out . (Typeable a) => Registry ins out -> Registry ins out
makeNonEmpty r =
  -- extract a generator for one element only
  let genA = genWith @a r
  -- add that element in front of a list of generated elements
  in  tweakUnsafe @(Gen [a]) (\genAs -> (:) <$> genA <*> genAs) r

-- | Make sure there is always one element of a given type in a list of elements in a State monad
makeNonEmptyS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => m ()
makeNonEmptyS = modify (makeNonEmpty @a)

-- * CONTAINERS COMBINATORS

-- | Create a generator for a pair
pairOf :: forall a b . Gen a -> Gen b -> Gen (a, b)
pairOf ga gb = (,) <$> ga <*> gb

-- | Create a generator for a triple
tripleOf :: forall a b c . Gen a -> Gen b -> Gen c -> Gen (a, b, c)
tripleOf ga gb gc = do
  a <- ga
  b <- gb
  c <- gc
  pure (a, b, c)

-- | Create a default generator for a small list of elements
listOf :: forall a . Gen a -> Gen [a]
listOf = Gen.list (linear 0 3)

-- | Create a default generator for a list of elements of min elements and max elements
listOfMinMax :: forall a . Int -> Int -> Gen a -> Gen [a]
listOfMinMax min' max' = Gen.list (linear min' max')

-- | Create a default generator for a small non-empty list of elements
nonEmptyOf :: Gen a -> Gen (NonEmpty a)
nonEmptyOf = Gen.nonEmpty (linear 1 3)

-- | Create a default generator for a Maybe, choosing evenly between Nothing and Just
maybeOf :: forall a . Gen a -> Gen (Maybe a)
maybeOf genA = choice [pure Nothing, Just <$> genA]

-- | Create a default generator for a small set of elements
setOf :: forall a . (Ord a) => Gen a -> Gen (Set a)
setOf = fmap Set.fromList . listOf

-- | Create a default generator for map of key/values
mapOf :: forall k v . (Ord k) => Gen k -> Gen v ->  Gen (Map k v)
mapOf gk gv = Map.fromList <$> listOf (pairOf gk gv)

-- | Create a default generator for HashMap of key/values
hashMapOf :: forall k v . (Ord k, Hashable k) => Gen k -> Gen v ->  Gen (HashMap k v)
hashMapOf gk gv = HashMap.fromList <$> listOf (pairOf gk gv)

-- | Create a default generator for a small non-empty map of elements
nonEmptyMapOf :: forall k v . (Ord k) => Gen k -> Gen v ->  Gen (Map k v)
nonEmptyMapOf gk gv = do
  h <- pairOf gk gv
  t <- listOf (pairOf gk gv)
  pure (Map.fromList (h : t))
