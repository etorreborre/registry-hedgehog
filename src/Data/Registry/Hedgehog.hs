{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Hedgehog (
  -- creation / tweaking functions
  GenIO
, Chooser (..)
, genFun
, genVal
, genWith
, tweakGen
, tweakGenS
, setGen
, setGenIO
, setGenS
, specializeGen
, specializeGenIO
, specializeGenS
, makeNonEmpty
, makeNonEmptyS
, forallS
, forAllT -- re-export of forAllT for convenience purpose since we are working in GenIO

-- combinators to compose different types of generators
, pairOf
, tripleOf
, listOf
, listOfMinMax
, nonEmptyOf
, maybeOf
, eitherOf
, setOf
, mapOf
, nonEmptyMapOf
, hashMapOf

-- cycling values
, setCycleChooserS
, setCycleChooser
, choiceChooser
, chooseOne

-- making distinct values
, setDistinctS
, setDistinctForS
, setDistinct
, setDistinctFor
, distinct

-- sampling for GenIO generators
, sampleIO
) where

import           Control.Monad.Morph
import           Data.HashMap.Strict             as HashMap (HashMap, fromList)
import           Data.IORef
import           Data.List.NonEmpty              hiding (cycle, nonEmpty, (!!))
import           Data.Map                        as Map (fromList)
import           Data.Maybe                      as Maybe
import           Data.Registry
import           Data.Registry.Internal.Hedgehog
import           Data.Registry.Internal.Types
import           Data.Set                        as Set (fromList)
import           Hedgehog
import           Hedgehog.Gen                    as Gen
import           Hedgehog.Internal.Property      (forAllT)
import           Hedgehog.Range
import           Protolude                       as P


-- * CREATION / TWEAKING OF REGISTRY GENERATORS

-- | Create a GenIO a for a given constructor of type a
genFun :: forall a b . (ApplyVariadic GenIO a b, Typeable a, Typeable b) => a -> Typed b
genFun = funTo @GenIO

-- | Lift a Gen a into GenIO a to be added to a registry
genVal :: forall a . (Typeable a) => Gen a -> Typed (GenIO a)
genVal g = fun (liftGen g)

-- | Extract a generator from a registry
--   We use makeUnsafe assuming that the registry has been checked before
genWith :: forall a ins out . (Typeable a) => Registry ins out -> GenIO a
genWith = makeUnsafe @(GenIO a)

-- | Modify the value of a generator in a given registry
tweakGen :: forall a ins out . (Typeable a) => (a -> a) -> Registry ins out -> Registry ins out
tweakGen f = tweakUnsafe @(GenIO a) (\genA -> f <$> genA)

-- | Modify the registry for a given generator in a State monad
tweakGenS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => (a -> a) -> m ()
tweakGenS f = modify (tweakGen f)

-- | Set a specific generator on the registry the value of a generator in a given registry
setGen :: forall a ins out . (Typeable a) => Gen a -> Registry ins out -> Registry ins out
setGen = setGenIO . liftGen

setGenIO :: forall a ins out . (Typeable a) => GenIO a -> Registry ins out -> Registry ins out
setGenIO genA = tweakUnsafe @(GenIO a) (const genA)

-- | Set a specific generator on the registry the value of a generator in a given registry in a State monad
setGenS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => Gen a -> m ()
setGenS genA = modify (setGen genA)

-- | Specialize a generator in a given context
specializeGen :: forall a b ins out . (Typeable a, Typeable b, Contains (GenIO a) out) => Gen b -> Registry ins out -> Registry ins out
specializeGen g = specializeGenIO @a (liftGen g)

-- | Specialize a generator in a given context
specializeGenIO :: forall a b ins out . (Typeable a, Typeable b, Contains (GenIO a) out) => GenIO b -> Registry ins out -> Registry ins out
specializeGenIO = specialize @(GenIO a)

-- | Specialize a generator in a given context
specializeGenS :: forall a b m ins out . (Typeable a, Typeable b, Contains (GenIO a) out, MonadState (Registry ins out) m) => Gen b -> m ()
specializeGenS g = modify (specializeGen @a @b g)

-- | Get a value generated from one of the generators in the registry and modify the registry
--   using a state monad
forallS :: forall a m out . (Typeable a, Show a, MonadIO m) => PropertyT (StateT (Registry _ out) m) a
forallS = do
  r <- P.lift $ get
  withFrozenCallStack $ hoist liftIO $ forAllT (genWith @a r)

-- | Make sure there is always one element of a given type in a list of elements
makeNonEmpty :: forall a ins out . (Typeable a) => Registry ins out -> Registry ins out
makeNonEmpty r =
  -- extract a generator for one element only
  let genA = genWith @a r
  -- add that element in front of a list of generated elements
  in  tweakUnsafe @(GenIO [a]) (\genAs -> (:) <$> genA <*> genAs) r

-- | Make sure there is always one element of a given type in a list of elements in a State monad
makeNonEmptyS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => m ()
makeNonEmptyS = modify (makeNonEmpty @a)

-- * CONTAINERS COMBINATORS

-- | Create a generator for a pair
pairOf :: forall a b . GenIO a -> GenIO b -> GenIO (a, b)
pairOf ga gb = (,) <$> ga <*> gb

-- | Create a generator for a triple
tripleOf :: forall a b c . GenIO a -> GenIO b -> GenIO c -> GenIO (a, b, c)
tripleOf ga gb gc = (,,) <$> ga <*> gb <*> gc

-- | Create a default generator for a small list of elements
listOf :: forall a . GenIO a -> GenIO [a]
listOf = Gen.list (linear 0 3)

-- | Create a default generator for a list of elements of min elements and max elements
listOfMinMax :: forall a . Int -> Int -> GenIO a -> GenIO [a]
listOfMinMax min' max' = Gen.list (linear min' max')

-- | Create a default generator for a small non-empty list of elements
nonEmptyOf :: GenIO a -> GenIO (NonEmpty a)
nonEmptyOf = Gen.nonEmpty (linear 1 3)

-- | Create a default generator for a Maybe, choosing evenly between Nothing and Just
maybeOf :: forall a . GenIO a -> GenIO (Maybe a)
maybeOf genA = choice [pure Nothing, Just <$> genA]

-- | Create a default generator for a Either, choosing evenly between Left and Right
eitherOf :: forall a b . GenIO a -> GenIO b -> GenIO (Either a b)
eitherOf genA genB = choice [Left <$> genA, Right <$> genB]

-- | Create a default generator for a small set of elements
setOf :: forall a . (Ord a) => GenIO a -> GenIO (Set a)
setOf = fmap Set.fromList . listOf

-- | Create a default generator for map of key/values
mapOf :: forall k v . (Ord k) => GenIO k -> GenIO v -> GenIO (Map k v)
mapOf gk gv = Map.fromList <$> listOf (pairOf gk gv)

-- | Create a default generator for HashMap of key/values
hashMapOf :: forall k v . (Ord k, Hashable k) => GenIO k -> GenIO v -> GenIO (HashMap k v)
hashMapOf gk gv = HashMap.fromList <$> listOf (pairOf gk gv)

-- | Create a default generator for a small non-empty map of elements
nonEmptyMapOf :: forall k v . (Ord k) => GenIO k -> GenIO v -> GenIO (Map k v)
nonEmptyMapOf gk gv = do
  h <- pairOf gk gv
  t <- listOf (pairOf gk gv)
  pure (Map.fromList (h : t))

-- * STATEFUL GENERATORS

-- * CHOOSING VALUES DETERMINISTICALLY

-- | Set a cycling chooser for a specific data type
setCycleChooser :: forall a ins out . (Typeable a, Contains (GenIO a) out) => Registry ins out -> IO (Registry ins out)
setCycleChooser r = do
  c <- cycleChooser
  pure $ specializeValTo @GenIO @(GenIO a) c r

-- | Set a cycling chooser for a specific data type
setCycleChooserS :: forall a m ins out . (Typeable a, Contains (GenIO a) out, MonadState (Registry ins out) m, MonadIO m) => m ()
setCycleChooserS = do
  r <- get
  r' <- liftIO $ setCycleChooser @a r
  put r'

-- * MAKING DISTINCT VALUES

-- | Generate distinct values for a specific data type
setDistinct :: forall a ins out . (Eq a, Typeable a, Contains (GenIO a) out) => Registry ins out -> IO (Registry ins out)
setDistinct r = do
  ref <- newIORef []
  let g = makeFast @(GenIO a) r
  pure $ setGenIO (distinctWith ref g) r

-- | Generate distinct values for a specific data type
setDistinctS :: forall a m ins out . (Eq a, Typeable a, Contains (GenIO a) out, MonadState (Registry ins out) m, MonadIO m) => m ()
setDistinctS = do
  r <- get
  r' <- liftIO $ setDistinct @a r
  put r'

-- | Generate distinct values for a specific data type, when used inside another data type
setDistinctFor :: forall a b ins out . (Typeable a, Contains (GenIO a) out, Eq b, Typeable b, Contains (GenIO b) out) => Registry ins out -> IO (Registry ins out)
setDistinctFor r = do
  ref <- newIORef []
  let g = makeFast @(GenIO b) r
  pure $ specializeGenIO @a (distinctWith ref g) r

-- | Generate distinct values for a specific data type, when used inside another data type
setDistinctForS :: forall a b m ins out . (Typeable a, Contains (GenIO a) out, Eq b, Typeable b, Contains (GenIO b) out, MonadState (Registry ins out) m, MonadIO m) => m ()
setDistinctForS = do
  r <- get
  r' <- liftIO $ setDistinctFor @a @b r
  put r'
