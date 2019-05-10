{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Hedgehog (
  genFun
, genVal
, genWith
, tweakGen
, tweakGenS
, setGen
, setGenS
, forallS
, makeNonEmpty
, makeNonEmptyS
, pairOf
, tripleOf
, listOf
, listOfMinMax
, nonEmptyOf
, maybeOf
, setOf
, mapOf
, nonEmptyMapOf
, hashMapOf
, forAllT
, cycleWith
, setCycleChooser
, setCycleChooserS
, specializeGen
, specializeGenS
, sampleIO
, GenIO
, Chooser (..)
, chooseOne
, choiceChooser
, cycleChooser
) where

import           Control.Monad.Morph
import           Control.Monad.Trans.Maybe
import           Data.HashMap.Strict          as HashMap (HashMap, fromList)
import           Data.IORef
import           Data.List.NonEmpty           hiding (cycle, nonEmpty, (!!))
import           Data.Map                     as Map (fromList)
import           Data.Maybe                   as Maybe
import           Data.Registry
import           Data.Registry.Internal.Types
import           Data.Set                     as Set (fromList)
import           Hedgehog
import           Hedgehog.Gen                 as Gen
import           Hedgehog.Internal.Gen        as Gen
import           Hedgehog.Internal.Property   (forAllT)
import           Hedgehog.Internal.Seed       as Seed (random)
import           Hedgehog.Internal.Tree       as Tree (Node (..), Tree (..))
import           Hedgehog.Range
import           Prelude                      ((!!), show)
import           Protolude                    as P


-- * COMBINATORS

type GenIO = GenT IO

genFun :: forall a b . (ApplyVariadic GenIO a b, Typeable a, Typeable b) => a -> Typed b
genFun = funTo @GenIO

genVal :: forall a . (Typeable a) => Gen a -> Typed (GenIO a)
genVal g = fun (Gen.lift g)

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
setGen :: forall a ins out . (Typeable a) => GenIO a -> Registry ins out -> Registry ins out
setGen genA = tweakUnsafe @(GenIO a) (const genA)

-- | Set a specific generator on the registry the value of a generator in a given registry in a State monad
setGenS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => GenIO a -> m ()
setGenS genA = modify (setGen genA)

-- | Specialize a generator in a given context
specializeGen :: forall a b ins out . (Typeable a, Typeable b, Contains (GenIO a) out) => GenIO b -> Registry ins out -> Registry ins out
specializeGen = specialize @(GenIO a)

-- | Specialize a generator in a given context
specializeGenS :: forall a b m ins out . (Typeable a, Typeable b, Contains (GenIO a) out, MonadState (Registry ins out) m) => Gen b -> m ()
specializeGenS g = modify (specializeGen @a @b (Gen.lift g))

-- | Cycle a specific datatype
setCycleChooser :: forall a ins out . (Typeable a, Contains (GenIO a) out) => Registry ins out -> IO (Registry ins out)
setCycleChooser r = do
  c <- cycleChooser
  pure $ specializeValTo @GenIO @(GenIO a) c r

-- | Set a specific generator on the registry the value of a generator in a given registry in a State monad
setCycleChooserS :: forall a m ins out . (Typeable a, Contains (GenIO a) out, MonadState (Registry ins out) m, MonadIO m) => m ()
setCycleChooserS = do
  r <- get
  r' <- liftIO $ setCycleChooser @a r
  put r'

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
pairOf :: forall a b m . (Monad m) => GenT m a -> GenT m b -> GenT m (a, b)
pairOf ga gb = (,) <$> ga <*> gb

-- | Create a generator for a triple
tripleOf :: forall a b c m . (Monad m) => GenT m a -> GenT m b -> GenT m c -> GenT m (a, b, c)
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

-- * Choosing

chooseOne :: GenIO Chooser -> [GenIO a] -> GenIO a
chooseOne chooser gs = do
  c <- chooser
  g <- P.lift $ pickOne c gs
  g

choiceChooser :: Chooser
choiceChooser = Chooser { chooserType = "choice", pickOne = pure . Gen.choice }

cycleChooser :: IO Chooser
cycleChooser = do
  ref <- newIORef 0
  pure $ Chooser { chooserType = "cycle", pickOne = cycleWith ref }

data Chooser = Chooser {
  chooserType :: Text
, pickOne :: forall a . [GenIO a] -> IO (GenIO a)
}

instance Show Chooser where
  show c = toS (chooserType c)
-- | Cycle generators

cycleWith :: (MonadIO m) => IORef Int -> [GenT m a] -> IO (GenT m a)
cycleWith ref gs = do
  n <- readIORef ref
  writeIORef ref (if n == P.length gs - 1 then 0 else n + 1)
  pure (gs !! n)

sampleIO :: GenIO a -> IO a
sampleIO gen =
    let
      loop n =
        if n <= 0 then
          panic "Hedgehog.Gen.sample: too many discards, could not generate a sample"
        else do
          seed <- Seed.random
          r <- evalGenIO 30 seed gen
          case r of
            Nothing ->
              loop (n - 1)
            Just a ->
              pure a
    in
      loop (100 :: Int)

-- | Runs a generator, producing its shrink tree.
--
evalGenIO :: Size -> Seed -> GenIO a -> IO (Maybe a)
evalGenIO size seed g = do
  r <- runMaybeT . runTree $ runGenT size seed g
  pure $ case r of
    Nothing         -> Nothing
    Just (Node a _) -> Just a
