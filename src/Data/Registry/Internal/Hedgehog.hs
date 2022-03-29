{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Internal.Hedgehog
  ( Chooser (..),
    chooseOne,
    choiceChooser,
    -- utilities
    liftGen,
    sampleIO,
  )
where

import Control.Monad.Morph
import Data.Maybe as Maybe
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen as Gen
import Hedgehog.Internal.Seed as Seed (random)
import Hedgehog.Internal.Tree as Tree (NodeT (..), runTreeT)
import Protolude as P
import Prelude (show)

-- | Lift a pure generator into another monad like IO
liftGen :: (Monad m) => Gen a -> GenT m a
liftGen = hoist (pure . runIdentity)

-- * CHOOSING VALUES DETERMINISTICALLY

-- | Given a choosing strategy pick a generator
--   This is possibly a stateful operation
chooseOne :: Gen Chooser -> [Gen a] -> Gen a
chooseOne chooser gs = do
  c <- chooser
  pickOne c gs

-- | Chooser for randomly selecting a generator
choiceChooser :: Chooser
choiceChooser = Chooser {chooserType = "choice", pickOne = Gen.choice}

-- | A "chooser" strategy
--   The type can be used to debug specializations
data Chooser = Chooser
  { chooserType :: Text,
    pickOne :: forall a. [Gen a] -> Gen a
  }

instance Show Chooser where
  show c = toS (chooserType c)

-- * UTILITIES

-- | Sample Gen values
sampleIO :: GenT IO a -> IO a
sampleIO gen =
  let loop n =
        if n <= 0
          then panic "Hedgehog.Gen.sample: too many discards, could not generate a sample"
          else do
            seed <- Seed.random
            NodeT r _ <- runTreeT $ evalGenT 30 seed gen
            case r of
              Nothing ->
                loop (n - 1)
              Just a ->
                pure a
   in loop (100 :: Int)
