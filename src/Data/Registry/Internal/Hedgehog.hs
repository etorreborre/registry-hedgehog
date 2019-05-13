{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Registry.Internal.Hedgehog (
  GenIO
, Chooser (..)

-- cycling values
, cycleWith
, chooseOne
, choiceChooser
, cycleChooser

-- making distinct values
, distinct
, distinctWith

-- utilities
, liftGen
, sampleIO
) where

import           Control.Monad.Morph
import           Data.IORef
import           Data.Maybe             as Maybe
import           Hedgehog
import           Hedgehog.Gen           as Gen
import           Hedgehog.Internal.Gen  as Gen
import           Hedgehog.Internal.Seed as Seed (random)
import           Hedgehog.Internal.Tree as Tree (NodeT (..), runTreeT)
import           Prelude                (show, (!!))
import           Protolude              as P

-- | All the generators we use are lifted into GenIO to allow some generators to be stateful
type GenIO = GenT IO

-- | Lift a pure generator into another monad like IO
liftGen :: (Monad m) => Gen a -> GenT m a
liftGen = hoist (pure . runIdentity)

-- * CHOOSING VALUES DETERMINISTICALLY

-- | Given a choosing strategy pick a generator
--   This is possibly a stateful operation
chooseOne :: GenIO Chooser -> [GenIO a] -> GenIO a
chooseOne chooser gs = do
  c <- chooser
  join $ P.lift $ pickOne c gs

-- | Chooser for randomly selecting a generator
choiceChooser :: Chooser
choiceChooser = Chooser { chooserType = "choice", pickOne = pure . Gen.choice }

-- | Chooser for deterministically choosing elements in a list
--   by cycling over them, which requires to maintain some state about the last position
cycleChooser :: IO Chooser
cycleChooser = do
  ref <- newIORef 0
  pure $ Chooser { chooserType = "cycle", pickOne = cycleWith ref }

-- | A "chooser" strategy
--   The type can be used to debug specializations
data Chooser = Chooser {
  chooserType :: Text
, pickOne     :: forall a . [GenIO a] -> IO (GenIO a)
}

instance Show Chooser where
  show c = toS (chooserType c)

-- | Pick a generator in a list based on the previous position selected
cycleWith :: (MonadIO m) => IORef Int -> [GenT m a] -> IO (GenT m a)
cycleWith ref gs = do
  n <- readIORef ref
  modifyIORef ref increment
  pure (gs !! n)

  where increment i = if i == P.length gs - 1 then 0 else i + 1

-- * MAKING DISTINCT VALUES

-- | Create a generator for distinct values
--   This is a stateful operation
distinct :: (MonadIO m, Eq a) => GenT m a -> IO (GenT m a)
distinct g = do
  ref <- newIORef []
  pure $ distinctWith ref g

-- | Generate distinct values based on the values already generated
distinctWith :: (MonadIO m, Eq a) => IORef [a] -> GenT m a -> GenT m a
distinctWith ref g = GenT $ \size seed -> do
  as <- liftIO $ readIORef ref
  a <- runGenT size seed $ (Gen.filterT (not . flip elem as)) g
  liftIO $ writeIORef ref (a:as)
  pure a

-- * UTILITIES

-- | Sample GenIO values
sampleIO :: GenIO a -> IO a
sampleIO gen =
    let
      loop n =
        if n <= 0 then
          panic "Hedgehog.Gen.sample: too many discards, could not generate a sample"
        else do
          seed <- Seed.random
          NodeT r _  <- runTreeT $ evalGenT 30 seed gen
          case r of
            Nothing ->
              loop (n - 1)
            Just a ->
              pure a
    in
      loop (100 :: Int)
