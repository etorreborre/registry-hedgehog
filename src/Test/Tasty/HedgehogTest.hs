{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--
-- This redefines the HedgehogTest from tasty-hedgehog to display
-- color during the reporting
module Test.Tasty.HedgehogTest
  ( HedgehogTest (..),
    HedgehogTestLimit (..),
    HedgehogDiscardLimit (..),
    HedgehogShrinkLimit (..),
    HedgehogShrinkRetries (..),
    HedgehogReplay (..),
    HedgehogShowReplay (..),
    ModuleName (..),
    testProperty,
    groupByModuleName,
    getModuleName,
  )
where

import Data.MultiMap hiding (foldr, size)
import GHC.Stack
import Hedgehog hiding (test, (===))
import Hedgehog.Internal.Config (UseColor, detectColor)
import Hedgehog.Internal.Property
import Hedgehog.Internal.Report as Hedgehog
import Hedgehog.Internal.Runner as Hedgehog
import Hedgehog.Internal.Seed as Seed
import Protolude as P hiding (empty, toList, unwords, words)
import qualified Protolude as P
import Test.Tasty as Tasty
import Test.Tasty.Options as Tasty
import Test.Tasty.Providers as Tasty
import Test.Tasty.Runners as Tasty
  ( TestTree (..),
    foldSingle,
    foldTestTree,
    trivialFold,
  )
import Prelude (String, unwords, words)

-- | Hedgehog Property as a Tasty Test
data HedgehogTest = HedgehogTest Tasty.TestName Property
  deriving (Typeable)

-- | Create a 'Test' from a Hedgehog property
testProperty :: Tasty.TestName -> Property -> Tasty.TestTree
testProperty name prop = singleTest name (HedgehogTest name prop)

instance Tasty.IsTest HedgehogTest where
  testOptions =
    return
      [ Tasty.Option (Proxy :: Proxy HedgehogReplay),
        Tasty.Option (Proxy :: Proxy HedgehogShowReplay),
        Tasty.Option (Proxy :: Proxy HedgehogTestLimit),
        Tasty.Option (Proxy :: Proxy HedgehogDiscardLimit),
        Tasty.Option (Proxy :: Proxy HedgehogShrinkLimit),
        Tasty.Option (Proxy :: Proxy HedgehogShrinkRetries)
      ]

  run opts (HedgehogTest name (Property pConfig pTest)) yieldProgress = do
    useColor <- detectColor
    let HedgehogReplay replay = lookupOption opts
        HedgehogTestLimit mTests = lookupOption opts
        HedgehogDiscardLimit mDiscards = lookupOption opts
        HedgehogShrinkLimit mShrinks = lookupOption opts
        HedgehogShrinkRetries mRetries = lookupOption opts
        showReplay = lookupOption opts
        config =
          PropertyConfig
            (fromMaybe (propertyDiscardLimit pConfig) mDiscards)
            (fromMaybe (propertyShrinkLimit pConfig) mShrinks)
            (fromMaybe (propertyShrinkRetries pConfig) mRetries)
            (NoConfidenceTermination $ fromMaybe (propertyTestLimit pConfig) mTests)
            Nothing
    randSeed <- Seed.random
    -- if we just run one test we choose a high size (knowing that the max size is 99)
    -- if the test fails we can turn it to a prop and let the shrinking process find a
    -- smaller counter-example
    let minSize = if propertyTestLimit config == 1 then 50 else 0
    let size = P.maybe minSize fst replay
        seed = P.maybe randSeed snd replay
    report <- checkReport config size seed pTest (yieldProgress . reportToProgress config)
    let resultFn =
          if reportStatus report == OK
            then testPassed
            else testFailed
    out <- reportOutput showReplay useColor name report
    return $ resultFn out

reportToProgress ::
  PropertyConfig ->
  Report Hedgehog.Progress ->
  Tasty.Progress
reportToProgress config (Report testsDone _ _ _ status) =
  let TestLimit testLimit = propertyTestLimit config
      ShrinkLimit shrinkLimit = propertyShrinkLimit config
      ratio x y = 1.0 * fromIntegral x / fromIntegral y
   in -- TODO add details for tests run / discarded / shrunk
      case status of
        Running ->
          Tasty.Progress "Running" (ratio testsDone testLimit)
        Shrinking fr ->
          Tasty.Progress "Shrinking" (ratio (failureShrinks fr) shrinkLimit)

reportOutput ::
  HedgehogShowReplay ->
  UseColor ->
  String ->
  Report Hedgehog.Result ->
  IO String
reportOutput (HedgehogShowReplay showReplay) useColor name report = do
  s <- renderResult useColor (Just (PropertyName name)) report
  pure $ case reportStatus report of
    Failed _ ->
      let seed = reportSeed report
          skip = SkipToTest (reportTests report) (reportDiscards report)
          replayStr =
            if showReplay
              then
                "  --hedgehog-replay \""
                  ++ show (skipCompress skip)
                  ++ " "
                  ++ show seed
                  ++ "\""
              else ""
       in s ++ replayStr ++ "\n"
    GaveUp ->
      s
    OK ->
      -- do not report hedgehog successes because they are redundant with the Tasty report
      -- except if there is coverage information
      if not . P.null . P.toList . coverageLabels . reportCoverage $ report
        then s
        else ""

propertyTestLimit :: PropertyConfig -> TestLimit
propertyTestLimit =
  let getTestLimit (EarlyTermination _ tests) = tests
      getTestLimit (NoEarlyTermination _ tests) = tests
      getTestLimit (NoConfidenceTermination tests) = tests
   in getTestLimit . propertyTerminationCriteria

-- * OPTIONS DEFINITIONS

-- | The replay token to use for replaying a previous test run
newtype HedgehogReplay = HedgehogReplay (Maybe (Size, Seed))
  deriving (Typeable)

instance IsOption HedgehogReplay where
  defaultValue = HedgehogReplay Nothing

  parseValue v = HedgehogReplay . Just <$> replay
    where
      -- Reads a replay token in the form "{size} {seed}"
      replay = (,) <$> safeRead (unwords size) <*> safeRead (unwords seed)
      (size, seed) = splitAt 2 $ words v

  optionName = return "hedgehog-replay"

  optionHelp = return "Replay token to use for replaying a previous test run"

-- | If a test case fails, show a replay token for replaying tests
newtype HedgehogShowReplay = HedgehogShowReplay Bool
  deriving (Typeable)

instance IsOption HedgehogShowReplay where
  defaultValue = HedgehogShowReplay True

  parseValue = fmap HedgehogShowReplay . safeRead

  optionName = return "hedgehog-show-replay"

  optionHelp = return "Show a replay token for replaying tests"

-- | The number of successful test cases required before Hedgehog will pass a test
newtype HedgehogTestLimit = HedgehogTestLimit (Maybe TestLimit)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogTestLimit where
  defaultValue = HedgehogTestLimit Nothing

  parseValue = fmap (HedgehogTestLimit . Just . TestLimit) . safeRead

  optionName = return "hedgehog-tests"

  optionHelp = return "Number of successful test cases required before Hedgehog will pass a test"

-- | The number of discarded cases allowed before Hedgehog will fail a test
newtype HedgehogDiscardLimit = HedgehogDiscardLimit (Maybe DiscardLimit)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogDiscardLimit where
  defaultValue = HedgehogDiscardLimit Nothing

  parseValue = fmap (HedgehogDiscardLimit . Just . DiscardLimit) . safeRead

  optionName = return "hedgehog-discards"

  optionHelp = return "Number of discarded cases allowed before Hedgehog will fail a test"

-- | The number of shrinks allowed before Hedgehog will fail a test
newtype HedgehogShrinkLimit = HedgehogShrinkLimit (Maybe ShrinkLimit)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogShrinkLimit where
  defaultValue = HedgehogShrinkLimit Nothing

  parseValue = fmap (HedgehogShrinkLimit . Just . ShrinkLimit) . safeRead

  optionName = return "hedgehog-shrinks"

  optionHelp = return "Number of shrinks allowed before Hedgehog will fail a test"

-- | The number of times to re-run a test during shrinking
newtype HedgehogShrinkRetries = HedgehogShrinkRetries (Maybe ShrinkRetries)
  deriving (Eq, Ord, Show, Typeable)

instance IsOption HedgehogShrinkRetries where
  defaultValue = HedgehogShrinkRetries Nothing

  parseValue = fmap (HedgehogShrinkRetries . Just . ShrinkRetries) . safeRead

  optionName = return "hedgehog-retries"

  optionHelp = return "Number of times to re-run a test during shrinking"

-- * GROUPING

-- | This allows the discovery of Hedgehog properties and their grouping by module name
--   in the test report.
--   Extract the ModuleName option value for a given test and
--   group all the tests with that option into the same test group
groupByModuleName :: TestTree -> TestTree
groupByModuleName testTree =
  let grouped =
        assocs $
          foldTestTree
            ( trivialFold
                { foldSingle = \os n t ->
                    let (ModuleName aModuleName) = lookupOption os :: ModuleName
                     in insert (toS aModuleName) (setOptionSet os $ singleTest n t) empty
                }
            )
            mempty
            testTree
   in TestGroup "All" (uncurry TestGroup <$> grouped)

-- | Option describing the current module name
newtype ModuleName = ModuleName Text deriving (Eq, Show)

-- | This option is not used on the command line, it is just used to annotate test groups
instance IsOption ModuleName where
  defaultValue = ModuleName "root"
  parseValue = fmap ModuleName . safeRead
  optionName = pure "module-name"
  optionHelp = pure "internal option used to group tests into the same module"
  optionCLParser = mkFlagCLParser mempty (ModuleName "root")

instance (Ord k) => Semigroup (MultiMap k v) where
  (<>) m1 m2 = fromList (toList m1 <> toList m2)

instance (Ord k) => Monoid (MultiMap k v) where
  mempty = empty
  mappend = (<>)

-- | This is unfortunate. Due to the API for `foldTestTree` in Tasty
--   giving back the current `OptionSet` applicable to a single test
--   it is not possible to re-set those option values on that test
--   without listing them exhaustively. This means
--   that if other options are set on tests in that file, they need to be
--   added in that function
setOptionSet :: OptionSet -> TestTree -> TestTree
setOptionSet os =
  localOption (lookupOption os :: HedgehogTestLimit)
    . localOption (lookupOption os :: HedgehogShrinkLimit)
    . localOption (lookupOption os :: HedgehogReplay)

-- | Return the module name of the current callstack
getModuleName :: HasCallStack => Prelude.String
getModuleName =
  case getCallStack callStack of
    ((_, loc) : _) -> srcLocModule loc
    _ -> "root"
