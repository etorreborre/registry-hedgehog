{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-

This module unifies property based testing with Hedgehog and one-off tests.

-}
module Test.Tasty.Hedgehogx
  ( module Hedgehog,
    module Tasty,
    module Test.Tasty.HedgehogTest,
    -- * Tests definition
    prop,
    test,
    -- * Tests settings
    minTestsOk,
    noShrink,
    withSeed,
    -- * Running tests
    run,
    runOnly,
    -- * Assertions
    gotException,
    -- * Display
    printDifference,
    display,
  )
where

import qualified Data.Text as T
import GHC.Stack
import Hedgehog hiding (test)
import Hedgehog.Gen as Hedgehog hiding (discard, print)
import Hedgehog.Internal.Config (UseColor (EnableColor))
import Hedgehog.Internal.Property (Coverage (..), Diff (..), DiscardCount (..), ShrinkCount (..), TestCount (..))
import Hedgehog.Internal.Report
import Hedgehog.Internal.Show (mkValue, valueDiff)
import Protolude hiding (SrcLoc, empty, toList, (.&.))
import System.Environment
import Test.Tasty as Tasty
import Test.Tasty.HedgehogTest
import Test.Tasty.Options as Tasty
import Test.Tasty.Providers as Tasty (singleTest)
import Test.Tasty.Runners as Tasty
  ( TestTree (..),
    foldSingle,
    foldTestTree,
    trivialFold,
  )
import Prelude (String)

-- * TESTS AND PROPERTIES

-- | Create a Tasty test from a Hedgehog property
prop :: HasCallStack => TestName -> PropertyT IO () -> TestTree
prop name p =
  let aModuleName = getModuleName
   in withFrozenCallStack . localOption (ModuleName (toS aModuleName)) $
        testProperty name (Hedgehog.property p)

-- | Create a Tasty test from a Hedgehog property called only once
test :: HasCallStack => TestName -> PropertyT IO () -> TestTree
test name p = withFrozenCallStack (minTestsOk 1 . noShrink $ prop name p)

-- * SETTING TEST OPTIONS

-- | Set the minimum number of tests which must be successful for a property to pass
minTestsOk :: Int -> TestTree -> TestTree
minTestsOk n = localOption (HedgehogTestLimit (Just (toEnum n :: TestLimit)))

-- | Do not shrink failures
noShrink :: TestTree -> TestTree
noShrink = localOption (HedgehogShrinkLimit (Just (0 :: ShrinkLimit)))

-- | Execute a property with a specific seed
withSeed :: Prelude.String -> TestTree -> TestTree
withSeed seed tree =
  case parseValue seed of
    Nothing -> prop ("cannot parse seed " <> seed) failure
    Just (s :: HedgehogReplay) -> localOption s tree

-- * ASSERTIONS

-- | Assert that an exception is thrown
gotException :: forall a. (HasCallStack, Show a) => a -> PropertyT IO ()
gotException a = withFrozenCallStack $ do
  res <- liftIO (try (evaluate a) :: IO (Either SomeException a))
  case res of
    Left _ -> assert True
    Right _ -> annotateShow ("excepted an exception" :: Text) >> assert False

-- * REPORTING

printDifference :: (MonadIO m, Show a, Show b, HasCallStack) => a -> b -> m ()
printDifference actual expected = withFrozenCallStack $ do
  let failureReport = mkFailure (Size 0) (Seed 0 0) (ShrinkCount 0) Nothing Nothing "" (failureDifference actual expected) []
  report <- renderResult EnableColor Nothing (Report (TestCount 0) (DiscardCount 0) (Coverage mempty) (Failed failureReport))
  putText (T.unlines . drop 3 . T.lines . toS $ report)

failureDifference :: (Show a, Show b, HasCallStack) => a -> b -> Maybe Diff
failureDifference x y = withFrozenCallStack $
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      Nothing
    Just d ->
      withFrozenCallStack $
        Just $ Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" d

display :: (Show a, Monad m, HasCallStack) => a -> PropertyT m a
display a = withFrozenCallStack (annotateShow a $> a)

-- * GHCi run functions

-- | Run either a test tree (a test or a property) whether it is in IO or not
run :: Runnable t => t -> IO ()
run tests = runIt tests >>= defaultMain . groupByModuleName

-- | Run only some tests by passing a tasty pattern
runOnly :: Runnable t => Text -> t -> IO ()
runOnly p tests = do
  setEnv "TASTY_PATTERN" (toS p)
  run tests `finally` unsetEnv "TASTY_PATTERN"

-- | Typeclass to unify a simple test in a file like test_simple :: TestTree
--   and all the tests retrieved by tasty-discovery which have the type :: IO TestTree
class Runnable t where
  runIt :: t -> IO TestTree

instance Runnable (IO TestTree) where
  runIt t = t

instance Runnable TestTree where
  runIt = pure
