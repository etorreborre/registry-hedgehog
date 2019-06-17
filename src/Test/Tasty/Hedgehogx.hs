{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-

This module unifies property based testing with Hedgehog and one-off tests.

-}
module Test.Tasty.Hedgehogx (
  module Hedgehog
, module Tasty
, gotException
, groupByModuleName
, minTestsOk
, mustBe
, noShrink
, prop
, run
, runOnly
, test
, withSeed
, (===)
) where

import           Data.Maybe           (fromJust)
import           Data.MultiMap        hiding (foldr)
import           GHC.Stack
import           Hedgehog             as Hedgehog hiding (test, (===))
import qualified Hedgehog             as Hedgehog ((===))
import           Hedgehog.Gen         as Hedgehog hiding (discard, print)
import           Prelude              (String)
import qualified Prelude              as Prelude
import           Protolude            hiding (SrcLoc, empty, toList, (.&.))
import           System.Environment
import           Test.Tasty           as Tasty
import           Test.Tasty.Hedgehog  as Tasty
import           Test.Tasty.Options   as Tasty
import           Test.Tasty.Providers as Tasty (singleTest)
import           Test.Tasty.Runners   as Tasty (TestTree (..), foldSingle,
                                                foldTestTree, trivialFold)

-- * TESTS AND PROPERTIES

-- | Create a Tasty test from a Hedgehog property
prop :: HasCallStack => TestName -> PropertyT IO () -> TestTree
prop name p = withFrozenCallStack $
  let aModuleName = getModuleName
  in  localOption (ModuleName (toS aModuleName)) $
      testProperty name (Hedgehog.property p)

-- | Create a Tasty test from a Hedgehog property called only once
test :: HasCallStack => TestName -> PropertyT IO () -> TestTree
test name p = withFrozenCallStack (minTestsOk 1 . noShrink $ prop name p)

-- * ASSERTIONS

-- | Assert that an exception is thrown
gotException :: forall a . (HasCallStack, Show a) => a -> PropertyT IO ()
gotException a = withFrozenCallStack $ do
  res <- liftIO (try (evaluate a) :: IO (Either SomeException a))
  case res of
    Left _  -> assert True
    Right _ -> annotateShow ("excepted an exception" :: Text) >> assert False


-- | Redefinition of Hedgehog's === operator to add better source file information
--
--   It adds a link to the source file which is conforms to
--   output of GHC.Exception. See makeSourceLink
--   It also displays the actual and expected values in a compact form before
--   the pretty-printed form provided by Hedgehog
infix 4 ===
(===) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
actual === expected = withFrozenCallStack $ do
  displayActualAndExpectedValues actual expected
  actual Hedgehog.=== expected

-- | An equality assertion which does not try to do a smart diffs for cases
--   where the output is too large.
mustBe :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
actual `mustBe` expected = do
  ok <- eval (actual == expected)
  if ok then
    success
  else withFrozenCallStack $ do
    displayActualAndExpectedValues actual expected
    failure

-- | Display actual and expected values as a footnote, using the Show a instance
displayActualAndExpectedValues :: (Show a, MonadTest m, HasCallStack) => a -> a -> m ()
displayActualAndExpectedValues actual expected =
  withFrozenCallStack $ do
    footnote makeSourceLink
    footnote "\n"
    footnote $ "Expected\n" <> (show expected)
    footnote "\n"
    footnote $ "Actual\n" <> (show actual)

-- | This function creates a link to the source file which is conforms to
--   the output of GHC.Exception and thus can be navigated to with some text editors (like emacs)
--   by specifying a regular expression like
--   (".*error, called at \\(.*\\.hs\\):\\([0-9]+\\):\\([0-9]+\\) in .*" 1 2 3 2 1)
--      (" +\\(.*\\.hs\\):\\([0-9]+\\):$" 1 2 nil 2 1)
makeSourceLink :: (HasCallStack) => String
makeSourceLink =
  case getCallStack callStack of
    [] -> "FAIL!"
    (f, SrcLoc {..}) : _ ->
      f ++ " error, called at " ++
      foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]


-- * SETTINGS

-- | Set a mininum number of tests to be successful on a property
minTestsOk :: Int -> TestTree -> TestTree
minTestsOk n = localOption (HedgehogTestLimit (Just (toEnum n :: TestLimit)))

-- | Don't shrink failures
noShrink :: TestTree -> TestTree
noShrink = localOption (HedgehogShrinkLimit (Just (0 :: ShrinkLimit)))

-- | Run a property with a specify seed. You can copy and paste the exact string
--   which Hedgehog outputs when there is a failure
withSeed :: Prelude.String -> TestTree -> TestTree
withSeed seed = localOption (fromJust (parseValue seed :: Maybe HedgehogReplay))

-- * GROUPING

-- | This allows the discovery of Hedgehog properties and their grouping by module name
--   in the test report.
--   Extract the ModuleName option value for a given test and
--   group all the tests with that option into the same test group
groupByModuleName :: TestTree -> TestTree
groupByModuleName testTree =
  let grouped = assocs $ foldTestTree (trivialFold { foldSingle = \os n t ->
        let (ModuleName aModuleName) = lookupOption os :: ModuleName
        in insert (toS aModuleName) (setOptionSet os $ singleTest n t) empty
        }) mempty testTree
  in  TestGroup "All" (uncurry TestGroup <$> grouped)

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
  localOption (lookupOption os :: HedgehogTestLimit) .
  localOption (lookupOption os :: HedgehogShrinkLimit) .
  localOption (lookupOption os :: HedgehogReplay)

-- | Return the module name of the current callstack
getModuleName :: HasCallStack => Prelude.String
getModuleName =
  case getCallStack  callStack of
    ((_, loc):_) -> srcLocModule loc
    _            -> "root"

-- | Option describing the current module name
newtype ModuleName = ModuleName Text deriving (Eq, Show)

-- | This option is not used on the command line, it is just used to annotate test groups
instance IsOption ModuleName where
  defaultValue = ModuleName "root"
  parseValue = fmap ModuleName . safeRead
  optionName = pure "module-name"
  optionHelp = pure "internal option used to group tests into the same module"
  optionCLParser = mkFlagCLParser mempty (ModuleName "root")

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
