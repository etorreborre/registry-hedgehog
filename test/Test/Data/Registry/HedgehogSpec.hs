{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.HedgehogSpec where

import Control.Monad.Morph (hoist)
import Data.Registry
import Data.Registry.Hedgehog
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen hiding (print)
import Hedgehog.Internal.Seed as Seed (random)
import Hedgehog.Internal.Tree as Tree (NodeT (..), runTreeT)
import Hedgehog.Range
import Protolude
import Test.Data.Registry.Company
import Test.Data.Registry.Generators
import Test.Tasty.Hedgehogx

-- * This specification shows the usage of several features of this library

--   First of all you will notice that if you run `stack test`
--   all the properties of this file will be grouped under the Test.Data.Registry.HedgehogSpec test group

-- * DECLARING PROPERTIES

genWord :: MonadIO m => GenT m Text
genWord = do
  ws <- T.lines <$> liftIO (T.readFile "/usr/share/dict/words")
  Gen.element ws

test_a_simple_test =
  test "make a simple assertion, tested only once" $
    1 === 1

test_a_simple_property =
  prop "make a simple property, tested 100 times by default" $ do
    n <- forAll (integral (linear 1 3))
    n === n

test_a_property_200_times = minTestsOk 200 $
  prop "make a simple property, tested 200 times" $ do
    n <- forAll (integral (linear 1 3))
    n === n

test_a_property_no_shrinking = noShrink $
  prop "make a simple property, not shrinked in case of a failure" $ do
    n <- forAll (integral (linear 1 3))
    n === n

test_a_property_with_a_seed = withSeed "(Size 51) (Seed 35539461314630919 5029106023111197789)" $
  prop "make a simple property, with a specific seed to reproduce a failure" $ do
    n <- forAll (integral (linear 1 3))
    (n >= 1) === True

-- * USING registry

test_company_1 =
  prop "a company can be used for testing" $ do
    -- note that we are using forall and not forAll
    company <- forall @Company
    (not . null) (departments company) === True

-- Let's create some registry modifiers to constrain the generation
setOneDepartment = addFun $ listOfMinMax @Department 1 1

setOneEmployee = addFun $ listOfMinMax @Employee 1 1

setSmallCompany = setOneEmployee . setOneDepartment

test_small_company = prop "a small company has just one department and one employee" $ do
  company <- forallWith @Company setSmallCompany
  length (departments company) === 1
  let Just d = head $ departments company
  length (employees d) === 1

-- * We can also specialize some registry in a given context

--   For example we might want to generate shorter department names even
--   if Department is using Text values. To do this we specialize the Text
--   generator in the context of a Gen Department

genDepartmentName = T.take 5 . T.toUpper <$> genText

setDepartmentName = specializeGen @Department genDepartmentName

test_with_better_department_name = prop "a department must have a short capitalized name" $ do
  company <- forallWith @Company (setSmallCompany . setDepartmentName)
  -- uncomment to print the department names and inspect them
  -- print company
  let Just d = head $ departments company
  (T.length (_departmentName $ departmentName d) <= 5) === True

-- | Generate a value with a modified list of generators
forallWith :: forall a b c. (HasCallStack, Show a, Typeable a) => (Registry _ _ -> Registry b c) -> PropertyT IO a
forallWith f = withFrozenCallStack $ forAll $ genWith @a (f registry)

-- * Fresh identifiers using a state monad

test_fresh = minTestsOk 10000 $
  prop "we can generate terms with fresh ids" $ do
    -- let termGenerator = genTerm :: GenT (StateT Int Identity) Term
    m <- forAll $ runStateGen genTerm
    -- collect m
    m === m

genFixedText :: (MonadGen m) => m Text
genFixedText = pure "xxx"

genTerm :: (MonadGen m, Fresh m) => m Term
genTerm = Gen.recursive Gen.choice [genValue genFixedText] [Gen.subtermM2 genTerm genTerm makeExp]

genValue :: (MonadGen m, Fresh m) => m Text -> m Term
genValue g = do
  t <- g
  makeValue t

data Term
  = Value Int Text
  | Exp Int Term Term
  deriving (Eq, Show)

makeValue :: (Monad m, Fresh m) => Text -> m Term
makeValue t = do
  n <- fresh
  pure (Value n t)

makeExp :: (Monad m, Fresh m) => Term -> Term -> m Term
makeExp t1 t2 = do
  n <- fresh
  pure (Exp n t1 t2)

class Fresh m where
  fresh :: m Int

runFresh :: (Show a) => GenT (State Int) a -> PropertyT IO a
runFresh = forAll . runStateGen

runStateGen :: (Show a) => GenT (State Int) a -> Gen a
runStateGen = hoist (pure . flip evalState 0)

instance Fresh (GenT (State Int)) where
  fresh = do
    n <- get
    put (n + 1)
    pure n

-- | Sample GenT IO values
sampleGenIO :: GenT IO a -> IO a
sampleGenIO gen =
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
