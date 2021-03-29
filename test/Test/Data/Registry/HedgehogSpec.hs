{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.HedgehogSpec where

import Control.Monad.Morph (hoist)
import Data.IORef
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
  ( Applicative (pure),
    Bool (True),
    Eq,
    Foldable (length),
    IO,
    Int,
    Maybe (Just, Nothing),
    Monad ((>>)),
    MonadIO (..),
    MonadState (get, put),
    Num ((+), (-)),
    Ord ((<=), (>=)),
    Show,
    State,
    Text,
    evalState,
    flip,
    head,
    lift,
    panic,
    ($),
    (.),
    (<$>),
  )
import System.IO.Unsafe
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
    (length (departments company) >= 0) === True

-- Let's create some registry modifiers to constrain the generation
setOneDepartment = addFunS $ listOfMinMax @Department 1 1

setOneEmployee = addFunS $ listOfMinMax @Employee 1 1

setSmallCompany = setOneEmployee >> setOneDepartment

test_small_company =
<<<<<<< HEAD
  prop "a small company has just one department and one employee" $
    runS generators $ do
      setSmallCompany
      company <- forallS @Company
      length (departments company) === 1
      let Just d = head $ departments company
      length (employees d) === 1

-- * We can also specialize some generators in a given context

=======
  prop "a small company has just one department and one employee" $ runS registry $ do
    setSmallCompany
    company <- forallS @Company
    length (departments company) === 1
    let Just d = head $ departments company
    length (employees d) === 1

-- * We can also specialize some registry in a given context
>>>>>>> fix: fixed the implementation of makeGenerators
--   For example we might want to generate shorter department names even
--   if Department is using Text values. To do this we specialize the Text
--   generator in the context of a Gen Department

genDepartmentName = T.take 5 . T.toUpper <$> genText

setDepartmentName = specializeGenS @Department genDepartmentName

test_with_better_department_name = noShrink $
<<<<<<< HEAD
  prop "a department must have a short capitalized name" $
    runS generators $ do
      setSmallCompany
      setDepartmentName
      company <- forallS @Company
=======
  prop "a department must have a short capitalized name" $ runS registry $ do
    setSmallCompany
    setDepartmentName
    company <- forallS @Company
>>>>>>> fix: fixed the implementation of makeGenerators

      -- uncomment to print the department names and inspect them
      -- print company
      let Just d = head $ departments company
      (T.length (departmentName d) <= 5) === True

-- * It would be also very nice to have stateful generation where we can cycle

--   across different constructors for a given data type

test_cycle_constructors =
<<<<<<< HEAD
  prop "we can cycle deterministically across all the constructors of a data type" $
    runS generators $ do
      setCycleChooserS @EmployeeStatus
      -- uncomment to check
      -- collect =<< forallS @EmployeeStatus
      success

-- We can also make sure we generate distinct values for a given type
test_distinct_values =
  prop "we can generate distinct values for a given data type when used in a specific context" $
    runS generators $ do
      setDistinctForS @Department @Text
      -- uncomment to check
      -- collect =<< departmentName <$> forallS @Department
      success
=======
  prop "we can cycle deterministically across all the constructors of a data type" $ runS registry $ do
    setCycleChooserS @EmployeeStatus
    -- uncomment to check
    -- collect =<< forallS @EmployeeStatus
    success

-- We can also make sure we generate distinct values for a given type
test_distinct_values =
  prop "we can generate distinct values for a given data type when used in a specific context" $ runS registry $ do
   setDistinctForS @Department @Text
   -- uncomment to check
   -- collect =<< departmentName <$> forallS @Department
   success

>>>>>>> fix: fixed the implementation of makeGenerators

test_ints_generator =
  prop "we can generate ints" $ do
    n <- forAllT distinctInt
    n === n -- collect n

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

{-# NOINLINE distinctInt #-}
distinctInt :: GenIO Int
distinctInt = unsafePerformIO $ do
  ref <- newIORef (0 :: Int)
  pure $ distinctIntGenerator ref

distinctIntGenerator :: IORef Int -> GenIO Int
distinctIntGenerator ref = do
  i <- lift $ readIORef ref
  lift $ writeIORef ref (i + 1)
  pure i
