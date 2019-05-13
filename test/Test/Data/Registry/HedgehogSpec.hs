{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.HedgehogSpec where

import           Data.List                     (nub)
import           Data.Registry
import           Data.Registry.Hedgehog
import qualified Data.Text                     as T (length, take, toUpper)
import           Hedgehog.Internal.Gen         hiding (print)
import           Hedgehog.Range
import           Protolude                     hiding (list)
import           Test.Data.Registry.Company
import           Test.Data.Registry.Generators
import           Test.Tasty.Hedgehogx

-- * This specification shows the usage of several features of this library
--   First of all you will notice that if you run `stack test`
--   all the properties of this file will be grouped under the Test.Data.Registry.HedgehogSpec test group

-- * DECLARING PROPERTIES

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

-- * USING GENERATORS

test_company_1 =
  prop "a company can be used for testing" $ do
    -- note that we are using forall and not forAll
    company <- forall @Company
    (length (departments company) >= 0) === True

-- Let's create some registry modifiers to constrain the generation
setOneDepartment = addFunS $ listOfMinMax @Department 1 1
setOneEmployee   = addFunS $ listOfMinMax @Employee 1 1
setSmallCompany = setOneEmployee >> setOneDepartment

test_small_company =
  prop "a small company has just one department and one employee" $ runS generators $ do
    setSmallCompany
    company <- forallS @Company
    length (departments company) === 1
    let Just d = head $ departments company
    length (employees d) === 1

-- * We can also specialize some generators in a given context
--   For example we might want to generate shorter department names even
--   if Department is using Text values. To do this we specialize the Text
--   generator in the context of a Gen Department

genDepartmentName = T.take 5 . T.toUpper <$> genText
setDepartmentName = specializeGenS @Department genDepartmentName

test_with_better_department_name = noShrink $
  prop "a department must have a short capitalized name" $ runS generators $ do
    setSmallCompany
    setDepartmentName
    company <- forallS @Company

    -- uncomment to print the department names and inspect them
    -- print company
    let Just d = head $ departments company
    (T.length (departmentName d) <= 5) === True

-- * It would be also very nice to have stateful generation where we can cycle
--   across different constructors for a given data type

test_cycle_constructors = noShrink $
  prop "we can cycle deterministically across all the constructors of a data type" $ runS generators $ do
    setGenS @Int (pure 1)
    setCycleChooserS @EmployeeStatus

    names <- replicateM 10 (forallS @EmployeeStatus)
    names === take 10 (join $ repeat [Permanent, Temporary 1])

-- We can also make sure we generate distinct values for a given type
test_distinct_values =
  prop "we can generate distinct values for a given data type when used in a specific context" $ runS generators $ do
   setDistinctForS @Department @Text

   departments <- replicateM 10 (forallS @Department)
   let names = departmentName <$> departments
   names === nub names
