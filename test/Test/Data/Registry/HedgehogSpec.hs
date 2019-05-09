{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.HedgehogSpec where

import           Data.Registry
import           Data.Registry.Hedgehog
import           Data.Registry.TH
import qualified Data.Text                 as T (length, take, toUpper)
import           Hedgehog.Gen              as Gen hiding (lift, print)
import           Hedgehog.Internal.Gen     hiding (print, lift)
import           Hedgehog.Range
import           Protolude                 hiding (list)
import           Test.Tasty.Hedgehogx      hiding (lift)

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

-- * WORKING WITH GENERATORS

-- Some complex nested data
data Company = Company {
  companyName :: Text
, departments :: [Department]
} deriving (Eq, Show)

data Department = Department {
  departmentName :: Text
, employees      :: [Employee]
} deriving (Eq, Show)

data Employee = Employee {
  employeeName   :: Text
, employeeStatus :: EmployeeStatus
, salary         :: Int
, bonus          :: Maybe Int
} deriving (Eq, Show)

-- | Note that this is an ADT with several constructors
data EmployeeStatus =
   Permanent
 | Temporary Int -- number of days
 deriving (Eq, Show)

registry =
     funTo @Gen Company
  +: funTo @Gen Department
  +: funTo @Gen Employee
  -- we can generate data for different constructors in an ADT
  +: fun   genEmployeeStatus
  +: funTo @Gen (tag @"permanent" Permanent)
  +: funTo @Gen (tag @"temporary" Temporary)
  -- we can generate Lists or Maybe of elements
  +: fun (listOf @Department)
  +: fun (listOf @Employee)
  +: fun (maybeOf @Int)
  +: fun genInt
  +: fun genText
  +: end

-- | We can also generate data for ADTs having several constructors by tagging
--   the generators for each constructor.
--   Then, here, we equally generate permanent and temp employees
genEmployeeStatus :: Gen (Tag "permanent" EmployeeStatus) -> Gen (Tag "temporary" EmployeeStatus) -> Gen EmployeeStatus
genEmployeeStatus g1 g2 = choice [unTag <$> g1, unTag <$> g2]

genInt :: Gen Int
genInt = integral (linear 1 3)

genText :: Gen Text
genText = text (linear 1 10) alpha

-- we need this if the registry and the TH check are done in the same file
-- See the gory details of why this is necessary: https://gitlab.haskell.org/ghc/ghc/issues/9813
$(return [])

-- | Check that the registry is complete. This speeds up compilation when there are lots of generators
generators :: Registry _ _
generators = $(checkRegistry 'registry)

-- | We create a forall function using all the generators
forall :: forall a . _ => PropertyT IO a
forall = withFrozenCallStack $ forAll (genWith @a generators)

-- Now we can write more properties!

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
setDepartmentName = modify (specialize @(Gen Department) genDepartmentName)

test_with_better_department_name =
  prop "a department must have a short capitalized name" $ runS generators $ do
    setDepartmentName
    setSmallCompany
    company <- forallS @Company

    -- uncomment to print the department names and inspect them
    -- print company
    let Just d = head $ departments company
    (T.length (departmentName d) <= 5) === True
