{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Generators where

import           Data.Registry
import           Data.Registry.Hedgehog
import           Data.Registry.TH
import           Hedgehog.Gen           as Gen hiding (lift, print)
import           Hedgehog.Internal.Gen  hiding (lift, print)
import           Hedgehog.Range
import           Protolude              hiding (list)
import           Test.Tasty.Hedgehogx   hiding (lift)

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
    genFun Company
 +: genFun Department
 +: genFun Employee
 -- we can generate data for different constructors in an ADT
 +: fun genEmployeeStatus
 +: genFun (tag @"permanent" Permanent)
 +: genFun (tag @"temporary" Temporary)
 -- we can generate Lists or Maybe of elements
 +: fun (listOf @Department)
 +: fun (listOf @Employee)
 +: fun (maybeOf @Int)
 +: genVal genInt
 +: genVal genText
 -- the default chooser for selecting generators
 +: genFun choiceChooser
 +: end

-- | We can also generate data for ADTs having several constructors by tagging
--   the generators for each constructor.
--   We use a "Chooser" to select a strategy for selecting specific generators
--     the default is Gen.choice, but you can also modify the registry with setCycleChooserS
--     to specialize the choosing and cycle through constructors
genEmployeeStatus :: GenIO Chooser -> GenIO (Tag "permanent" EmployeeStatus) -> GenIO (Tag "temporary" EmployeeStatus) -> GenIO EmployeeStatus
genEmployeeStatus chooser g1 g2 = chooseOne chooser [unTag <$> g1, unTag <$> g2]

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
forall = withFrozenCallStack $ forAllT (genWith @a generators)
