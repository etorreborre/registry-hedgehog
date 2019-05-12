{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Generators where

import           Data.Registry
import           Data.Registry.Hedgehog
import           Data.Registry.Hedgehog.TH
import           Data.Registry.TH
import           Hedgehog.Gen               as Gen hiding (lift, print)
import           Hedgehog.Internal.Gen      hiding (lift, print)
import           Hedgehog.Range
import           Protolude                  hiding (list)
import           Test.Data.Registry.Company
import           Test.Tasty.Hedgehogx       hiding (lift)

registry =
    genFun Company
 <: genFun Department
 <: genFun Employee
 -- we can generate data for different constructors in an ADT
 <: $(makeGenerators ''EmployeeStatus)
 -- we can generate Lists or Maybe of elements
 <: fun (listOf @Department)
 <: fun (listOf @Employee)
 <: fun (maybeOf @Int)
 <: genVal genInt
 <: genVal genText
 -- the default chooser for selecting generators
 <: genFun choiceChooser

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
