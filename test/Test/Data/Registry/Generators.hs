{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Generators where

import Data.Registry
import Data.Registry.Hedgehog
import Data.Registry.Hedgehog.TH
import Hedgehog.Gen as Gen hiding (print)
import Hedgehog.Internal.Gen hiding (print)
import Hedgehog.Range
import Protolude hiding (list)
import Test.Data.Registry.Company
import Test.Tasty.Hedgehogx

registry =
  genFun Company
    <: fun (listOf @Department)
    <: genFun Department
    <: fun (listOf @Employee)
    <: genFun Employee
    -- we can generate data for different constructors in an ADT with some Template Haskell
    <: $(makeGenerators ''EmployeeStatus)
    <: fun (maybeOf @Int)
    -- we can generate Lists or Maybe of elements
    <: genVal genInt
    <: genVal genText

genInt :: Gen Int
genInt = integral (linear 1 3)

genText :: Gen Text
genText = text (linear 1 10) alpha

-- we need this if the registry and the TH check are done in the same file
-- See the gory details of why this is necessary: https://gitlab.haskell.org/ghc/ghc/issues/9813
$(return [])

-- | We create a forall function using all the generators
forall :: forall a. _ => PropertyT IO a
forall = withFrozenCallStack $ forAllT (genWith @a registry)
