{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise2 where

import Data.Registry
import Data.Registry.Hedgehog
import Hedgehog hiding (test, (===), forAll)
import Hedgehog.Gen
import Hedgehog.Range
import Protolude
import Test.Tasty.Hedgehogx
import Test.Tutorial.DataModel

registry :: Registry _ _
registry =
  genFun Company
    <: fun (listOf @Department)
    <: genFun Department
    <: fun (listOf @Employee)
    <: genFun Employee
    <: genFun genEmployeeStatus
    <: fun (maybeOf @Int)
    <: genVal genInt
    <: genVal genText

genInt :: Gen Int
genInt = integral (linear 1 3)

genText :: Gen Text
genText = text (linear 1 10) alpha

genEmployeeStatus :: Gen EmployeeStatus
genEmployeeStatus = pure Permanent

-- this compiles ok now
makeCompanyGen :: Gen Company
makeCompanyGen = make @(Gen Company) registry

forSome :: forall a. (Typeable a, Show a) => PropertyT IO a
forSome = withFrozenCallStack $ forAll $ genWith @a registry

test_company = test "make a company" $ do
  _ <- forSome @Company
  success
