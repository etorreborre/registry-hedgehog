{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise1 where

import Data.Registry
import Data.Registry.Hedgehog
import Hedgehog.Gen
import Hedgehog.Range
import Protolude
import Test.Tutorial.DataModel

registry :: Registry _ _
registry =
  genFun Company
    <: fun (listOfMinMax @Department 1 5)
    <: genFun Department
    <: fun (listOfMinMax @Employee 1 5)
    <: genFun Employee
    <: genVal genEmployeeStatus
    <: fun (maybeOf @Int)
    <: genVal genInt
    <: genVal genText

genInt :: Gen Int
genInt = integral (linear 1 3)

genText :: Gen Text
genText = text (linear 1 10) alpha

genEmployeeStatus :: Gen EmployeeStatus
genEmployeeStatus = pure Permanent

-- this does not compile the registry is not complete
-- makeCompanyGen :: Gen Company
-- makeCompanyGen = make @(Gen Company) registry
