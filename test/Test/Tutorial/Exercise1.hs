{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise1 where

import Data.Registry
import Data.Registry.Hedgehog
import Hedgehog hiding (test)
import Hedgehog.Gen
import Hedgehog.Range
import Protolude
import Test.Tutorial.DataModel

registry :: Registry _ _
registry =
  genFun Company
    +: genFun Department
    +: genFun Employee
    +: genVal genEmployeeStatus
    +: genVal genInt
    +: genVal genText
    +: mempty

genInt :: Gen Int
genInt = integral (linear 1 3)

genText :: Gen Text
genText = text (linear 1 10) alpha

genEmployeeStatus :: Gen EmployeeStatus
genEmployeeStatus = pure Permanent

-- this does not compile the registry is not complete
-- makeCompanyGen :: GenIO Company
-- makeCompanyGen = make @(GenIO Company) registry
