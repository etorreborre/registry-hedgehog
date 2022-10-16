{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise3 where

import Data.Registry
import Data.Registry.Hedgehog
import Data.Registry.Hedgehog.TH
import Hedgehog hiding (test)
import Protolude
import Test.Tasty.Hedgehogx
import Test.Tutorial.DataModel
import Test.Tutorial.Exercise2 (registry)

registry3 :: Registry _ _
registry3 =
  $(makeGenerators ''EmployeeStatus)
    <: registry

forall :: forall a. (Typeable a, Show a) => PropertyT IO a
forall = withFrozenCallStack $ forAll $ genWith @a registry3

test_employee_status = prop "make an employee status" $ do
  status <- forall @EmployeeStatus
  collect status
