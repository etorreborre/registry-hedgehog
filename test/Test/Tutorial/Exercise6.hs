{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise6 where

import Data.Registry.Hedgehog
import Data.Text as T
import Hedgehog hiding (test)
import Protolude
import Test.Tasty.Hedgehogx
import Test.Tutorial.DataModel
import Test.Tutorial.Exercise5

test_another_small_company = prop "make a small company" $
  runGens $ do
    setSmallCompany
    setEmployeeName
    setDepartmentName
    setGenS @Int (pure 1)
    setDistinctForS @Department @Text

    collect =<< forallS @Company

    setCycleChooserS @EmployeeStatus
    collect =<< forallS @EmployeeStatus
