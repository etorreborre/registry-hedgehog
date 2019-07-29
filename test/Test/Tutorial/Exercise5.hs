{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise5 where

import           Data.Registry
import           Data.Text as T
import           Data.Registry.Hedgehog
import           Hedgehog                 hiding (test)
import           Protolude
import           Test.Tasty.Hedgehogx
import           Test.Tutorial.DataModel
import           Test.Tutorial.Exercise2 (genText)
import           Test.Tutorial.Exercise3 (registry3)
import           Test.Tutorial.Exercise4 (genDepartmentName)

runGens = runS registry3

genEmployeeName :: Gen Text
genEmployeeName = T.take 10 . T.toLower <$> genText

setDepartmentName = specializeGenS @Department genDepartmentName
setEmployeeName   = specializeGenS @Employee genEmployeeName

setOneDepartment = addFunS $ listOfMinMax @Department 1 1
setOneEmployee   = addFunS $ listOfMinMax @Employee 1 1
setSmallCompany = setOneEmployee >> setOneDepartment

test_small_company = prop "make a small company" $ runGens $ do
  setSmallCompany
  setEmployeeName
  setDepartmentName
  collect =<< forallS @Company
