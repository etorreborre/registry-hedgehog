{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise5 where

import Data.Registry
import Data.Registry.Hedgehog
import Data.Registry.State
import Data.Text as T
import Hedgehog hiding (test)
import Protolude
import Test.Tasty.Hedgehogx
import Test.Tutorial.DataModel
import Test.Tutorial.Exercise2 (genText)
import Test.Tutorial.Exercise3 (registry3)
import Test.Tutorial.Exercise4 (genDepartmentName)

test_small_company = prop "make a small company" $ do
  company <- forallWith @Company (setSmallCompany . setEmployeeName . setDepartmentName)
  collect company

genEmployeeName :: Gen Text
genEmployeeName = T.take 10 . T.toLower <$> genText

setDepartmentName :: Registry _ _ -> Registry _ _
setDepartmentName = specializeGen @Department genDepartmentName

setEmployeeName :: Registry _ _ -> Registry _ _
setEmployeeName = specializeGen @Employee genEmployeeName

setOneDepartment :: Registry _ _ -> Registry _ _
setOneDepartment = addFun (listOfMinMax @Department 1 1)

setOneEmployee :: Registry _ _ -> Registry _ _
setOneEmployee = addFun (listOfMinMax @Employee 1 1)

setSmallCompany :: Registry _ _ -> Registry _ _
setSmallCompany = setOneEmployee . setOneDepartment

-- | Generate a value with a modified list of generators
forallWith :: forall a b c. (HasCallStack, Show a, Typeable a) => (Registry _ _ -> Registry b c) -> PropertyT IO a
forallWith f = withFrozenCallStack $ forAll $ genWith @a (f registry3)
