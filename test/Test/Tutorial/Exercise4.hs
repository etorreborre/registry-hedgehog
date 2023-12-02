{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise4 where

import Data.Registry
import Data.Registry.Hedgehog
import Data.Text as T
import Hedgehog hiding (test)
import Protolude
import Test.Tasty.Hedgehogx
import Test.Tutorial.DataModel
import Test.Tutorial.Exercise2 (genText)
import Test.Tutorial.Exercise3 (registry3)

registry12 :: Registry _ _
registry12 = specializeGen @Department genDepartmentName registry3

genDepartmentName :: Gen Text
genDepartmentName = T.take 5 . T.toUpper <$> genText

forSome :: forall a. (Typeable a, Show a) => PropertyT IO a
forSome = withFrozenCallStack $ forAll $ genWith @a registry12

test_deparment_name = prop "make a department" $ do
  department <- forSome @Department
  collect (departmentName department)
