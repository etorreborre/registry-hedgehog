{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.TestingFeatSpec where

import           Data.Registry
import           Data.Registry.Feat
import           Data.Registry.Feat.TH
import           Protolude             hiding (list)
import           Test.Feat.Access
import           Test.Feat.Enumerate
import           Test.Data.Registry.Company
import           Test.Tasty.Hedgehogx

test_enumSizedListOf = test "we can enumerate a list of elements with a fixed size" $ do
  let listOfSize2 = enumSizedListOf 2 enumInt

  -- the first lists have 2 elements
  head (valuesWith listOfSize2) === Just (4, [[0, 0], [0, -1], [-1 , 0], [-1 , -1]])

  -- for a given part/size if we get more elements we increase the int values but the list
  -- stays with 2 elements
  selectWith listOfSize2 2 1 === [0, 3]
  selectWith listOfSize2 2 10 === [-2, 1]

test_th = test "we can make an enumerate for an ADT" $ do
  let r =  fun enumInt +: $(makeEnums ''EmployeeStatus)
  let status = make @(Enumerate EmployeeStatus) r

  selectWith status 0 0 === Permanent
  selectWith status 0 1 === Temporary 0
  selectWith status 1 0 === Temporary 1


-- * HELPERS

enumInt = optimal :: Enumerate Int
