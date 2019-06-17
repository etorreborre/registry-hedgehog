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
import           Data.Registry.Hedgehog
import           Protolude                        hiding (C1, D1, list)
import           Test.Data.Registry.Company
import           Test.Data.Registry.EnumerateData
import           Test.Tasty.Hedgehogx

test_enumSizedListOf = test "we can enumerate a list of elements with a fixed size" $ do
  let listOfSize2 = enumSizedListOf 2 enumInt

  -- the first lists have 2 elements
  valuesWith listOfSize2 === [(4, [[1, 1], [1, 2], [2 , 1], [2, 2]])]

test_th = test "we can make an enumerate for an ADT" $ do
  let r =  fun enumInt +: $(makeEnums ''EmployeeStatus)
  let status = make @(Enumerate EmployeeStatus) r

  valuesWith status === [(3, [Permanent, Temporary 1, Temporary 2])]

test_to_gen = maxSize 10000 $ minTestsOk 10000 $ prop "an enumerate can be transformed into a generator with enumAll" $ do
  _ <- enumAll @[EmployeeStatus]
  -- print values
  success

test_enumeration_order = test "constructors must be enumerated alternatively" $ do
  let e = enumWith @T registry
  valuesWith e ===
    [(14
    , [ A Yes
      , B True
      , C (C1 True)
      , D (D1 True)
      , A No
      , B False
      , C (C2 True)
      , D (D2 True)
      , A What
      , C (C1 False)
      , D (D1 False)
      , A WhoKnows
      , C (C2 False)
      , D (D2 False)
      ]
    )
    ]

test_regression = test "fix bug on bconcat on a simple case" $ do
  let values = make @(Enumerate Bug) registry
  valuesWith values ===
    [(7
      , [ Bug1
        , Bug2 True
        , Bug3 True True
        , Bug2 False
        , Bug3 True False
        , Bug3 False True
        , Bug3 False False
        ]
    )
    ]

test_bconcat = test "bconcat must collect values in a breadth first order" $ do
  let values = bconcat $ [
         pure 1
       , pure 2 <|> pure 6 <|> pure 9
       , pure 3
       , pure 4 <|> pure 7 <|> pure 10 <|> pure 11 <|> pure 12
       , pure 5 <|> pure 8
       ]

  valuesWith values === [(12, [1..12])]

-- * HELPERS

enumInt :: Enumerate Int
enumInt = bconcat (pure <$> [1..2])

enumBool :: Enumerate Bool
enumBool = pure True <|> pure False

enumText :: Enumerate Text
enumText = pure "abc" <|> pure "xyz"

registry =
     fun (enumListOf @EmployeeStatus)
  <: fun (enumListOfMinMax @Bool 0 10)
  <: fun (enumPairOf @Bool @Bool)
  <: fun enumInt
  <: fun enumBool
  <: fun (enumPairOf @Bool @Bool)
  <: fun (enumNonEmptyOfMinMax @Bool 1 1)
  <: fun (enumPairOf @Text @Int)
  <: fun enumText
  <: $(makeEnums ''EmployeeStatus)
  <: $(makeEnums ''T)
  <: $(makeEnums ''Bug)
  <: $(makeEnums ''A1)
  <: $(makeEnums ''B1)
  <: $(makeEnums ''C1)
  <: $(makeEnums ''D1)
  <: $(makeEnums ''Fuzzy)


enumAll :: forall a . _ => PropertyT IO a
enumAll = withFrozenCallStack $ forAllT (enumToGen $ enumWith @a registry)
