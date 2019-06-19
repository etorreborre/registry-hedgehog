{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.FeatSpec where

import           Data.Registry
import           Data.Registry.Feat
import           Data.Registry.Feat.TH
import           Data.Registry.Hedgehog           (GenIO, forAllT, listOf, listOfMinMax)
import           Hedgehog.Gen                     hiding (print)
import           Hedgehog.Range
import           Prelude                          as Prelude (show)
import           Protolude                        as P hiding (C1, D1, check, list)
import           Test.Data.Registry.Company
import           Test.Data.Registry.EnumerateData
import           Test.Tasty.Hedgehogx             hiding (check)

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

test_limit_size = prop "the number of values of the parts of an enumerate can be limited" $ do
  distribution         <- forall @[Integer]
  Enumerated enumerate <- forall @(Enumerated Int)
  let limited = limitPartsSizeWith distribution enumerate

  -- to visualize results
  print $ "distribution " <> (P.show distribution :: Text)
  print $ "original     " <> (P.show (valuesWith enumerate) :: Text)
  print $ "limited      " <> (P.show (valuesWith limited) :: Text)

  check distribution (fmap fst (valuesWith limited))

  where
    check (d:rest) (l:ps) = do
      (l <= d) === True
      check rest ps
    check _ _ = success

-- * HELPERS

newtype Enumerated a = Enumerated { _enumerated :: Enumerate a }

instance Show a => Show (Enumerated a) where
  show (Enumerated e) = Prelude.show (valuesWith e)

instance Eq a => Eq (Enumerated a) where
  Enumerated e1 == Enumerated e2 = valuesWith e1 == valuesWith e2

enumInt :: Enumerate Int
enumInt = bconcat (pure <$> [1..2])

enumBool :: Enumerate Bool
enumBool = pure True <|> pure False

enumText :: Enumerate Text
enumText = pure "abc" <|> pure "xyz"

registry =
     $(makeEnumerates ''EmployeeStatus)
  <: $(makeEnumerates ''T)
  <: $(makeEnumerates ''Bug)
  <: $(makeEnumerates ''A1)
  <: $(makeEnumerates ''B1)
  <: $(makeEnumerates ''C1)
  <: $(makeEnumerates ''D1)
  <: $(makeEnumerates ''Fuzzy)
  <: fun (enumListOf @EmployeeStatus)
  <: fun (enumListOfMinMax @Bool 0 10)
  <: fun (enumPairOf @Bool @Bool)
  <: fun enumInt
  <: fun enumBool
  <: fun (enumPairOf @Bool @Bool)
  <: fun (enumNonEmptyOfMinMax @Bool 1 1)
  <: fun (enumPairOf @Text @Int)
  <: fun enumText
  <: fun genInt
  <: fun genInteger
  <: fun genEnumeratedInt
  <: fun genBool
  <: fun (listOf @Int)
  <: fun (listOfMinMax @Integer 1 20)

genInt :: GenIO Int
genInt = integral (linear 1 10)

genInteger :: GenIO Integer
genInteger = integral (linear 1 10)

genEnumeratedInt :: GenIO (Enumerated Int)
genEnumeratedInt = do
  listOfInts <- listOf (listOf genInt)
  pure (Enumerated (fromParts (mconcat . fmap pure <$> listOfInts)))

genBool :: GenIO Bool
genBool = choice [pure True, pure False]

enumAll :: forall a . _ => PropertyT IO a
enumAll = withFrozenCallStack $ forAllT (enumToGen $ enumWith @a registry)

forall :: forall a . _ => PropertyT IO a
forall = withFrozenCallStack $ forAllT (makeUnsafe @(GenIO a) registry)
