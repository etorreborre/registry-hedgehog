{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Registry.Internal.TH where

import Control.Monad.Fail (fail)
import Data.Registry.Internal.Hedgehog
import Data.Text (splitOn, toLower)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import Prelude (last)

-- | Create a generator for selecting between constructors of an ADT
--   One parameter is a GenIO Chooser in order to be able to later on
--   switch the selection strategy
makeSelectGenerator :: Name -> [Con] -> ExpQ
makeSelectGenerator name constructors = do
  chooserParam <- [p|(chooser :: GenIO Chooser)|]
  otherParams <- traverse (parameterFor name) constructors
  untaggedGenerators <- traverse untagGenerator constructors
  expression <- appE (appE (varE (mkName "chooseOne")) (varE (mkName "chooser"))) (pure $ ListE untaggedGenerators)
  pure $ LamE (chooserParam : otherParams) expression
  where
    parameterFor :: Name -> Con -> Q Pat
    parameterFor typeName constructor = do
      constructorParam <- constructorParameterName constructor
      constructorTag <- tagName constructor
      sigP (varP constructorParam) (appT (conT (mkName "GenIO")) (appT (appT (conT (mkName "Tag")) (litT (strTyLit (show constructorTag)))) (conT typeName)))

-- Create a generator expression for a specific constructor of a data type
-- runQ [|tag @"permanent" Permanent|]
-- AppE (AppTypeE (VarE Data.Registry.Lift.tag) (LitT (StrTyLit "permanent"))) (ConE Test.Data.Registry.Generators.Permanent)
makeConstructorGenerator :: Con -> ExpQ
makeConstructorGenerator constructor = do
  constructorTag <- tagName constructor
  constructorType <- nameOf constructor
  appE (appTypeE (varE (mkName "tag")) (litT (strTyLit (show constructorTag)))) (conE constructorType)

-- | Remove the tag of a given constructor: fmap unTag g :: GenIO (Tag "t" SomeType) -> GenIO SomeType
untagGenerator :: Con -> ExpQ
untagGenerator constructor = do
  constructorParam <- constructorParameterName constructor
  appE (appE (varE (mkName "fmap")) (varE (mkName "unTag"))) (varE constructorParam)

-- | Create a tag used to distinguish constructors in an ADT
tagName :: Con -> Q Name
tagName constructor = do
  name <- constructorName constructor
  pure $ mkName $ toS name

-- | Same as the tag name but lower cased
constructorParameterName :: Con -> Q Name
constructorParameterName constructor = do
  name <- constructorName constructor
  pure $ mkName (toS $ toLower name)

-- | Extract the last name of a constructor
constructorName :: Con -> Q Text
constructorName constructor = do
  n <- nameOf constructor
  pure $ last . splitOn "." $ show n

-- | The name of a given constructor
nameOf :: Con -> Q Name
nameOf (NormalC n _) = pure n
nameOf (RecC n _) = pure n
nameOf other = do
  qReport True ("we can only create generators for normal constructors and records, got: " <> show other)
  fail "generators creation failed"

-- | The list of types necessary to create a given constructor
typesOf :: Con -> Q [Type]
typesOf (NormalC _ types) = pure (snd <$> types)
typesOf (RecC _ types) = pure $ (\(_, _, t) -> t) <$> types
typesOf other = do
  qReport True ("we can only create generators for normal constructors and records, got: " <> show other)
  fail "generators creation failed"

-- | runQ [| fun g <: genFun e <: genFun f|]
--   InfixE (Just (AppE (VarE Data.Registry.Registry.fun) (UnboundVarE g))) (VarE <:) (Just (InfixE (Just (AppE (VarE Data.Registry.Hedgehog.genFun) (UnboundVarE e)))
--  (VarE Data.Registry.Registry.<:) (Just (AppE (VarE Data.Registry.Hedgehog.genFun) (UnboundVarE f)))))
assembleGeneratorsToRegistry :: Exp -> [Exp] -> ExpQ
assembleGeneratorsToRegistry _ [] =
  fail "generators creation failed"
assembleGeneratorsToRegistry selectorGenerator [g] =
  let r = appendExpressions (genFunOf (pure g)) (funOf (pure selectorGenerator))
   in appendExpressions r (genFunOf (varE (mkName "choiceChooser")))
assembleGeneratorsToRegistry selectorGenerator (g : gs) =
  appendExpressions (genFunOf (pure g)) (assembleGeneratorsToRegistry selectorGenerator gs)

appendExpressions :: ExpQ -> ExpQ -> ExpQ
appendExpressions e1 e2 =
  infixE (Just e1) (varE (mkName "<:")) (Just e2)

genFunOf :: ExpQ -> ExpQ
genFunOf = appE (varE (mkName "genFun"))

funOf :: ExpQ -> ExpQ
funOf = appE (varE (mkName "fun"))
