{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Registry.Internal.TH where

import Control.Monad.Fail (fail)
import Data.Registry.Internal.Hedgehog
import Data.Text (splitOn)
import Hedgehog
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude hiding (Type)
import Prelude (last)

-- | Options for changing the generated TH code
--    if checked = True then we use the <: operator which checks input
--    otherwise we use the <+ operator which doesn't check inputs
newtype GeneratorOptions = GeneratorOptions
  { checked :: Bool
  }
  deriving (Eq, Show)

-- | By default we want to typecheck the generators
defaultGeneratorOptions :: GeneratorOptions
defaultGeneratorOptions = GeneratorOptions True

-- | Create a generator for selecting between constructors of an ADT
--   One parameter is a Gen Chooser in order to be able to later on
--   switch the selection strategy
makeSelectGenerator :: Name -> [Con] -> ExpQ
makeSelectGenerator name constructors = do
  chooserParam <- [p|(chooser :: Gen Chooser)|]
  otherParams <- traverse (parameterFor name) constructors
  untaggedGenerators <- traverse untagGenerator constructors
  expression <- appE (appE (varE (mkName "chooseOne")) (varE (mkName "chooser"))) (pure $ ListE untaggedGenerators)
  pure $ LamE (chooserParam : otherParams) expression
  where
    parameterFor :: Name -> Con -> Q Pat
    parameterFor typeName constructor = do
      constructorParam <- constructorParameterName constructor
      constructorTag <- tagName constructor
      sigP (varP constructorParam) (appT (conT (mkName "Gen")) (appT (appT (conT (mkName "Tag")) (litT (strTyLit (show constructorTag)))) (conT typeName)))

-- Create a generator expression for a specific constructor of a data type
-- runQ [|tag @"permanent" Permanent|]
-- AppE (AppTypeE (VarE Data.Registry.Lift.tag) (LitT (StrTyLit "permanent"))) (ConE Test.Data.Registry.Generators.Permanent)
makeConstructorGenerator :: Con -> ExpQ
makeConstructorGenerator constructor = do
  constructorTag <- tagName constructor
  constructorType <- nameOf constructor
  appE (appTypeE (varE (mkName "tag")) (litT (strTyLit (show constructorTag)))) (conE constructorType)

-- | Remove the tag of a given constructor: fmap unTag g :: Gen (Tag "t" SomeType) -> Gen SomeType
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
  pure $ mkName (toLower <$> toS name)

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

-- | runQ [| fun g +: genFun e +: genFun f|]
--   InfixE (Just (AppE (VarE Data.Registry.Registry.fun) (UnboundVarE g))) (VarE +:) (Just (InfixE (Just (AppE (VarE Data.Registry.Hedgehog.genFun) (UnboundVarE e)))
--  (VarE Data.Registry.Registry.+:) (Just (AppE (VarE Data.Registry.Hedgehog.genFun) (UnboundVarE f)))))
assembleGeneratorsToRegistry :: GeneratorOptions -> Exp -> [Exp] -> ExpQ
assembleGeneratorsToRegistry options selectorGenerator generators =
  app options (funOf (pure selectorGenerator)) $
    app options (genFunOf (varE (mkName "choiceChooser"))) $
      go generators
  where
    go [] = fail "generators creation failed"
    go [g] = genFunOf (pure g)
    go (g:gs) =
      app options (genFunOf (pure g)) $
        go gs

app :: GeneratorOptions -> ExpQ -> ExpQ -> ExpQ
app options e1 e2 = do
  let op = if checked options then "<:" else "<+"
  infixE (Just e1) (varE (mkName op)) (Just e2)

genFunOf :: ExpQ -> ExpQ
genFunOf = appE (varE (mkName "genFun"))

funOf :: ExpQ -> ExpQ
funOf = appE (varE (mkName "fun"))
