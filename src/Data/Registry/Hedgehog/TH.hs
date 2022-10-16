{-# LANGUAGE DataKinds #-}

module Data.Registry.Hedgehog.TH where

import Control.Monad.Fail (fail)
import Data.Registry
import Data.Registry.Internal.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Protolude

-- | Make a registry containing generators for an ADT
--   We  want to generate the following
--
--       fun genEmployeeStatus
--    <: genFun (tag @"permanent" Permanent)
--    <: genFun (tag @"temporary" Temporary)
--
-- genEmployeeStatus :: Gen Chooser -> Gen (Tag "permanent" EmployeeStatus) -> Gen (Tag "temporary" EmployeeStatus) -> Gen EmployeeStatus
-- genEmployeeStatus chooser g1 g2 = chooseOne chooser [fmap unTag1, fmap unTag g2]
makeGenerators :: Name -> ExpQ
makeGenerators genType = do
  info <- reify genType
  case info of
    TyConI (NewtypeD _context _name _typeVars _kind constructor _deriving) -> do
      constructorType <- nameOf constructor
      genFunOf (conE constructorType)
    TyConI (DataD _context _name _typeVars _kind [constructor] _deriving) -> do
      constructorType <- nameOf constructor
      genFunOf (conE constructorType)
    TyConI (DataD _context name _typeVars _kind constructors _deriving) -> do
      selector <- makeSelectGenerator name constructors
      generators <- traverse makeConstructorGenerator constructors
      assembleGeneratorsToRegistry selector generators
    other -> do
      qReport True ("can not create generators for this kind of data type at the moment. Got: " <> show other)
      fail "generators creation failed"

emptyRegistry :: Registry '[] '[]
emptyRegistry = mempty
