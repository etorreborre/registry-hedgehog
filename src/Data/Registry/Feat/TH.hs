{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Registry.Feat.TH where

import           Control.Monad.Fail              (fail)
import           Data.Registry.Internal.TH
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Protolude

-- | Make a registry containing enumerates for an ADT
--   We  want to generate the following

--       fun enumEmployeeStatus
--    <: enumFun (tag @"permanent" Permanent)
--    <: enumFun (tag @"temporary" Temporary)
-- enumEmployeeStatus :: Enumerate (Tag "permanent" EmployeeStatus) -> Enumerate (Tag "temporary" EmployeeStatus) -> Enumerate EmployeeStatus
-- enumEmployeeStatus g1 g2 = asum [fmap unTag g1, fmap unTag g2]

makeEnums :: Name -> ExpQ
makeEnums enumType = do
  info <- reify enumType
  case info of
    TyConI (DataD _context name _typeVars _kind constructors _deriving) -> do
      selector   <- makeSelectEnumerate name constructors
      enumerates <- traverse makeConstructorGenerator constructors
      assembleEnumeratesToRegistry selector enumerates

    other -> do
      qReport True ("can only create enumerates for an ADT, got: " <> show other)
      fail "enumerates creation failed"
