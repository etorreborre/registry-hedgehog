module Test.Data.Registry.EnumerateData where

import           Protolude hiding (C1, D1)

-- * Various data types to test enumerates

data T =
   A Fuzzy
 | B Bool
 | C C1
 | D D1
 deriving (Eq, Show)

data Bug =
   Bug1
 | Bug2 Bool
 | Bug3 Bool Bool
 deriving (Eq, Show)

data Fuzzy =
   Yes
 | No
 | What
 | WhoKnows
 deriving (Eq, Show)

data A1 =
   A1 Bool
 | A2 Bool
 deriving (Eq, Show)

data B1 =
   B1 Bool
 | B2 Bool
 deriving (Eq, Show)

data C1 =
   C1 Bool
 | C2 Bool
 deriving (Eq, Show)

data D1 =
   D1 Bool
 | D2 Bool
 deriving (Eq, Show)
