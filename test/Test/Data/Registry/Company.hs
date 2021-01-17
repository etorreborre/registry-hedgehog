module Test.Data.Registry.Company where

import Protolude

-- Some complex nested data
data Company = Company
  { companyName :: Text,
    departments :: [Department]
  }
  deriving (Eq, Show)

data Department = Department
  { departmentName :: Text,
    employees :: [Employee]
  }
  deriving (Eq, Show)

data Employee = Employee
  { employeeName :: Text,
    employeeStatus :: EmployeeStatus,
    salary :: Int,
    bonus :: Maybe Int
  }
  deriving (Eq, Show)

-- | Note that this is an ADT with several constructors
data EmployeeStatus
  = Permanent
  | Temporary Int -- number of days
  deriving (Eq, Show)
