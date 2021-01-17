module Test.Tutorial.DataModel where

import Protolude

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

data EmployeeStatus
  = Permanent
  | Temporary Int -- number of days
  deriving (Eq, Show)
