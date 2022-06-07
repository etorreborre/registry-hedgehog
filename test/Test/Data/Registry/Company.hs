module Test.Data.Registry.Company where

import Protolude

-- Some complex nested data
data Company = Company
  { companyName :: Text,
    departments :: [Department]
  }
  deriving (Eq, Show)

data Department = Department
  { departmentName :: DepartmentName,
    employees :: [Employee]
  }
  deriving (Eq, Show)

newtype DepartmentName = DepartmentName { _departmentName :: Text }
  deriving (Eq, Show)

data Employee = Employee
  { employeeName :: EmployeeName,
    employeeStatus :: EmployeeStatus,
    salary :: Int,
    bonus :: Maybe Int
  }
  deriving (Eq, Show)

newtype EmployeeName = EmployeeName Text
  deriving (Eq, Show)

-- | Note that this is an ADT with several constructors
data EmployeeStatus
  = Permanent
  | Temporary Int -- number of days
  deriving (Eq, Show)
