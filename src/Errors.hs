module Errors where


data OpenAPIErr = InvalidInfo InfoErr
                | InvalidPath PathErr
                | NoPaths
                | RepPaths
                deriving (Eq, Show)

data InfoErr = InvalidTitle
             | InvalidDescriptionI
             | InvalidToS
             | InvalidContact ContactErr
             | InvalidLicense LicenseErr
             | InvalidVersion
             | NoInfo
             deriving (Eq, Show)

data ContactErr = InvalidNameC
                | InvalidURLC
                | InvalidEmailC
                deriving (Eq, Show)

data LicenseErr = InvalidNameL
                | InvalidURLL
                deriving (Eq, Show)


data PathErr = InvalidNameP
             | InvalidSummaryP
             | InvalidDescriptionP
             | InvalidOperation OperationErr
             | NoOperations
             | RepResponses
             deriving (Eq, Show)

data OperationErr = InvalidTags
                  | InvalidSummaryO
                  | InvalidDescriptionO
                  | InvalidType
                  | NoResponses
                  | InvalidResponse ResponseErr
                  deriving (Eq, Show)

data ResponseErr = InvalidDescriptionR
                 | InvalidHttpStatus
                 deriving (Eq, Show)
