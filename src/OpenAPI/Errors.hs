module OpenAPI.Errors where


data OpenAPIErr = InvalidInfo InfoErr
                | InvalidPath PathErr
                | NoPaths
                | RepPaths
                | InvalidServer ServerErr
                | InvalidSecurity SecReqErr
                deriving (Eq, Show)

data ServerVarErr = InvalidDefault
                  | InvalidEnum
                  | InvalidDescriptionSV
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
             | InvalidServerP ServerErr
             | NoOperations
             | RepResponses
             | InvalidReferenceP
             deriving (Eq, Show)

data OperationErr = InvalidTags
                  | InvalidSummaryO
                  | InvalidDescriptionO
                  | InvalidType
                  | NoResponses
                  | NoDefault
                  | InvalidResponses ResponsesErr
                  deriving (Eq, Show)

data ServerErr = InvalidURLS
               | InvalidDescriptionS
               | InvalidVarName
               | InvalidServerVar ServerVarErr
               deriving (Eq, Show)

data ResponseErr = InvalidDescriptionR
                 | InvalidHttpStatus
                 deriving (Eq, Show)

data SecReqErr = InvalidNameSecR
               | InvalidEmptyScope
               deriving (Eq, Show)

data ReferenceErr = InvalidEmptyReference
                  deriving (Eq, Show)

data ResponsesErr = InvalidResponse ResponseErr
                  | InvalidReferenceR ReferenceErr
                  deriving (Eq, Show)

data ParameterErr = InvalidNameParameter
                  | InvalidDescriptionParameter
                  | InvalidTypeParameter
                  deriving (Eq, Show)
