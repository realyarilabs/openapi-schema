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
             | InvalidParameterP
             | InvalidSummaryP
             | InvalidDescriptionP
             | InvalidRequestO
             | InvalidOperation OperationErr
             | InvalidServerP ServerErr
             | NoOperations
             | RepResponses
             | InvalidReferenceP
             deriving (Eq, Show)

data OperationErr = InvalidTags
                  | InvalidSummaryO
                  | InvalidDocsO ExternalDocsErr
                  | InvalidIdO
                  | InvalidParameterO
                  | InvalidDescriptionO
                  | InvalidType
                  | NoResponses
                  | InvalidRequestBodyO RequestBodyErr
                  | InvalidRequestBodyRefO ReferenceErr
                  | NoDefault
                  | InvalidResponses ResponsesErr
                  | InvalidServerO ServerErr
                  | InvalidSecurityO SecReqErr
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
                  | InvalidSchemaParameter SchemaErr
                  | InvalidSchemaRefParameter ReferenceErr
                  | InvalidTypeParameter
                  deriving (Eq, Show)

data DiscriminatorErr = InvalidNameDiscriminator
                      | InvalidMappingDiscriminator
                      deriving (Eq, Show)

data XmlErr = InvalidNameXml
            | InvalidNameSpaceXml
            | InvalidPrefixXml
            deriving (Eq, Show)

data ExternalDocsErr = InvalidDescriptionDocs
                     | InvalidURLDocs
                     deriving (Eq, Show)

data SchemaErr = DiscriminatorError DiscriminatorErr
               | XmlError XmlErr
               | DocsError ExternalDocsErr
               | WriteAndReadOnly
               | InvalidExampleSchema
               deriving (Eq, Show)

data MediaTypeErr = InvalidExampleMediaType
                  deriving (Eq, Show)

data RequestBodyErr = InvalidDescriptionRequestBody
                    | InvalidMediaR MediaTypeErr
                    deriving (Eq, Show)
