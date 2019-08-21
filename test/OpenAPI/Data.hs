{-# LANGUAGE OverloadedStrings #-}
module OpenAPI.Data where

import OpenAPI.Builders
import OpenAPI.Errors
import OpenAPI.Types

-- OpenAPI
validOpenAPI :: Either OpenAPIErr OpenAPI
validOpenAPI = config $ do
  infoOpenAPI validInfo
  pathOpenAPI validPathFoo

jvalidOpenAPI :: Either OpenAPIErr OpenAPI
jvalidOpenAPI = config $ do
  infoOpenAPI validInfo
  pathOpenAPI validPathFoo
  serverOpenAPI validServer
  serverOpenAPI validServer2
  securityOpenAPI validSecReq

invalidOpenAPIRepPath :: Either OpenAPIErr OpenAPI
invalidOpenAPIRepPath = config $ do
  infoOpenAPI validInfo
  pathOpenAPI validPathFoo
  pathOpenAPI validPathFoo2

invalidOpenAPIScope :: Either OpenAPIErr OpenAPI
invalidOpenAPIScope = config $ do
  infoOpenAPI validInfo
  pathOpenAPI validPathFoo
  securityOpenAPI invalidSecReqScope

-- Path
validPathFoo2 :: Either PathErr Path
validPathFoo2 = configPath $ do
  namePath "/foo"
  summaryPath "URI for the foo resource"
  descriptionPath "Foo resource"
  operationPath validOperationGet
  operationPath validOperationPost

validPathFoo :: Either PathErr Path
validPathFoo = configPath $ do
  namePath "/foo"
  summaryPath "URI for the foo resource"
  descriptionPath "Foo resource"
  operationPath validOperationGet
  operationPath validOperationPost

invalidPathFooRep :: Either PathErr Path
invalidPathFooRep = configPath $ do
  namePath "/foo"
  summaryPath "URI for the foo resource"
  descriptionPath "Foo resource"
  operationPath validOperationGet
  operationPath validOperationPost
  operationPath validOperationPost

invalidPathFooNameEmpty :: Either PathErr Path
invalidPathFooNameEmpty = configPath $ do
  namePath ""
  operationPath validOperationGet

invalidPathFooName :: Either PathErr Path
invalidPathFooName = configPath $ do
  namePath "foo"
  operationPath validOperationGet

invalidPathFooOps :: Either PathErr Path
invalidPathFooOps = configPath $
  namePath "/foo"

invalidPathFooSummary :: Either PathErr Path
invalidPathFooSummary = configPath $ do
  namePath "/foo"
  summaryPath ""
  operationPath validOperationGet

invalidPathFooDescription :: Either PathErr Path
invalidPathFooDescription = configPath $ do
  namePath "/foo"
  descriptionPath ""
  operationPath validOperationGet

-- Operation
validOperationGet :: Either OperationErr Operation
validOperationGet = configOperation $ do
  typeOperation GET
  descriptionOperation "GET all `foo`s"
  defaultResponseOperation validResponseOk
  statusResponseOperation "404" validResponseNotFound

validOperationPost :: Either OperationErr Operation
validOperationPost = configOperation $ do
  typeOperation POST
  descriptionOperation "POST a `foo`"
  defaultResponseOperation validResponseOk

invalidOperationPostSummary :: Either OperationErr Operation
invalidOperationPostSummary = configOperation $ do
  typeOperation POST
  defaultResponseOperation validResponseOk
  summaryOperation ""

invalidOperationPostDescription :: Either OperationErr Operation
invalidOperationPostDescription = configOperation $ do
  typeOperation POST
  descriptionOperation ""
  defaultResponseOperation validResponseOk

invalidOperationPostNoReps :: Either OperationErr Operation
invalidOperationPostNoReps = configOperation $
  typeOperation POST

invalidOperationPostTags :: Either OperationErr Operation
invalidOperationPostTags = configOperation $ do
  typeOperation POST
  descriptionOperation "POST a `foo`"
  defaultResponseOperation validResponseOk
  tagOperation "         "

invalidOperationPostDefault :: Either OperationErr Operation
invalidOperationPostDefault = configOperation $ do
  typeOperation POST
  defaultResponseOperation validResponseOk

--  Response
validResponseOk :: Either ResponseErr Response
validResponseOk = configResponse $
  descriptionResponse "ok"

validResponseNotFound :: Either ResponseErr Response
validResponseNotFound  = configResponse $
  descriptionResponse "not found"

invalidResponse :: Either ResponseErr Response
invalidResponse  = configResponse $
  descriptionResponse ""

-- License
validLicense :: Either LicenseErr License
validLicense = configLicense $ do
  nameLicense "TEST"
  urlLicense "https://foo"

validLicenseOnlyRequired :: Either LicenseErr License
validLicenseOnlyRequired = configLicense $
  nameLicense "TEST"

invalidLicenseName :: Either LicenseErr License
invalidLicenseName = configLicense $
  nameLicense ""

invalidLicenseURLEmpty :: Either LicenseErr License
invalidLicenseURLEmpty = configLicense $ do
  nameLicense "TEST"
  urlLicense ""

invalidLicenseURL :: Either LicenseErr License
invalidLicenseURL = configLicense $ do
  nameLicense "TEST"
  urlLicense "not an URL format"


-- Contact
validContact :: Either ContactErr Contact
validContact = configContact $ do
  nameContact "Test McFoo"
  urlContact "http://foo"
  emailContact "foo@test"

validContactName :: Either ContactErr Contact
validContactName = configContact $
  nameContact "Test McFoo"

validContactURL :: Either ContactErr Contact
validContactURL = configContact $
  urlContact "http://foo"

validContactEmail :: Either ContactErr Contact
validContactEmail = configContact $
  emailContact "foo@test"

invalidContactName :: Either ContactErr Contact
invalidContactName = configContact $
  nameContact ""

invalidContactURLEmpty :: Either ContactErr Contact
invalidContactURLEmpty = configContact $
  urlContact ""

invalidContactURL :: Either ContactErr Contact
invalidContactURL = configContact $
  urlContact "not an URL format"

invalidContactEmailEmpty :: Either ContactErr Contact
invalidContactEmailEmpty = configContact $
  emailContact ""

invalidContactEmail :: Either ContactErr Contact
invalidContactEmail = configContact $
  emailContact "not a single `at`"


-- Info
validInfo :: Either InfoErr Info
validInfo = configInfo $ do
  titleInfo "Foo's API"
  descriptionInfo "An API for Foo"
  tosInfo "https://foo"
  contactInfo validContact
  licenseInfo validLicense
  versionInfo "v2"

invalidInfoTitle :: Either InfoErr Info
invalidInfoTitle = configInfo $ do
  titleInfo ""
  versionInfo "v2"

invalidInfoVersion :: Either InfoErr Info
invalidInfoVersion = configInfo $ do
  titleInfo "Foo's API"
  versionInfo ""

validInfoOnlyRequired :: Either InfoErr Info
validInfoOnlyRequired = configInfo $ do
  titleInfo "Foo's API"
  versionInfo "v2"

invalidInfoDesc :: Either InfoErr Info
invalidInfoDesc = configInfo $ do
  titleInfo "Foo's API"
  descriptionInfo ""
  versionInfo "v2"

invalidInfoToSEmpty :: Either InfoErr Info
invalidInfoToSEmpty = configInfo $ do
  titleInfo "Foo's API"
  tosInfo ""
  versionInfo "v2"

invalidInfoToS :: Either InfoErr Info
invalidInfoToS = configInfo $ do
  titleInfo "Foo's API"
  tosInfo "not a valid url"
  versionInfo "v2"

invalidInfoLicense :: Either InfoErr Info
invalidInfoLicense = configInfo $ do
  titleInfo "Foo's API"
  licenseInfo invalidLicenseName
  versionInfo "v2"

invalidInfoContact :: Either InfoErr Info
invalidInfoContact = configInfo $ do
  titleInfo "Foo's API"
  contactInfo invalidContactURLEmpty
  versionInfo "v2"

-- ServerVar
validServerVar :: Either ServerVarErr ServerVar
validServerVar = configServerVar $
  defaultServerV "yay"

validServerVar2 :: Either ServerVarErr ServerVar
validServerVar2 = configServerVar $ do
  defaultServerV "yeah"
  enumServerV "earth"
  enumServerV "heaven"
  descriptionServerV "Darwin antigame"

invalidServerVarEnum :: Either ServerVarErr ServerVar
invalidServerVarEnum = configServerVar $ do
  defaultServerV "yay"
  enumServerV "  "

invalidServerVarDescription :: Either ServerVarErr ServerVar
invalidServerVarDescription = configServerVar $ do
  defaultServerV "yay"
  descriptionServerV "  "

-- Server
validServer :: Either ServerErr Server
validServer = configServer $
  urlServer "bar"

validServer2 :: Either ServerErr Server
validServer2 = configServer $ do
  urlServer "fo"
  descriptionServer "Fo, almost Foo"
  varServer  "test1" validServerVar
  varServer  "test2" validServerVar2

invalidServerDescription :: Either ServerErr Server
invalidServerDescription = configServer $ do
  urlServer "bar"
  descriptionServer "  "


invalidServerVar :: Either ServerErr Server
invalidServerVar = configServer $ do
  urlServer "bar"
  varServer "foo" invalidServerVarDescription

invalidServerVarEmpty :: Either ServerErr Server
invalidServerVarEmpty = configServer $ do
  urlServer "bar"
  varServer " " validServerVar


-- SecReq
validSecReq :: Either SecReqErr SecReq
validSecReq = configSecReq $
  nameSecReq "test"

invalidSecReqEmptyName :: Either SecReqErr SecReq
invalidSecReqEmptyName = configSecReq $
  nameSecReq "     "

invalidSecReqScope :: Either SecReqErr SecReq
invalidSecReqScope = configSecReq $ do
  nameSecReq "test"
  scopeSeqReq "   "
