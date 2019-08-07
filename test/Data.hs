{-# LANGUAGE OverloadedStrings #-}
module Data where

import Builders
import Errors
import Types

-- OpenAPI
validOpenAPI :: Either OpenAPIErr OpenAPI
validOpenAPI = config $ do
  infoOpenAPI validInfo
  pathOpenAPI validPathFoo

-- Path foo
validPathFoo :: Either PathErr Path
validPathFoo = configPath $ do
  namePath "/foo"
  summaryPath "URI for the foo resource"
  descriptionPath "Foo resource"
  operationPath validOperationGet
  operationPath validOperationPost

validOperationGet :: Either OperationErr Operation
validOperationGet = configOperation $ do
  typeOperation GET
  descriptionOperation "GET all `foo`s"
  defaultRes $
    descriptionResponse "ok"
  statusRes "404" $
    descriptionResponse "not found"

validOperationPost :: Either OperationErr Operation
validOperationPost = configOperation $ do
  typeOperation POST
  descriptionOperation "POST a `foo`"
  defaultRes $
    descriptionResponse "ok"

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
