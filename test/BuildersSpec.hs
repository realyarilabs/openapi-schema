{-# LANGUAGE OverloadedStrings #-}
module BuildersSpec where

import Builders
import Data.Aeson.Text
import Data.Either
import Data.Text.Lazy (pack)
import Errors
import Test.Hspec
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Builders" $ do
    context "OpenAPI" $
      it "needs a valid Info object and Path" $
        isRight validOpenAPI `shouldBe` True

    context "Info" $ do
      it "requires a non-empty title" $
        invalidInfoTitle `shouldBe` Left InvalidTitle
      it "requires a non-empty version" $
        invalidInfoVersion `shouldBe` Left InvalidVersion
      it "requires at least a title and a version" $
        isRight validInfoOnlyRequired `shouldBe` True
      it "requires a non-empty description, when present" $
        invalidInfoDesc `shouldBe` Left InvalidDescriptionI
      it "requires a non-empty ToS, when present" $
        invalidInfoToSEmpty `shouldBe` Left InvalidToS
      it "requires an URL format ToS, when present" $
        invalidInfoToS `shouldBe` Left InvalidToS
      it "requires a valid license, when present" $
        invalidInfoLicense `shouldBe` Left (InvalidLicense InvalidNameL)
      it "requires a valid contact, when present" $
        invalidInfoContact `shouldBe` Left (InvalidContact InvalidURLC)

    context "License" $ do
      it "requires a non-empty name" $
        invalidLicenseName `shouldBe` Left InvalidNameL
      it "requires at least a non-empty name" $
        isRight validLicenseOnlyRequired `shouldBe` True
      it "requires a non-empty URL, when present" $
        invalidLicenseURLEmpty `shouldBe` Left InvalidURLL
      it "requires an URL format URL, when present" $
        invalidLicenseURL `shouldBe` Left InvalidURLL

    context "Contact" $ do
      it "requires a non-empty name, when present" $
        invalidContactName `shouldBe` Left InvalidNameC
      it "requires a non-empty URL, when present" $
        invalidContactURLEmpty `shouldBe` Left InvalidURLC
      it "requires an URL format URL, when present" $
        invalidContactURL `shouldBe` Left InvalidURLC
      it "requires a non-empty email, when present" $
        invalidContactEmailEmpty `shouldBe` Left InvalidEmailC
      it "requires an email format email, when present" $
        invalidContactEmail `shouldBe` Left InvalidEmailC
      it "when present, a name is a valid Contact" $
        isRight validContactName `shouldBe` True
      it "when present, an email is a valid Contact" $
        isRight validContactEmail `shouldBe` True
      it "when present, an URL is a valid Contact" $
        isRight validContactURL `shouldBe` True





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
