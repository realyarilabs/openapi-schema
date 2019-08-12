{-# LANGUAGE OverloadedStrings #-}
module OpenAPI.BuildersSpec where

import Data.Either
import OpenAPI.Data
import OpenAPI.Errors
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Builders" $ do
    context "OpenAPI" $ do
      it "needs a valid Info object and Path" $
        isRight validOpenAPI `shouldBe` True
      it "can't have paths with the same name" $
        invalidOpenAPIRepPath `shouldBe` Left RepPaths
      it "can't have an invalid security config" $
        invalidOpenAPIScope `shouldBe` Left (InvalidSecurity InvalidEmptyScope)

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

    context "Path" $ do
      it "can't have operations with repetitive types" $
        invalidPathFooRep `shouldBe` Left RepResponses
      it "requires a non-empty name" $
        invalidPathFooNameEmpty `shouldBe` Left InvalidNameP
      it "requires a name starting with /" $
        invalidPathFooName `shouldBe` Left InvalidNameP
      it "requires at least one Operation" $
        invalidPathFooOps `shouldBe` Left NoOperations
      it "requires a non-empty summary, when present" $
        invalidPathFooSummary `shouldBe` Left InvalidSummaryP
      it "requires a non-empty description, when present" $
        invalidPathFooDescription `shouldBe` Left InvalidDescriptionP

    context "Response" $
      it "requires a non-empty description" $
        invalidResponse `shouldBe` Left InvalidDescriptionR

    context "Operation" $ do
      it "requires a non-empty summary, when present" $
        invalidOperationPostSummary `shouldBe` Left InvalidSummaryO
      it "requires a non-empty description, when present" $
        invalidOperationPostDescription `shouldBe` Left InvalidDescriptionO
      it "requires at least one valid Response" $
        invalidOperationPostNoReps `shouldBe` Left NoResponses
      it "requires that only one Default Response is present" $
        invalidOperationPostDefault `shouldBe` Left MoreThanOneDefault
      it "when present, all tags must be non-empty" $
        invalidOperationPostTags `shouldBe` Left InvalidTags

    context "Server Var" $ do
      it "requires a non-empty default" $
        isRight validServerVar `shouldBe` True
      it "requires non-empty enums, when present" $
        invalidServerVarEnum `shouldBe` Left InvalidEnum
      it "requires a non-empty description, when present" $
        invalidServerVarDescription `shouldBe` Left InvalidDescriptionSV

    context "Server" $ do
      it "requires an URL" $
        isRight validServer `shouldBe` True
      it "requires a non-empty description, when present" $
        invalidServerDescription `shouldBe` Left InvalidDescriptionS
      it "requires valid server vars, when present" $
        invalidServerVar `shouldBe` Left (InvalidServerVar InvalidDescriptionSV)
      it "requires server vars with non-empty names, when present" $
        invalidServerVarEmpty `shouldBe` Left InvalidVarName

    context "SecReq" $ do
      it "requires a name" $
        isRight validSecReq `shouldBe` True
      it "requires a non-empty name" $
        invalidSecReqEmptyName `shouldBe` Left InvalidNameSecR
      it "requires non-empty scopes, when present" $
        invalidSecReqScope `shouldBe` Left InvalidEmptyScope

