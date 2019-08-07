{-# LANGUAGE OverloadedStrings #-}
module BuildersSpec where

import Data
import Data.Either
import Errors
import Test.Hspec


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
