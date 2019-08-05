{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Builders.InfoBuilder
  ( configInfo
  , titleInfo
  , descriptionInfo
  , tosInfo
  , contactInfo
  , licenseInfo
  , versionInfo
  , InfoBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Errors (ContactErr, InfoErr (..), LicenseErr)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import Types (Contact, Info (..), License)
import Utils (isValidURL, isValidVersionNumber, verifyMaybe)

type InfoBuilder = State InfoB ()

data InfoB = InfoB
  { _infoTitleB          :: Text
  , _infoDescriptionB    :: Maybe Text
  , _infoTermsOfServiceB :: Maybe Text
  , _infoContactB        :: Maybe (Either ContactErr Contact)
  , _infoLicenseB        :: Maybe (Either LicenseErr License)
  , _infoVersionB        :: Text
  } deriving (Eq, Show)

$(makeLenses ''InfoB)

configInfo :: InfoBuilder -> Either InfoErr Info
configInfo = convertI . flip execState emptyInfoB

convertI :: InfoB -> Either InfoErr Info
convertI (InfoB "" _ _ _ _ _)               = Left InvalidTitle
convertI (InfoB  _ (Just "") _ _ _ _)       = Left InvalidDescriptionI
convertI (InfoB  _ _ _ (Just (Left e)) _ _) = Left . InvalidContact $ e
convertI (InfoB  _ _ _ _ (Just (Left e)) _) = Left . InvalidLicense $ e
convertI (InfoB  _ _ _ _ _ "")              = Left InvalidVersion
convertI (InfoB t d tos c l v)              | not . verifyMaybe isValidURL $ tos =
  Left InvalidToS
                                            | otherwise =
  Right (Info t d tos (toC c) (toC l) v)

toC :: Maybe (Either b a) -> Maybe a
toc (Just (Left _)) = Nothing -- only to be total
toC = fmap (\(Right x) -> x)


titleInfo :: Text -> InfoBuilder
titleInfo t = modify $ infoTitleB .~ t

descriptionInfo :: Text -> InfoBuilder
descriptionInfo d = modify $ infoDescriptionB ?~ d

tosInfo :: Text -> InfoBuilder
tosInfo t = modify $ infoTermsOfServiceB ?~ t

contactInfo :: Either ContactErr Contact -> InfoBuilder
contactInfo c = modify $ infoContactB ?~ c

licenseInfo :: Either LicenseErr License -> InfoBuilder
licenseInfo l = modify $ infoLicenseB ?~ l

versionInfo :: Text -> InfoBuilder
versionInfo t = modify $ infoVersionB .~ t


emptyInfoB :: InfoB
emptyInfoB = InfoB "" Nothing Nothing Nothing Nothing ""
