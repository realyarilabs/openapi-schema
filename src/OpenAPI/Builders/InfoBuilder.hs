{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.InfoBuilder
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
import Data.Text (Text)
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors (ContactErr, InfoErr (..), LicenseErr)
import OpenAPI.Types (Contact, Info (..), License)
import OpenAPI.Utils

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
convertI (InfoB  _ _ _ (Just (Left e)) _ _) = Left . InvalidContact $ e
convertI (InfoB  _ _ _ _ (Just (Left e)) _) = Left . InvalidLicense $ e
convertI (InfoB t d tos c l v) | not . verifyMaybe isValidURL $ tos = Left InvalidToS
                               | emptyTxt t = Left InvalidTitle
                               | emptyTxtMaybe d = Left InvalidDescriptionI
                               | emptyTxt v = Left InvalidVersion
                               | otherwise = pure $ Info t d tos (maybeRight c) (maybeRight l) v


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
