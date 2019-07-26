{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Internal.InfoBuilder
  ( configInfo
  , titleInfo
  , descriptionInfo
  , tosInfo
  , contactInfo
  , licenseInfo
  , versionInfo
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Text           (Text)
import           Internal.Errors     (ContactErr, InfoErr (..), LicenseErr)
import           Internal.Types      (Contact, Info (..), License)
import           Lens.Micro          ((.~), (?~))
import           Lens.Micro.TH


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
convertI (InfoB  _ _ (Just "") _ _ _)       = Left InvalidToS
convertI (InfoB  _ _ _ (Just (Left e)) _ _) = Left . InvalidContact $ e
convertI (InfoB  _ _ _ _ (Just (Left e)) _) = Left . InvalidLicense $ e
convertI (InfoB  _ _ _ _ _ "")              = Left InvalidVersion
convertI (InfoB t d tos c l v)              =
  Right (Info t d tos (toC c) (toC l) v) where
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
