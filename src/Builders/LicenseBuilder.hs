{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Builders.LicenseBuilder
  ( configLicense
  , nameLicense
  , urlLicense
  , LicenseBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Errors (LicenseErr (..))
import Lens.Micro ((.~), (?~))
import Lens.Micro.TH
import Types (License (..))
import Utils (isValidURL, verifyMaybe)

type LicenseBuilder = State LicenseB ()

data LicenseB = LicenseB
  { _licenseNameB :: Text
  , _licenseUrlB  :: Maybe Text
  } deriving (Eq, Show)

$(makeLenses ''LicenseB)

configLicense :: LicenseBuilder -> Either LicenseErr License
configLicense = convertL . flip execState emptyLicenseB

convertL :: LicenseB -> Either LicenseErr License
convertL (LicenseB "" _)        = Left InvalidNameL
convertL (LicenseB n u)         | verifyMaybe isValidURL u = Right $ License n u
                                | otherwise = Left InvalidURLL


nameLicense :: Text -> LicenseBuilder
nameLicense c = modify $ licenseNameB .~ c

urlLicense :: Text -> LicenseBuilder
urlLicense url = modify $ licenseUrlB ?~ url


emptyLicenseB :: LicenseB
emptyLicenseB = LicenseB "" Nothing
