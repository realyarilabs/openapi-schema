{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.ContactBuilder
  ( configContact
  , nameContact
  , urlContact
  , emailContact
  , ContactBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((?~))
import Lens.Micro.TH
import OpenAPI.Errors (ContactErr (..))
import OpenAPI.Types (Contact (..))
import OpenAPI.Utils


type ContactBuilder = State ContactB ()

data ContactB = ContactB
  { _contactNameB  :: Maybe Text
  , _contactUrlB   :: Maybe Text
  , _contactEmailB :: Maybe Text
  } deriving (Eq, Show)

$(makeLenses ''ContactB)

configContact :: ContactBuilder -> Either ContactErr Contact
configContact = convertC . flip execState emptyContactB

convertC :: ContactB -> Either ContactErr Contact
convertC (ContactB n u e) | emptyTxtMaybe n = Left InvalidNameC
                          | not . verifyMaybe isValidEmail $ e = Left InvalidEmailC
                          | not . verifyMaybe isValidURL $ u = Left InvalidURLC
                          | otherwise = pure $ Contact n u e

nameContact :: Text -> ContactBuilder
nameContact c = modify $ contactNameB ?~ c

urlContact :: Text -> ContactBuilder
urlContact url = modify $ contactUrlB ?~ url

emailContact :: Text -> ContactBuilder
emailContact e = modify $ contactEmailB ?~ e


emptyContactB :: ContactB
emptyContactB = ContactB Nothing Nothing Nothing
