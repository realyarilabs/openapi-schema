{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Internal.ContactBuilder
  ( configContact
  , nameContact
  , urlContact
  , emailContact
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Text           (Text)
import           Internal.Errors     (ContactErr (..))
import           Internal.Types      (Contact (..))
import           Lens.Micro          ((?~))
import           Lens.Micro.TH


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
convertC (ContactB (Just "") _ _) = Left InvalidNameC
convertC (ContactB _ (Just "") _) = Left InvalidURLC
convertC (ContactB _ _ (Just "")) = Left InvalidEmailC
convertC (ContactB n u e)         = Right $ Contact n u e


nameContact :: Text -> ContactBuilder
nameContact c = modify $ contactNameB ?~ c

urlContact :: Text -> ContactBuilder
urlContact url = modify $ contactUrlB ?~ url

emailContact :: Text -> ContactBuilder
emailContact e = modify $ contactEmailB ?~ e


emptyContactB :: ContactB
emptyContactB = ContactB Nothing Nothing Nothing
