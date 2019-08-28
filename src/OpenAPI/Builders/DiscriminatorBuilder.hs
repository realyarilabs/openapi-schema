{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.DiscriminatorBuilder
  ( configDiscriminator
  , nameDiscriminator
  , mappingDiscriminator
  , DiscriminatorBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Lens.Micro ((%~), (.~))
import           Lens.Micro.TH
import           OpenAPI.Errors (DiscriminatorErr (..))
import           OpenAPI.Types (Discriminator (..))
import           OpenAPI.Utils


type DiscriminatorBuilder = State DiscriminatorB ()

data DiscriminatorB = DiscriminatorB
  { _discriminatorPropertyNameB :: Text
  , _discriminatorMappingB      :: HashMap Text Text
  } deriving (Eq, Show)

$(makeLenses ''DiscriminatorB)

configDiscriminator :: DiscriminatorBuilder -> Either DiscriminatorErr Discriminator
configDiscriminator = convertC . flip execState emptyDiscriminatorB

convertC :: DiscriminatorB -> Either DiscriminatorErr Discriminator
convertC (DiscriminatorB n m) | emptyTxt n = Left InvalidNameDiscriminator
                              | HM.foldrWithKey (\k v a -> emptyTxt k || emptyTxt v || a) False m = Left InvalidMappingDiscriminator
                              | otherwise = pure $ Discriminator n m


nameDiscriminator :: Text -> DiscriminatorBuilder
nameDiscriminator s = modify $ discriminatorPropertyNameB .~ s

mappingDiscriminator :: Text -> Text -> DiscriminatorBuilder
mappingDiscriminator k v = modify $ discriminatorMappingB %~ HM.insert k v

emptyDiscriminatorB :: DiscriminatorB
emptyDiscriminatorB = DiscriminatorB "" HM.empty
