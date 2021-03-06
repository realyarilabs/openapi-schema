{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.ServerVarBuilder
  ( configServerVar
  , enumServerV
  , defaultServerV
  , descriptionServerV
  , ServerVarBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((%~), (.~), (?~))
import Lens.Micro.TH
import OpenAPI.Errors (ServerVarErr (..))
import OpenAPI.Types (ServerVar (..))
import OpenAPI.Utils


type ServerVarBuilder = State ServerVarB ()

data ServerVarB = ServerVarB
  { _serverVEnumB        :: [Text]
  , _serverVDefaultB     :: Text
  , _serverVDescriptionB :: Maybe Text
  } deriving (Eq, Show)

$(makeLenses ''ServerVarB)

configServerVar :: ServerVarBuilder -> Either ServerVarErr ServerVar
configServerVar = convertC . flip execState emptyServerVarB

convertC :: ServerVarB -> Either ServerVarErr ServerVar
convertC (ServerVarB e dt d) | emptyTxtMaybe d = Left InvalidDescriptionSV
                             | emptyTxt dt = Left InvalidDefault
                             | emptyTxts e = Left InvalidEnum
                             | otherwise = pure $ ServerVar e dt d


defaultServerV :: Text -> ServerVarBuilder
defaultServerV d = modify $ serverVDefaultB .~ d

enumServerV :: Text -> ServerVarBuilder
enumServerV e = modify $ serverVEnumB %~ (e:)

descriptionServerV :: Text -> ServerVarBuilder
descriptionServerV d = modify $ serverVDescriptionB ?~ d


emptyServerVarB :: ServerVarB
emptyServerVarB = ServerVarB [] "" Nothing
