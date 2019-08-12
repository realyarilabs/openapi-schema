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
  { _serverVEnumB        :: Maybe [Text]
  , _serverVDefaultB     :: Text
  , _serverVDescriptionB :: Maybe Text
  } deriving (Eq, Show)

$(makeLenses ''ServerVarB)

configServerVar :: ServerVarBuilder -> Either ServerVarErr ServerVar
configServerVar = convertC . flip execState emptyServerVarB

convertC :: ServerVarB -> Either ServerVarErr ServerVar
convertC (ServerVarB (Just []) _ _) = Left InvalidEnum
convertC (ServerVarB _ "" _)        = Left InvalidDefault
convertC (ServerVarB e dt d)        | not . noEmptyTxtMaybe $ d = Left InvalidDescriptionSV
                                    | not . noEmptyTxtsMaybe $ e = Left InvalidEnum
                                    | otherwise = pure $ ServerVar e dt d


defaultServerV :: Text -> ServerVarBuilder
defaultServerV d = modify $ serverVDefaultB .~ d

enumServerV :: Text -> ServerVarBuilder
enumServerV e = modify $ serverVEnumB %~ pure . maybe [] (e:)

descriptionServerV :: Text -> ServerVarBuilder
descriptionServerV d = modify $ serverVDescriptionB ?~ d


emptyServerVarB :: ServerVarB
emptyServerVarB = ServerVarB Nothing "" Nothing
