{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.ServerBuilder
  ( configServer
  , urlServer
  , varServer
  , descriptionServer
  , ServerBuilder
  ) where

import           Control.Monad.State (State, execState, modify)
import           Data.Either
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Lens.Micro ((%~), (.~), (?~))
import           Lens.Micro.TH
import           OpenAPI.Errors (ServerErr (..), ServerVarErr)
import           OpenAPI.Types (Server (..), ServerVar)
import           OpenAPI.Utils


type ServerBuilder = State ServerB ()

data ServerB = ServerB
  { _serverURLB         :: Text
  , _serverDescriptionB :: Maybe Text
  , _serverVarsB        :: HashMap Text (Either ServerVarErr ServerVar)
  }

$(makeLenses ''ServerB)

configServer :: ServerBuilder -> Either ServerErr Server
configServer = convertC . flip execState emptyServerB

convertC :: ServerB -> Either ServerErr Server
convertC (ServerB "" _ _) = Left InvalidURLS
convertC (ServerB u d sv) | not . noEmptyTxtMaybe $ d = Left InvalidDescriptionS
                          | not . noEmptyTxts . HM.keys $ sv = Left InvalidVarName
                          | otherwise = foldBuilder InvalidServerVar (Server u d) sv

urlServer :: Text -> ServerBuilder
urlServer url = modify $ serverURLB .~ url

descriptionServer :: Text -> ServerBuilder
descriptionServer d = modify $ serverDescriptionB ?~ d

varServer :: Text -> Either ServerVarErr ServerVar -> ServerBuilder
varServer k v = modify $ serverVarsB %~ HM.insert k v

emptyServerB :: ServerB
emptyServerB = ServerB "" Nothing HM.empty
