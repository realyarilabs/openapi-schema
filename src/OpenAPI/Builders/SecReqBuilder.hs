{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module OpenAPI.Builders.SecReqBuilder
  ( configSecReq
  , nameSecReq
  , scopeSeqReq
  , SecReqBuilder
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Text (Text)
import Lens.Micro ((%~), (.~))
import Lens.Micro.TH
import OpenAPI.Errors (SecReqErr (..))
import OpenAPI.Types (SecReq (..))
import OpenAPI.Utils


type SecReqBuilder = State SecReqB ()

data SecReqB = SecReqB
  { _secReqNameB  :: Text
  , _secReqScopeB :: [Text]
  } deriving (Eq, Show)

$(makeLenses ''SecReqB)

configSecReq :: SecReqBuilder -> Either SecReqErr SecReq
configSecReq = convertC . flip execState emptySecReqB

convertC :: SecReqB -> Either SecReqErr SecReq
convertC (SecReqB n ss) | emptyTxt n = Left InvalidNameSecR
                        | emptyTxts ss = Left InvalidEmptyScope
                        | otherwise = pure $ SecReq n ss

nameSecReq :: Text -> SecReqBuilder
nameSecReq n = modify $ secReqNameB .~ n

scopeSeqReq :: Text -> SecReqBuilder
scopeSeqReq s = modify $ secReqScopeB %~ (s:)

emptySecReqB :: SecReqB
emptySecReqB = SecReqB "" []
