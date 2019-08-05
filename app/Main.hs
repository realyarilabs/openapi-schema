{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Text

import Builders
import Errors
import Types

import Data.Text.Lazy.IO as I

main :: IO ()
main =  either print (I.writeFile "config.json" . encodeToLazyText) spec

spec :: Either OpenAPIErr OpenAPI
spec = config $ do
    -- info
    info $ do
        titleInfo "yay"
        descriptionInfo "asdfsgd"
        tosInfo "http://WTFYW"
        license $
            nameLicense "BSD"
        versionInfo "3.2.1"
        contact $ do
            nameContact "Eduardo"
            emailContact "test@herulu.me"

    path $ do
        namePath "/coisas"
        summaryPath "Pois"
        descriptionPath "faz coisas"
        operation $ do
            typeOperation POST
            descriptionOperation "get coisas"
            defaultRes $
                descriptionResponse "all ok"
            statusRes "404" $
                descriptionResponse "not found"
