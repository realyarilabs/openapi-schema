{-# LANGUAGE OverloadedStrings #-}

module Utils where


import           Control.Arrow ((&&&))
import           Control.Monad
import           Data.Aeson (KeyValue, ToJSON, (.=))
import           Data.Bifunctor (bimap)
import           Data.Either (either)
import           Data.List (zipWith)
import           Data.Maybe (isNothing, maybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Traversable (sequence)

{-
   Some records have optional fields, this are represented by a `Maybe a`. When
   generating the JSON representation we do not want to have fields with the
   `Nothing` value, since this is transpiled to a `null`.
   This function removes all the `Nothing`s and generates the respective
   `Pair`s (Name, Value) to be converted JSON.
-}
pairMaybes :: (ToJSON a, KeyValue b) => [Maybe a] -> [Text] -> [b]
pairMaybes a b = join $ zipWith (\x y -> if isNothing x then [] else [y .= x]) a b

{-
    Some records fields depend on a list of other records that may have failed
    building. On such cases we want to propagate the error.
-}
foldBuilder :: (b -> c) -> ([a] -> d) -> [Either b a] -> Either c d
foldBuilder err build = bimap err build . sequence

{-
  Verify email addresses.
  Only checks if it contains an `@`.
  Read this https://davidcel.is/posts/stop-validating-email-addresses-with-regex/
-}
isValidEmail :: Text -> Bool
isValidEmail email = let (preAt, posAt) = T.breakOn "@" email
                         notNull = preAt /= "" &&  posAt /= ""
                     in notNull && checkAtE '@' posAt where
                        checkAtE c = (/= c) . T.last

isValidURL :: Text -> Bool
isValidURL url = let (protocol, _) = T.breakOn "://" url
                 in protocol == "http" || protocol == "https"

verifyMaybe :: (a -> Bool) -> Maybe a -> Bool
verifyMaybe = maybe True

{-
  Verify version number.
  Must follow the X.X.X format, where X is an Integer.
  `rightFormat` verifies that the input follows the expected format and the
  values can be converted to an Integer.
  `rightDivision` covers the following edge cases: `.X.X.X` `X.X.X.` `.X.X.X.`
-}
isValidVersionNumber :: Text -> Bool
isValidVersionNumber = uncurry (&&) . (rightFormat &&& rightDivision . nullText)  where
  rightFormat = either (const False) ((==3) . length) . mapM decimal . T.splitOn "."
  rightDivision = either (const False) (uncurry (&&) . ((/= '.') . T.head &&& (/= '.') . T.last))
  nullText f = if f == "" then Left f else Right f
