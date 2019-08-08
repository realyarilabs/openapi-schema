{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Utils where


import           Control.Arrow ((&&&))
import           Control.Monad
import           Data.Aeson (KeyValue, ToJSON, (.=))
import           Data.Bifunctor (bimap)
import           Data.Either (either)
import           Data.List
import           Data.Maybe (isNothing, maybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Data.Traversable (sequence)
import           OpenAPI.Types (Responses (..))

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

{-
  Takes a record that has a `[b]` as a member, and checks if each `b` is unique.
  In order to determine if each `b` is unique, a function of type `b -> a` is
  applied.
  If all elements are unique, return the original record.
  If not all elements are unique, return an error.

  For a record `r`:
    - `fl` gets the [b]
    - `fe` transforms the `b`s in `a`s
-}
noRepRecord :: (Ord a, Eq b) => (t -> [b]) -> (b -> a) -> c -> t -> Either c t
noRepRecord fl fe err r = cond (allDifferent fe) (const (Right r)) (const (Left err)) $ fl r


apIfRight :: Traversable t => c -> (t a -> c) -> t (Either b a) -> c
apIfRight c f = either (const c) f . sequence

maybeRight :: Maybe (Either b a) -> Maybe a
maybeRight = either (const Nothing) id . sequence
{-
  Very if all list elements are unique.
  Even though `tail` is not total, it will never be called on an empty list.
-}
allDifferent :: (Ord a, Eq a, Eq b) => (b -> a) -> [b] -> Bool
allDifferent f = all (null . tail) . group . sortOn f

{-
  Point free if then else, by J.N.O.
-}
cond :: (b -> Bool) -> (b -> c) -> (b -> c) -> b -> c
cond p f g = either f g . grd p where
  grd :: (a -> Bool) -> a -> Either a a
  grd pr x = if pr x then Left x else Right x


isDefault :: Responses -> Bool
isDefault (Default _)  = True
isDefault (Status _ _) = False
