module Internal.Utils where


import           Data.Aeson (KeyValue, ToJSON, (.=))
import           Data.Text  (Text)

cleanMaybes :: (ToJSON a, KeyValue b) => [Maybe a] -> [Text] -> [b]
cleanMaybes [] []               = []
cleanMaybes (Nothing:xs) (y:ys) = [] <> cleanMaybes xs ys
cleanMaybes (x:xs) (y:ys)       = [y .= x ] <> cleanMaybes xs ys
