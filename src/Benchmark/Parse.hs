{-# LANGUAGE OverloadedStrings #-}

module Benchmark.Parse where

import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.HashMap.Strict ((!?))
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import Data.Vector (toList)
import Grammar
import Pretty

type OutputParser = Object -> Lit

parseJsonLine :: String -> Object
parseJsonLine jsonStr = json where Just (Object json) = decodeStrict (pack jsonStr)

defaultOutputParser :: OutputType -> OutputParser
defaultOutputParser gType json = parseJsonLit gType val where Just val = json !? "output1"

parseJsonLit :: OutputType -> Value -> Lit
parseJsonLit GInt (Number sci) = IntLit $ round $ toRealFloat sci
parseJsonLit GFloat (Number sci) = FloatLit $ toRealFloat sci
parseJsonLit GBool (Bool b) = BoolLit b
parseJsonLit GChar (String txt) = CharLit $ T.head txt
parseJsonLit (GList GChar) (String txt) = stLit $ T.unpack txt
parseJsonLit (GList gt) (Array vec) = ListLit gt $ toList $ fmap (parseJsonLit gt) vec
parseJsonLit typ val = error $ unwords ["Could not parse", pretty typ, "from", show val]

(^.) :: Object -> String -> Value
(^.) json key = let Just val = json !? T.pack key in val