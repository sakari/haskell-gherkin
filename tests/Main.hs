module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Text.Parsec
import Text.Parsec.String
import System.IO.Unsafe
import Language.Gherkin
import Data.Either.Utils

main = defaultMain [testGroup "Parsing tests" tests]


tests = [
  testProperty "parse table" $
  fromRight (parse parseTable "" "| a | b |\n|c | d|") == 
  Table { table_headers = [ "a", "b"  ]  
        , table_values =  [[ "c", "d" ]]
        }
  ]