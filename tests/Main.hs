module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Text.Parsec
import Text.Parsec.String
import System.IO.Unsafe
import Language.Gherkin
import Data.Either.Utils

main = defaultMain [testGroup "Parsing tests" tests]

feature = Feature { feature_tags = []
                  , feature_name = "feature"
                  , feature_description = ""
                  , feature_background = Nothing
                  , feature_scenarios = []
                  }

prop p str = case parse p "" str of
  Left e -> error $ show e
  Right l -> l
  
tests = [
  testProperty "parse table" $
  prop parseTable "| a | b |\n|c | d|" == 
  Table { table_headers = [ "a", "b"  ]  
        , table_values =  [[ "c", "d" ]]
        }
  
  , testProperty "parse feature with scenario" $ 
    prop parseFeature  "\nFeature: feature\nScenario: a scenario\nGiven first step\nThen second step" == feature {
      feature_scenarios = [ Scenario { scenario_name = "a scenario"
                                     , scenario_steps = 
                                       [Given 
                                        (StepText [Atom "first"
                                                  , Atom "step"] Nothing)
                                       , Then (StepText [Atom "second"
                                                        , Atom "step"] Nothing)
                                       ]
                                     } ]
      }
  ]