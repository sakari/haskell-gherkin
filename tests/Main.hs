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
  
l =.= r | l /= r = error $ "Expected '"  ++ show l ++ "'\nGot '" ++ show r
        | otherwise = True
                      
tests = [
  testProperty "parse table" $
  prop parseTable "| a | b |\n|c | d|" =.= 
  Table { table_headers = [ "a", "b"  ]  
        , table_values =  [[ "c", "d" ]]
        }
  
  , testProperty "parse scenario-outline" $ 
    prop parseFeature "Feature: feature\nScenario-outline: scenario\nGiven bar\n| header |\n|value|" =.=
    feature { feature_scenarios = 
              [ScenarioOutline { scenario_name = "scenario"
                               , scenario_steps = 
                                 [Given $ StepText 
                                  [Atom "bar"] Nothing] 
                               , scenario_table = 
                                 Table { table_headers = 
                                            ["header"]
                                       , table_values =
                                              [["value"]]
                                       }
                               }
              ]}
    
  , testProperty "parse feature tags" $
    prop parseFeature "@fst @snd\nFeature: feature\n" =.=
    feature { feature_tags = ["fst", "snd"] }
    
  , testProperty "parse feature description" $
    prop parseFeature "Feature: feature\ndescription first line\nsecond line" =.=
    feature { feature_description = "description first line\nsecond line" }
  
  , testProperty "parse feature with scenario" $ 
    prop parseFeature  "\nFeature: feature\nScenario: a scenario\nGiven first step\nThen second step" =.= feature {
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