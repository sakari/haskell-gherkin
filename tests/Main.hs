module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Text.Parsec
import Text.Parsec.String
import Instances ()
import Language.Gherkin
import Text.PrettyPrint

main :: IO ()
main = defaultMain [testGroup "Parsing tests" tests
                   , testGroup "Pretty roundtrip" prettyTests
                   ]

feature :: Feature
feature = Feature { feature_tags = []
                  , feature_name = "feature"
                  , feature_description = ""
                  , feature_background = Nothing
                  , feature_scenarios = []
                  }

prop :: Parser a -> String -> a
prop p str = case parse (p >>= \r -> eof >> return r) "" str of
  Left e -> error $ show e
  Right l -> l
  
(=.=) :: (Show a, Eq a) => a -> a -> Bool
l =.= r | l /= r = error $ "Expected '"  ++ show r ++ "'\nGot '" ++ show l
        | otherwise = True
                      

prettyTests :: [Test]
prettyTests = [
  testProperty "Feature" $ \f ->
   prop parseFeature (render $ pretty f) =.= f
  
  , testProperty "Step" $ \s ->
   prop parseStep (render $ prettyStep s) =.= s
  
  , testProperty "Scenario" $ \s -> 
   prop parseScenario (render $ prettyScenario s) =.= s
   
  , testProperty "StepText" $ \s ->
   prop parseStepText (render $ prettyStepText s) =.= s
   
  , testProperty "BlockText" $ \b ->
   prop parseBlockText (render $ prettyBlock b) =.= b
  ]

tests :: [Test]
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
    
  , testProperty "parse block argument to step" $
    prop parseStep "Given step:\n| a |\n| b|" =.=
    Given (StepText [Atom "step"]
           $ Just $ BlockTable $ 
           Table { table_headers = ["a"]
                 , table_values = [["b"]]
                 }
          )
    
  , testProperty "parse table argument" $
    prop parseBlockText "| a |\n|b|" =.=
    (BlockTable $ Table { table_headers = ["a"] 
                        ,table_values = [["b"]]
                        })
    
  , testProperty "parse pystring argument" $ 
    prop parseBlockText "\"\"\"\nfoobar\nbar\n\"\"\"" =.=
    (BlockPystring "foobar\nbar")
    
  , testProperty "a single newline inside pystring" $ 
    prop parseBlockText "\"\"\"\n\n\n\"\"\"" =.=
    (BlockPystring "\n")

  , testProperty "pystrings are indented according to start quotes" $
    prop parseBlockText (unlines $ 
                         fmap ("  " ++) 
                         ["\"\"\""   
                         , "No indent"
                         , " One indent"
                         , "\"\"\"" ]) =.=
    (BlockPystring "No indent\n One indent")

  , testProperty "parse feature tags" $
    prop parseFeature "@fst @snd\nFeature: feature\n" =.=
    feature { feature_tags = ["fst", "snd"] }
    
  , testProperty "allow empty scenarios" $
    prop parseFeature "Feature: feature\nScenario: empty scenario\nScenario: second empty" =.=
    feature { feature_scenarios = [ Scenario { scenario_name = "empty scenario"
                                             , scenario_steps = []}
                                  , Scenario { scenario_name = "second empty"
                                             , scenario_steps = []}
                                  ]}
    
  , testProperty "parse feature description" $
    prop parseFeature "Feature: feature\ndescription first line\nsecond line" =.=
    feature { feature_description = "description first linesecond line" }
  
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