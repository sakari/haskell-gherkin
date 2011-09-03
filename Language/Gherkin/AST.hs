module Language.Gherkin.AST where

data Feature = Feature { feature_tags :: [Tag] 
                       , feature_name :: String
                       , feature_description :: String
                       , feature_background :: Maybe Background
                       , feature_scenarios :: [Scenario]
                       } 
             deriving (Show, Eq)
                                  
type Tag = String 
                  
data Scenario = Scenario { scenario_name :: String
                         , scenario_steps :: [Step]
                         } 
              | ScenarioOutline { scenario_name :: String
                                , scenario_steps :: [Step]
                                , scenario_table :: Table 
                                }
              deriving (Show, Eq)
                
data Table = Table { table_headers :: [String]
                   , table_values :: [[String]]
                   } 
           deriving (Show, Eq)

data Background = Background [Step] deriving (Show, Eq)
                  
data Step = Given StepText
          | Then StepText
          | When StepText
          | And StepText
          deriving (Show, Eq)
                   
data StepText = StepText [Token] (Maybe BlockArg) deriving (Show, Eq)

data Token = Atom String
           | Var String
           deriving (Show, Eq)
                    
data BlockArg = BlockTable Table
              | BlockPystring String
                deriving (Show, Eq)
                         
