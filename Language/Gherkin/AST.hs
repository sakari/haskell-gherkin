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

data Step = Given { step_text :: StepText }
          | Then { step_text :: StepText }
          | When { step_text :: StepText }
          | And { step_texs :: StepText }
          deriving (Show, Eq)

data StepText = StepText { step_body :: String,
                           step_arg :: Maybe BlockArg
                         }
              deriving (Show, Eq)

data BlockArg = BlockTable Table
              | BlockPystring String
                deriving (Show, Eq)

