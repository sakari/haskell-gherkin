{-# LANGUAGE DeriveDataTypeable #-}
module Language.Gherkin.AST where
import Data.Typeable
import Data.Data

data Pos = Pos {
  pos_path :: FilePath
  , pos_column :: Int
  , pos_line :: Int
  } deriving (Eq, Show, Typeable, Data)

data Feature = Feature { feature_tags :: [Tag]
                      , feature_name :: String
                      , feature_description :: String
                      , feature_background :: Maybe Background
                      , feature_scenarios :: [Scenario]
                      , feature_position :: Pos
                      }
             deriving (Show, Eq, Typeable, Data)

type Tag = String

data Scenario = Scenario { scenario_tags :: [Tag]
                         , scenario_name :: String
                         , scenario_steps :: [Step]
                         , scenario_position :: Pos
                         }
                | ScenarioOutline { scenario_tags :: [Tag]
                                  , scenario_name :: String
                                  , scenario_steps :: [Step]
                                  , scenario_table :: Table
                                  , scenario_position :: Pos
                                  }
                deriving (Show, Eq, Typeable, Data)

data Table = Table { table_headers :: [String]
                   , table_values :: [[String]]
                   }
           deriving (Show, Eq, Typeable, Data)

data Background = Background [Step]
                deriving (Show, Eq, Typeable, Data)

data Step = Given { step_position :: Pos, step_text :: StepText}
          | Then { step_position :: Pos, step_text :: StepText }
          | When { step_position :: Pos, step_text :: StepText }
          | And { step_position :: Pos, step_text :: StepText }
          deriving (Show, Eq, Typeable, Data)

data StepText = StepText { step_body :: String,
                           step_arg :: Maybe BlockArg
                         }
              deriving (Show, Eq, Typeable, Data)

data BlockArg = BlockTable Table
              | BlockPystring String
              deriving (Show, Eq, Typeable, Data)

