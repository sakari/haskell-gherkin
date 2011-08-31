module Gherkin where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List

data Feature = Feature { feature_tags :: [Tag] 
                       , feature_name :: String
                       , feature_description :: String
                       , feature_background :: Maybe Background
                       , feature_scenarios :: [Scenario]
                       } 
             deriving (Show)
                                  
type Tag = String 
         
data Scenario = Scenario { scenario_name :: String
                         , scenario_steps :: [Step]
                         } 
              | ScenarioOutline { scenario_name :: String
                                , scenario_steps :: [Step]
                                , scenario_table :: Table 
                                }
              deriving (Show)
                
data Table = Table { table_headers :: [String]
                   , table_values :: [[String]]
                   } 
           deriving (Show)

data Background = Background [Step] deriving (Show)
                  
data Step = Given StepText
          | Then StepText
          | When StepText
          | And StepText
          deriving (Show)
                   
data StepText = StepText [Token] (Maybe BlockArg) deriving (Show)

data Token = Atom String
           | Var String
           deriving (Show)
                    
data BlockArg = BlockTable Table
              | BlockPystring String
                deriving (Show)
                         
parseFeature :: Parser Feature
parseFeature = do
  tags <- line $ parseTag `sepBy` ws 
  string "Feature:"
  name <- parseLine
  emptyLines
  description <- return "" -- parseDescription
  background <- return Nothing -- optionMaybe parseBackground  
  scenarios <- fmap (:[]) $ parseScenario
  return $ Feature { feature_tags = tags
                   , feature_name = name
                   , feature_description = description
                   , feature_background = background
                   , feature_scenarios = scenarios 
                   }
    
emptyLines :: Parser ()
emptyLines = skipMany $ try $ ws >> newline_

parseTag :: Parser Tag
parseTag = char '@' >> many1 alphaNum

parseDescription :: Parser String
parseDescription = fmap join $ manyTill parseLine descriptionEnd
  where
    join = concat . intersperse "\n"
    descriptionEnd = lookAhead $ ws >> 
                     ( choice $ 
                       map lookAhead $ [ string_ "Scenario:"
                                       , string_ "Scenario-outline:"
                                       , string_ "Feature:"
                                       , string_ "Background:"
                                       , eof])
  
parseBackground :: Parser Background
parseBackground = do
  string "Background:"
  Background `fmap` many1 parseStep
  
parseStep :: Parser Step
parseStep = (parseGiven <|> 
             parseWhen <|>
             parseThen <|>
             parseAnd) <?> "a scenario step"
            
parseGiven :: Parser Step
parseGiven = try $ string "Given" >> Given `fmap` parseStepText

parseWhen :: Parser Step
parseWhen = try $ string "When" >> When `fmap` parseStepText

parseThen :: Parser Step
parseThen = try $ string "Then" >> Then `fmap` parseStepText

parseAnd :: Parser Step
parseAnd = try $ string "And" >> And `fmap` parseStepText

parseScenario :: Parser Scenario
parseScenario = do
  ws >> string "Scenario:"
  name <- parseLine
  steps <- many $ try $ line parseStep
  spaces_
  return $ Scenario { scenario_name = name
                    , scenario_steps = steps
                    }

parseStepText :: Parser StepText
parseStepText = do
  ws
  tokens <- parseToken `sepBy` ws
  -- block <- optionMaybe parseBlockText
  return $ StepText tokens Nothing
  
parseToken :: Parser Token
parseToken = parseVar <|> parseAtom
  where
    parseVar = fmap Var $ between (string "<") (string ">") $ 
               manyTill anyChar $ lookAhead $ string ">"
    parseAtom = Atom `fmap` many1 alphaNum

parseBlockText :: Parser BlockArg
parseBlockText = char ':' >> (parseBlockTable <|> parsePystring)
  where
    parseBlockTable = do
      header <- parseRow 
      values <- many1 parseRow    
      return $ BlockTable $ Table { table_headers = header
                                  , table_values = values
                                  }
    parseRow = line $ many1 parseCell
    parseCell = let bar = ws >> string "|" >> ws
                in bar >> anyChar `endBy` bar
    parsePystring =  
      let limit = string "\"\"\"" 
      in between limit limit $ fmap BlockPystring $ many anyChar

line :: Parser a -> Parser a
line p = between ws (ws >> newline) p

parseLine :: Parser String
parseLine = fmap strip $ manyTill anyChar $ try $ eof <|> newline_
  
spaces_ :: Parser ()            
spaces_ = const () `fmap` spaces  

string_ s = const () `fmap` string s

newline_  = const () `fmap` newline

ws :: Parser ()
ws = skipMany $ string " " <|> string "\t"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace