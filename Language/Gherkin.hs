module Language.Gherkin where

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
                         
parseFeature :: Parser Feature
parseFeature = do
  tags <- option [] $ line $ parseTag `sepBy` ws 
  string "Feature:"
  name <- parseLine
  description <- parseDescription
  background <- return Nothing -- optionMaybe parseBackground  
  scenarios <- many (parseScenario <|> parseScenarioOutline)
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
parseDescription = fmap join $ description
  where
    description = try (manyTill parseLine end) <?> "description"
    join = concat . intersperse "\n"
    end = try $ ws >> 
          ( choice $ 
            map (lookAhead . try) $ [ string_ "Scenario-outline:"
                                    , string_ "Scenario:"
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

parseScenarioOutline :: Parser Scenario
parseScenarioOutline = scenarioOutline <?> "scenario outline"
  where
    scenarioOutline = do
      try $ ws >> string "Scenario-outline:"
      name <- parseLine
      steps <- many $ try $ line parseStep
      table <- parseTable
      return $ ScenarioOutline { scenario_name = name
                               , scenario_steps = steps
                               , scenario_table = table 
                               }

parseScenario :: Parser Scenario
parseScenario = scenario <?> "scenario"
  where
    scenario = do
      try $ ws >> string "Scenario:"
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
  block <- optionMaybe parseBlockText
  return $ StepText tokens block
  
parseToken :: Parser Token
parseToken = parseVar <|> parseAtom
  where
    parseVar = fmap Var $ between (string "<") (string ">") $ 
               manyTill anyChar $ lookAhead $ string ">"
    parseAtom = Atom `fmap` many1 alphaNum

parseTable :: Parser Table
parseTable = do
  header <- parseRow
  values <- many parseRow
  return $ Table { table_headers = header
                 , table_values = values
                 }
  
parseRow = fmap (map strip) $ line $ char '|' >> endBy go (char '|')
  where
    go = try $ do
      r <- many (noneOf "|\n")
      lookAhead $ string_ "|"
      return r

parseBlockText :: Parser BlockArg
parseBlockText = line (char ':') >> (parseBlockTable <|> parsePystring)
  where
    parseBlockTable = BlockTable `fmap` try parseTable 
    parsePystring =  try $ do
      indent <- many $ oneOf " \t"
      line $ string "\"\"\""
      let ln = string_ indent >> parseWholeLine
      lines <- manyTill ln $ (try $ line $ string_ "\"\"\"")
      return $ BlockPystring $ unlines lines

line :: Parser a -> Parser a
line p = between ws (ws >> (newline_ <|> eof)) p

parseWholeLine :: Parser String
parseWholeLine = manyTill anyChar $ try $ eof <|> newline_

parseLine :: Parser String
parseLine = fmap strip $ parseWholeLine
  
spaces_ :: Parser ()            
spaces_ = const () `fmap` spaces  

string_ s = const () `fmap` string s

newline_  = const () `fmap` newline

ws :: Parser ()
ws = skipMany $ string " " <|> string "\t"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace