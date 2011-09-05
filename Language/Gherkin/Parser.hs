module Language.Gherkin.Parser where
       
import Language.Gherkin.AST
import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List

parseFeature :: Parser Feature
parseFeature = do
  tags <- option [] $ line $ parseTag `sepBy` ws 
  string_ "Feature:"
  name <- parseLine
  description <- parseDescription
  background <- return Nothing -- optionMaybe parseBackground  
  scenarios <- many (parseScenario <|> parseScenarioOutline)
  eof
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
parseDescription = fmap concat $ description
  where
    description = try (manyTill parseLine end) <?> "description"
    end = try $ ws >> 
          ( choice $ 
            map (lookAhead . try) $ [ string_ "Scenario-outline:"
                                    , string_ "Scenario:"
                                    , string_ "Feature:"
                                    , string_ "Background:"
                                    , eof])
  
parseBackground :: Parser Background
parseBackground = do
  string_ "Background:"
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
      try $ ws >> string_ "Scenario-outline:"
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
      try $ ws >> string_ "Scenario:"
      name <- parseLine
      steps <- many $ try $ line parseStep
      spaces_
      return $ Scenario { scenario_name = name
                        , scenario_steps = steps
                        }

parseStepText :: Parser StepText
parseStepText = do
  ws
  stepTokens <- parseToken `sepBy1` ws
  block <- optionMaybe parseBlockText
  return $ StepText stepTokens block
  
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
  
parseRow :: Parser [String]
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
      line $ string_ "\"\"\""
      let ln = ((string_ indent >> parseWholeLine) <|> (ws >> newline >> return []))
      pystrings <- manyTill ln $ (try $ line $ string_ "\"\"\"")
      return $ BlockPystring $ concat $ intersperse "\n" pystrings

line :: Parser a -> Parser a
line p = between ws (ws >> (newline_ <|> lookAhead eof)) p

parseWholeLine :: Parser String
parseWholeLine = manyTill anyChar $ try $ newline_ <|> lookAhead eof

parseLine :: Parser String
parseLine = fmap strip $ parseWholeLine
  
spaces_ :: Parser ()            
spaces_ = const () `fmap` spaces  

string_ :: String -> Parser ()
string_ s = const () `fmap` string s

newline_ :: Parser ()
newline_  = const () `fmap` newline

ws :: Parser ()
ws = skipMany $ string " " <|> string "\t"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace