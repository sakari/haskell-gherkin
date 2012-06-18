module Language.Gherkin.Parser where

import Language.Gherkin.AST
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String
import Data.Char
import Data.List
import Control.Applicative

get_position :: Parser Pos
get_position = do
  p <- getPosition
  return $ Pos { pos_path = sourceName p
               , pos_column = sourceColumn p
               , pos_line = sourceLine p
               }

parseFeature :: Parser Feature
parseFeature = do
  pos <- get_position
  tags <- parseTags
  string_ "Feature:"
  name <- parseLine
  description <- parseDescription
  background <- optionMaybe parseBackground
  scenarios <- many $ (parseScenario <|> parseScenarioOutline) <* spaces
  eof
  return $ Feature { feature_tags = tags
                   , feature_name = name
                   , feature_description = description
                   , feature_background = background
                   , feature_scenarios = scenarios
                   , feature_position = pos
                   }

emptyLines :: Parser ()
emptyLines = skipMany $ try $ ws >> newline_

parseTags :: Parser [Tag]
parseTags = option [] $ line $ parseTag `sepBy` ws
  where
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
                                    , string_ "@"
                                    , eof])

parseBackground :: Parser Background
parseBackground = do
  line $ string_ "Background:"
  Background `fmap` many1 parseStep

parseStep :: Parser Step
parseStep = (parseGiven <|>
             parseWhen <|>
             parseThen <|>
             parseAnd) <?> "a scenario step"

stepStart :: String -> Parser Pos
stepStart step = try $ do
  ws
  pos <- get_position
  string_ step
  return pos

parseGiven :: Parser Step
parseGiven = stepStart "Given" >>= \pos -> Given pos `fmap` parseStepText

parseWhen :: Parser Step
parseWhen = stepStart "When" >>= \pos -> When pos `fmap` parseStepText

parseThen :: Parser Step
parseThen = stepStart "Then" >>= \pos -> Then pos `fmap` parseStepText

parseAnd :: Parser Step
parseAnd = stepStart "And" >>= \pos -> And pos `fmap` parseStepText

parseScenarioOutline :: Parser Scenario
parseScenarioOutline = scenarioOutline
  where
    scenarioOutline = do
      pos <- get_position
      tags <- try $ parseTags `followedBy`
              (ws >> string_ "Scenario-outline:")
      name <- parseLine
      steps <- many parseStep
      line $ string_ "Examples:"
      table <- parseTable
      return $ ScenarioOutline { scenario_tags = tags
                               , scenario_name = name
                               , scenario_steps = steps
                               , scenario_table = table
                               , scenario_position = pos
                               }

followedBy :: Parser a -> Parser () -> Parser a
followedBy p by = do
  r <- p
  by
  return r

parseScenario :: Parser Scenario
parseScenario = do
  pos <- get_position
  tags <- try $ parseTags `followedBy`
          (ws >> string_ "Scenario:")
  name <- parseLine
  steps <- many parseStep
  spaces_
  return $ Scenario { scenario_tags = tags
                    , scenario_name = name
                    , scenario_steps = steps
                    , scenario_position = pos
                    }

parseStepText :: Parser StepText
parseStepText = do
  step <- parseLine
  block <- optionMaybe $ try $ parseBlockText
  return $ StepText step block

parseTable :: Parser Table
parseTable = do
  header <- parseRow
  values <- many parseRow
  return $ Table { table_headers = header
                 , table_values = values
                 }

parseRow :: Parser [String]
parseRow = do
  try $ ws >> string_ "|"
  r <- endBy1 (many1 $ noneOf "|\n") $ string "|"
  ws >> lineEnd
  return $ fmap strip r

parseBlockText :: Parser BlockArg
parseBlockText = parsePystring <|> parseBlockTable
  where
    parseBlockTable = BlockTable `fmap` parseTable
    startOfPystring = do
      i <- many $ oneOf " \t"
      line $ string_ "\"\"\""
      return i
    parsePystring =  do
      indent <- try $ startOfPystring
      let ln = ((string_ indent >> parseWholeLine) <|>
                (ws >> newline >> return []))
      pystrings <- manyTill ln $ (try $ line $ string_ "\"\"\"")
      return $ BlockPystring $ concat $ intersperse "\n" pystrings

line :: Parser a -> Parser a
line p = between ws (ws >> lineEnd) p

parseWholeLine :: Parser String
parseWholeLine = manyTill anyChar $ try lineEnd

parseLine :: Parser String
parseLine = fmap strip $ parseWholeLine

spaces_ :: Parser ()
spaces_ = const () `fmap` spaces

string_ :: String -> Parser ()
string_ s = const () `fmap` string s

lineEnd :: Parser ()
lineEnd = newline_ <|> lookAhead eof

newline_ :: Parser ()
newline_  = const () `fmap` newline

ws :: Parser ()
ws = skipMany $ string " " <|> string "\t"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

