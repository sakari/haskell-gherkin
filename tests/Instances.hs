{-# LANGUAGE NamedFieldPuns #-}

module Instances where
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Language.Gherkin
import Control.Applicative

tagChars :: String
tagChars = ['a' .. 'z']

genTag :: Gen String
genTag = listOf1 $ elements tagChars

shrinkTag :: String -> [String]
shrinkTag tag = filter notEmpty $ shrinkList1 (`elem` tagChars) tag
  where
    notEmpty t = not $ null t

shrinkTags :: [String] -> [[String]]
shrinkTags = shrinkList shrinkTag

shrinkList1 :: Arbitrary a => (a -> Bool) -> [a] -> [[a]]
shrinkList1 p ls = filter null $ fmap (filter p) $ shrink ls

shrinkName :: String -> [String]
shrinkName name = shrinkList1 (`elem` nameChars) name

nameChars :: String
nameChars = ['a' .. 'z'] ++
         ['0' .. '9'] ++
         "+/"

genName :: Gen String
genName = do
  h <- elements nameChars
  t <- listOf $ elements (' ':nameChars)
  e <- elements nameChars
  return $ h:t ++ [e]

genStepText :: Gen String
genStepText = do
  h <- elements stepChars
  t <- listOf $ elements (' ':stepChars)
  e <- elements stepChars
  return $ h:t ++ [e]

stepChars :: String
stepChars = ['a' .. 'z'] ++ ['0' .. '9'] ++ "\"'-_<>()[]{}.,;"

descChars :: String
descChars = ['a' .. 'z']

genDescription :: Gen String
genDescription = do
  h <- elements descChars
  t <- listOf $ elements (" \t" ++ descChars)
  e <- elements descChars
  return $ h:t ++ [e]

smaller :: Gen a -> Gen a
smaller gen = sized $ \s -> resize (s `div` 2) gen

zero_pos :: Pos
zero_pos = Pos { pos_path = "path", pos_column = 0, pos_line = 0 }

instance Arbitrary Scenario where
  arbitrary = oneof [Scenario
                     <$> listOf1 genTag
                     <*> genName
                     <*> smaller arbitrary
                     <*> return zero_pos
                    , ScenarioOutline
                      <$> listOf1 genTag
                      <*> genName
                      <*> smaller arbitrary
                      <*> smaller arbitrary
                      <*> return zero_pos
                    ]
  shrink (Scenario tags name steps pos) =
    tail' $ Scenario tags <$>
    (name : shrinkName name) <*>
    (steps : shrink steps)  <*>
    return pos
  shrink (ScenarioOutline tags name steps table pos) =
    tail' $ ScenarioOutline tags <$>
    (name : shrinkName name) <*>
    (steps : shrink steps) <*>
    (table : shrink table) <*>
    return pos

instance Arbitrary Step where
  arbitrary = do
    prefix <- elements ["Given"
                       , "Then"
                       , "When"
                       , "And"]
    Step prefix zero_pos <$> smaller genStepText <*> smaller arbitrary
  shrink Step { step_body = body , step_arg = arg } =
    filter (not . emptyStep) $ Step "Given" zero_pos
    <$> shrink body
    <*> shrink arg
    where
      emptyStep = null . step_body

instance Arbitrary Background where
  arbitrary = Background <$> listOf1 arbitrary
  shrink (Background steps) = tail' $ Background <$> filter (not . null) (steps:shrink steps)

instance Arbitrary BlockArg where
  arbitrary = smaller $ oneof [table, pystring]
    where
      pystring = BlockPystring <$> arbitrary
      table = BlockTable <$> arbitrary

instance Arbitrary Table where
  arbitrary = Table <$> listOf1 genTag <*> listOf1 (listOf1 genTag)
  shrink Table { table_headers
               , table_values } = tail' $ Table <$>
                                  (table_headers : shrinkRow table_headers) <*>
                                  (table_values : shrinkRows table_values)
                                    where
                                      shrinkRow [_] = []
                                      shrinkRow (_:as) = [as]
                                      shrinkRow _ = error "shrinkRow"
                                      shrinkRows [[_]] = []
                                      shrinkRows [(_:as)] = [[as]]
                                      shrinkRows _ = error "shrinkRows"

instance Arbitrary Feature where
  arbitrary = Feature <$> smaller (listOf genTag) <*>
              smaller genName <*>
              smaller genDescription <*>
              smaller arbitrary <*>
              smaller arbitrary <*>
              return zero_pos
  shrink Feature {
    feature_tags
    , feature_name
    , feature_description
    , feature_background
    , feature_scenarios
    , feature_position
    } = tail' $ Feature <$>
        (feature_tags : shrinkTags feature_tags) <*>
        (feature_name : shrinkName feature_name) <*>
        (feature_description : shrink feature_description) <*>
        (feature_background : shrink feature_background) <*>
        (feature_scenarios : shrink feature_scenarios) <*>
        return feature_position

tail' :: [a] -> [a]
tail' [] = []
tail' (_:as) = as
