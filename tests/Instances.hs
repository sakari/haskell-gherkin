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

instance Arbitrary Scenario where
  arbitrary = oneof [Scenario
                     <$> listOf1 genTag
                     <*> genName
                     <*> smaller arbitrary
                    , ScenarioOutline
                      <$> listOf1 genTag
                      <*> genName
                      <*> smaller arbitrary
                      <*> smaller arbitrary
                    ]
  shrink (Scenario tags name steps) =
    tail' $ Scenario tags <$>
    (name : shrinkName name) <*>
    (steps : shrink steps)
  shrink (ScenarioOutline tags name steps table) =
    tail' $ ScenarioOutline tags <$>
    (name : shrinkName name) <*>
    (steps : shrink steps) <*>
    (table : shrink table)

instance Arbitrary Step where
  arbitrary = elements [Given, Then, When, And] <*> arbitrary
  shrink (Given steps) = Given <$> shrink steps
  shrink (Then steps) = [Given steps] ++ (Then <$> shrink steps)
  shrink (When steps) = [Given steps] ++ (When <$> shrink steps)
  shrink (And steps) = [Given steps] ++ (And <$> shrink steps)


instance Arbitrary Background where
  arbitrary = Background <$> listOf1 arbitrary
  shrink (Background steps) = tail' $ Background <$> filter (not . null) (steps:shrink steps)

instance Arbitrary StepText where
  arbitrary = smaller $
    StepText <$> genStepText <*> arbitrary
  shrink (StepText step block) = filter noEmptySteps $ tail' $ StepText
                                   <$> return step
                                   <*> (block : shrink block)
                                     where
                                       noEmptySteps (StepText ts _) = not $ null ts


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
              smaller arbitrary
  shrink Feature {
    feature_tags
    , feature_name
    , feature_description
    , feature_background
    , feature_scenarios
    } = tail' $ Feature <$>
        (feature_tags : shrinkTags feature_tags) <*>
        (feature_name : shrinkName feature_name) <*>
        (feature_description : shrink feature_description) <*>
        (feature_background : shrink feature_background) <*>
        (feature_scenarios : shrink feature_scenarios)

tail' :: [a] -> [a]
tail' [] = []
tail' (_:as) = as
