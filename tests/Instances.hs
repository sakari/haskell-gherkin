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

descChars :: String
descChars = ['a' .. 'z']

genDescription :: Gen String
genDescription = do
  h <- elements descChars
  t <- listOf $ elements (" \t" ++ descChars)
  e <- elements descChars
  return $ h:t ++ [e]

instance Arbitrary Scenario where
  arbitrary = Scenario <$> genName <*> arbitrary
  
instance Arbitrary Step where
  arbitrary = elements [Given, Then, When, And] <*>
              arbitrary
  shrink (Given steps) = Given <$> shrink steps
  shrink (Then steps) = [Given steps] ++ (Then <$> shrink steps)
  shrink (When steps) = [Given steps] ++ (When <$> shrink steps)
  shrink (And steps) = [Given steps] ++ (And <$> shrink steps)
  

instance Arbitrary Background
instance Arbitrary StepText where
  arbitrary = StepText <$> listOf1 arbitrary <*> arbitrary
  shrink (StepText tokens block) = filter noEmptySteps $ tail' $ StepText 
                                   <$> (tokens : shrink tokens) 
                                   <*> (block : shrink block)
                                     where
                                       noEmptySteps (StepText ts _) = not $ null ts
                                       
  
instance Arbitrary BlockArg where
  arbitrary = oneof [table,  pystring]
    where
      table = BlockTable <$> arbitrary
      pystring = fmap BlockPystring $ listOf $ elements $ 
                 ['a' .. 'z'] ++ "\t\n -_.,'\""
      
instance Arbitrary Table where      
  arbitrary = Table <$> listOf1 genTag <*> listOf (listOf1 genTag)

instance Arbitrary Token where
  arbitrary = oneof [ Atom <$> genTag
                    , Var <$> genTag
                    ]
  shrink (Atom atom) = tail' $ Atom <$> (atom : shrinkTag atom)
  shrink (Var atom) = [Atom atom] ++ (tail' $ Var <$> (atom : shrinkTag atom))

instance Arbitrary Feature where
  arbitrary = Feature <$> listOf genTag <*> 
              genName <*>
              genDescription <*>
              return Nothing <*>
              arbitrary
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