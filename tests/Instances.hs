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

shrinkTags :: [String] -> [[String]]
shrinkTags tags = shrinkList (shrinkList1 (`elem` tagChars)) tags
    
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

instance Arbitrary Scenario where
  arbitrary = error "tbd"
  
instance Arbitrary Background where
  arbitrary = error "tbd"
  
instance Arbitrary Feature where
  arbitrary = Feature <$> listOf genTag <*> 
              genName <*>
              return "description" <*>
              return Nothing <*>
              return []
  shrink Feature {
    feature_tags
    , feature_name
    , feature_description
    , feature_background
    , feature_scenarios
    } = tail' $ Feature <$> 
        shrinkTags feature_tags <*>
        shrinkName feature_name <*>
        shrink feature_description <*>
        shrink feature_background <*>
        shrink feature_scenarios

tail' :: [a] -> [a]
tail' [] = []
tail' (_:as) = as