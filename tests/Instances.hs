{-# LANGUAGE NamedFieldPuns #-}

module Instances where
import Test.QuickCheck
import Language.Gherkin
import Control.Applicative


tag :: Gen String
tag = listOf1 $  elements $ ['a' .. 'z'] ++ 
      ['0' .. '9']


instance Arbitrary Scenario where
  arbitrary = error "tbd"
  
instance Arbitrary Background where
  arbitrary = error "tbd"
  
instance Arbitrary Feature where
  arbitrary = Feature <$> listOf tag <*> 
              return "feature" <*>
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
        shrink feature_tags <*>
        shrink feature_name <*>
        shrink feature_description <*>
        shrink feature_background <*>
        shrink feature_scenarios

tail' :: [a] -> [a]
tail' [] = []
tail' (_:as) = as