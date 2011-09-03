module Instances where
import Test.QuickCheck
import Language.Gherkin

newtype GF = GF Feature
             deriving (Eq, Show)

instance Arbitrary GF where
  arbitrary = return $ GF $ Feature {
    feature_tags = []
    , feature_name = "feature"
    , feature_description = "description"
    , feature_background = Nothing
    , feature_scenarios = []
    }