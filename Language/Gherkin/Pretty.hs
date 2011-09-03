{-# LANGUAGE NamedFieldPuns #-}

module Language.Gherkin.Pretty where

import Language.Gherkin.AST
import Text.PrettyPrint

pretty :: Feature -> Doc
pretty Feature { feature_tags
               , feature_name
               , feature_description
               -- , feature_background
               , feature_scenarios
               } = featureTags $$ featureLine $$ nest 4 featureBody
  where
    featureTags = hsep $ fmap (text . ("@" ++)) feature_tags
    featureLine = text "Feature:" <+> text feature_name 
    featureBody = text feature_description $$ 
                  (vcat $ map prettyScenario feature_scenarios)

prettyScenario :: Scenario -> Doc
prettyScenario Scenario { scenario_name
                        , scenario_steps
                        } = text "Scenario:" <+> text scenario_name $$ 
                            (nest 4 $
                             vcat $ map prettyStep scenario_steps)
prettyScenario ScenarioOutline {} = error "tbd"                            
                            
prettyStep :: Step -> Doc
prettyStep = error "tbd"

