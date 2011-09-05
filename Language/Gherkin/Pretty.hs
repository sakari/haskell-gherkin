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
               } = featureTags $+$ featureLine $+$ nest 4 featureBody
  where
    featureTags = hsep $ fmap (text . ("@" ++)) feature_tags
    featureLine = text "Feature:" <+> text feature_name 
    featureBody = text feature_description $+$ 
                  (vcat $ map prettyScenario feature_scenarios)

prettyScenario :: Scenario -> Doc
prettyScenario Scenario { scenario_name
                        , scenario_steps
                        } = text "Scenario:" <+> text scenario_name $+$ 
                            (nest 4 $
                             vcat $ map prettyStep scenario_steps)
prettyScenario ScenarioOutline {} = error "tbd"                            
                            
prettyStep :: Step -> Doc
prettyStep (Given s) = text "Given" <+> prettyStepText s
prettyStep (Then s) = text "Then" <+> prettyStepText s
prettyStep (When s) = text "When" <+> prettyStepText s
prettyStep (And s) = text "And" <+> prettyStepText s

prettyStepText :: StepText -> Doc
prettyStepText (StepText tokens maybeBlock) = 
  (hsep $ map prettyToken tokens ) <> maybe empty go maybeBlock 
  where
    go block = text ":" $+$ prettyBlock block

prettyBlock :: BlockArg -> Doc
prettyBlock (BlockPystring str) = text "\"\"\"" $+$ 
                                  text str $+$ 
                                  text "\"\"\""
prettyBlock (BlockTable table) = prettyTable table

prettyTable :: Table -> Doc
prettyTable Table { table_headers, table_values } = 
  row table_headers $+$ 
  (vcat $ map row table_values)
    where
      row rs = text "|" <+> 
               hsep (punctuate (text " | ") $ map text rs) <+> 
               text "|"

prettyToken :: Token -> Doc
prettyToken (Atom str) = text str 
prettyToken (Var str) = text $ "<" ++ str ++ ">"
  