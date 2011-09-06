{-# LANGUAGE NamedFieldPuns #-}

module Language.Gherkin.Pretty where

import Language.Gherkin.AST
import Text.PrettyPrint

pretty :: Feature -> Doc
pretty Feature { feature_tags
               , feature_name
               , feature_description
               , feature_background
               , feature_scenarios
               } = featureTags $+$ featureLine $+$ nest 4 featureBody
  where
    featureTags = hsep $ fmap (text . ("@" ++)) feature_tags
    featureLine = text "Feature:" <+> text feature_name 
    featureBody = text feature_description $+$ 
                  maybe empty prettyBackground feature_background $+$
                  (vcat $ map prettyScenario feature_scenarios)

prettyBackground :: Background -> Doc
prettyBackground (Background steps) = text "Background:" $+$
                                      (nest 4 $ 
                                       vcat $ map prettyStep steps)

prettyScenario :: Scenario -> Doc
prettyScenario Scenario { scenario_name
                        , scenario_steps
                        } = text "Scenario:" <+> text scenario_name $+$ 
                            (nest 4 $
                             vcat $ map prettyStep scenario_steps)
prettyScenario ScenarioOutline {} = error "tbd"                            
                            
prettyStep :: Step -> Doc
prettyStep (Given s) = prettyStepText "Given" s
prettyStep (Then s) = prettyStepText "Then" s
prettyStep (When s) = prettyStepText "When" s
prettyStep (And s) =  prettyStepText "And" s

prettyStepText :: String -> StepText -> Doc
prettyStepText key (StepText tokens Nothing) = 
  text key <+> (hsep $ map prettyToken tokens)
prettyStepText key (StepText tokens (Just arg)) =   
  (text key <+> (hsep $ map prettyToken tokens) <> text ":") $+$ 
  prettyBlock arg
  
prettyBlock :: BlockArg -> Doc
prettyBlock (BlockPystring str) = 
  text "\"\"\"" $+$ 
  vcat (map text lns)  $+$ 
  text "\"\"\""
    where                                       
      lns | null str = []  
          | last str == '\n' = lines str ++ [""]  
          | otherwise = lines str
prettyBlock (BlockTable table) = prettyTable table

prettyTable :: Table -> Doc
prettyTable Table { table_headers, table_values } = 
  vcat $ map prettyRow $ table_headers:table_values
      
prettyRow :: [String] -> Doc
prettyRow rs = text "|" <+> 
               hsep (punctuate (text " | ") $ map text rs) <+> 
               text "|"

prettyToken :: Token -> Doc
prettyToken (Atom str) = text str 
prettyToken (Var str) = text $ "<" ++ str ++ ">"
  