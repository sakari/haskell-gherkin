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
prettyScenario Scenario { scenario_tags
                        , scenario_name
                        , scenario_steps
                        } =
  hsep (map (text . ("@" ++)) scenario_tags) $+$
  text "Scenario:" <+> text scenario_name $+$
  (nest 4 $
   vcat $ map prettyStep scenario_steps)

prettyScenario ScenarioOutline { scenario_tags
                               , scenario_name
                               , scenario_steps
                               , scenario_table
                               } =
  hsep (map (text . ("@" ++)) scenario_tags) $+$
  text "Scenario-outline:" <+>
  text scenario_name $+$
  (nest 4 $
   vcat $ map prettyStep scenario_steps ++
   [text "Examples:"
   , nest 4 $ prettyTable scenario_table
   ]
  )

prettyStep :: Step -> Doc
prettyStep (Step step_prefix _step_pos step_body Nothing) =
  text step_prefix <+> text step_body
prettyStep (Step step_prefix _step_pos step_body (Just arg) ) =
  (text step_prefix <+> text step_body) $+$
  (nest 4 $ prettyBlock arg)

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

