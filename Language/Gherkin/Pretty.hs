{-# LANGUAGE NamedFieldPuns #-}

module Language.Gherkin.Pretty where

import Language.Gherkin.AST
import Text.PrettyPrint

pretty :: Feature -> Doc
pretty Feature { feature_tags
               , feature_name
               , feature_description
               -- , feature_background
               -- , feature_scenarios
               } = featureTags $$ featureLine $$ nest 4 featureBody
  where
    featureTags = hsep $ fmap (text . ("@" ++)) feature_tags
    featureLine = text "Feature:" <+> text feature_name 
    featureBody = text feature_description
