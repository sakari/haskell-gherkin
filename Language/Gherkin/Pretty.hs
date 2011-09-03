{-# LANGUAGE NamedFieldPuns #-}

module Language.Gherkin.Pretty where

import Language.Gherkin.AST
import Text.PrettyPrint

pretty :: Feature -> Doc
pretty Feature { -- feature_tags
               feature_name
               , feature_description
               -- , feature_background
               -- , feature_scenarios
               } = text "Feature:" <+> text feature_name $$ nest 4 featureBody
  where
    featureBody = text feature_description
