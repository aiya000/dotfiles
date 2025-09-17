local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local sm = function(trigger_table, nodes, opts)
  local result = {}
  for _, trigger in ipairs(trigger_table) do
    table.insert(result, s(trigger, nodes, opts))
  end
  return result
end

local pragma_snippets = {}

-- Basic pragma
table.insert(pragma_snippets, s("pragma", {t("{-# "), i(0), t(" #-}")}))

-- OPTIONS_GHC pragma
vim.list_extend(pragma_snippets, sm(
  {"pragma_options_ghc", "options_ghc"},
  {t("{-# OPTIONS_GHC "), i(1), t(" #-}")},
  {key = "pragma_options_ghc"}
))

-- ANN pragma
table.insert(pragma_snippets, s("pragma_ann", {t("{-# ANN "), i(1, "target"), t(" \""), i(2, "detail"), t("\" #-}")}))

-- INLINE/NOINLINE pragmas
vim.list_extend(pragma_snippets, sm(
  {"pragma_inline", "inline"},
  {t("{-# INLINE "), i(0, "funcName"), t(" #-}")},
  {key = "pragma_inline"}
))

vim.list_extend(pragma_snippets, sm(
  {"pragma_noinline", "noinline"},
  {t("{-# NOINLINE "), i(0, "funcName"), t(" #-}")},
  {key = "pragma_noinline"}
))

-- RULES pragma
table.insert(pragma_snippets, s("pragma_rules", {t("{-# RULES \""), i(1, "name"), t("\" "), i(0, "expr"), t(" #-}")}))

-- LANGUAGE pragmas
vim.list_extend(pragma_snippets, sm(
  {"pragma_language"},
  {t("{-# LANGUAGE "), i(1), t(" #-}")},
  {key = "pragma_language"}
))

-- Specific language extensions
table.insert(pragma_snippets, s("pragma_language_template_haskell", t("{-# LANGUAGE TemplateHaskell #-}")))
table.insert(pragma_snippets, s("pragma_language_derive_typeable", t("{-# LANGUAGE DeriveDataTypeable #-}")))
table.insert(pragma_snippets, s("pragma_language_overloaded_strings", t("{-# LANGUAGE OverloadedStrings #-}")))
table.insert(pragma_snippets, s("pragma_language_exteded_default_rules", t("{-# LANGUAGE ExtendedDefaultRules #-}")))
table.insert(pragma_snippets, s("pragma_language_derive_generic", t("{-# LANGUAGE DeriveGeneric #-}")))
table.insert(pragma_snippets, s("pragma_language_scoped_type_variables", t("{-# LANGUAGE ScopedTypeVariables #-}")))
table.insert(pragma_snippets, s("pragma_language_arrows", t("{-# LANGUAGE Arrows #-}")))
table.insert(pragma_snippets, s("pragma_language_gadts", t("{-# LANGUAGE GADTs #-}")))
table.insert(pragma_snippets, s("pragma_language_type_families", t("{-# LANGUAGE TypeFamilies #-}")))
table.insert(pragma_snippets, s("pragma_language_generalized_newtype_deriving", t("{-# LANGUAGE GeneralizedNewtypeDeriving #-}")))
table.insert(pragma_snippets, s("pragma_language_functional_dependencies", t("{-# LANGUAGE FunctionalDependencies #-}")))
table.insert(pragma_snippets, s("pragma_language_multi_param_type_classes", t("{-# LANGUAGE MultiParamTypeClasses #-}")))
table.insert(pragma_snippets, s("pragma_language_flexible_contexts", t("{-# LANGUAGE FlexibleContexts #-}")))
table.insert(pragma_snippets, s("pragma_language_existential_quantification", t("{-# LANGUAGE ExistentialQuantification #-}")))
table.insert(pragma_snippets, s("pragma_language_type_operators", t("{-# LANGUAGE TypeOperators #-}")))
table.insert(pragma_snippets, s("pragma_language_recursive_do", t("{-# LANGUAGE RecursiveDo #-}")))
table.insert(pragma_snippets, s("pragma_language_flexible_instances", t("{-# LANGUAGE FlexibleInstances #-}")))
table.insert(pragma_snippets, s("pragma_language_data_kinds", t("{-# LANGUAGE DataKinds #-}")))
table.insert(pragma_snippets, s("pragma_language_lambda_case", t("{-# LANGUAGE LambdaCase #-}")))
table.insert(pragma_snippets, s("pragma_language_type_synonym_instances", t("{-# LANGUAGE TypeSynonymInstances #-}")))
table.insert(pragma_snippets, s("pragma_language_rank_n_types", t("{-# LANGUAGE RankNTypes #-}")))
table.insert(pragma_snippets, s("pragma_language_overloaded_lists", t("{-# LANGUAGE OverloadedLists #-}")))
table.insert(pragma_snippets, s("pragma_language_multi_way_if", t("{-# LANGUAGE MultiWayIf #-}")))
table.insert(pragma_snippets, s("pragma_language_standalone_deriving", t("{-# LANGUAGE StandaloneDeriving #-}")))
table.insert(pragma_snippets, s("pragma_language_undecidable_instances", t("{-# LANGUAGE UndecidableInstances #-}")))
table.insert(pragma_snippets, s("pragma_language_default_signatures", t("{-# LANGUAGE DefaultSignatures #-}")))
table.insert(pragma_snippets, s("pragma_language_pattern_synonyms", t("{-# LANGUAGE PatternSynonyms #-}")))
table.insert(pragma_snippets, s("pragma_language_record_wild_cards", t("{-# LANGUAGE RecordWildCards #-}")))
table.insert(pragma_snippets, s("pragma_language_kind_signatures", t("{-# LANGUAGE KindSignatures #-}")))
table.insert(pragma_snippets, s("pragma_language_pattern_guards", t("{-# LANGUAGE PatternGuards #-}")))
table.insert(pragma_snippets, s("pragma_language_poly_kinds", t("{-# LANGUAGE PolyKinds #-}")))
table.insert(pragma_snippets, s("pragma_language_type_in_type", t("{-# LANGUAGE TypeInType #-}")))
table.insert(pragma_snippets, s("pragma_language_instance_sigs", t("{-# LANGUAGE InstanceSigs #-}")))
table.insert(pragma_snippets, s("pragma_language_quasi_quotes", t("{-# LANGUAGE QuasiQuotes #-}")))
table.insert(pragma_snippets, s("pragma_language_overlapping_instances", t("{-# LANGUAGE OverlappingInstances #-}")))
table.insert(pragma_snippets, s("pragma_language_incoherent_instances", t("{-# LANGUAGE IncoherentInstances #-}")))
table.insert(pragma_snippets, s("pragma_language_view_patterns", t("{-# LANGUAGE ViewPatterns #-}")))
table.insert(pragma_snippets, s("pragma_language_tuple_sections", t("{-# LANGUAGE TupleSections #-}")))
table.insert(pragma_snippets, s("pragma_language_liberal_type_synonyms", t("{-# LANGUAGE LiberalTypeSynonyms #-}")))
table.insert(pragma_snippets, s("pragma_language_overloaded_labels", t("{-# LANGUAGE OverloadedLabels #-}")))
table.insert(pragma_snippets, s("pragma_language_type_applications", t("{-# LANGUAGE TypeApplications #-}")))
table.insert(pragma_snippets, s("pragma_language_duplicate_record_fields", t("{-# LANGUAGE DuplicateRecordFields #-}")))
table.insert(pragma_snippets, s("pragma_language_constraint_kinds", t("{-# LANGUAGE ConstraintKinds #-}")))
table.insert(pragma_snippets, s("pragma_language_magic_hash", t("{-# LANGUAGE MagicHash #-}")))
table.insert(pragma_snippets, s("pragma_language_explicit_namespaces", t("{-# LANGUAGE ExplicitNamespaces #-}")))
table.insert(pragma_snippets, s("pragma_language_partial_type_signatures", t("{-# LANGUAGE PartialTypeSignatures #-}")))
table.insert(pragma_snippets, s("pragma_language_no_implicit_prelude", t("{-# LANGUAGE NoImplicitPrelude #-}")))
table.insert(pragma_snippets, s("pragma_language_rebidable_syntax", t("{-# LANGUAGE RebindableSyntax #-}")))
table.insert(pragma_snippets, s("pragma_language_constrained_class_methods", t("{-# LANGUAGE ConstrainedClassMethods #-}")))
table.insert(pragma_snippets, s("pragma_language_impredicative_polymorphism", t("{-# LANGUAGE ImpredicativeTypes #-}")))
table.insert(pragma_snippets, s("pragma_language_named_field_puns", t("{-# LANGUAGE NamedFieldPuns #-}")))
table.insert(pragma_snippets, s("pragma_language_derive_any_class", t("{-# LANGUAGE DeriveAnyClass #-}")))
table.insert(pragma_snippets, s("pragma_language_strict", t("{-# LANGUAGE Strict #-}")))
table.insert(pragma_snippets, s("pragma_language_strict_data", t("{-# LANGUAGE StrictData #-}")))
table.insert(pragma_snippets, s("pragma_language_unboxed_sums", t("{-# LANGUAGE UnboxedSums #-}")))
table.insert(pragma_snippets, s("pragma_language_implicit_params", t("{-# LANGUAGE ImplicitParams #-}")))
table.insert(pragma_snippets, s("pragma_language_applicative_do", t("{-# LANGUAGE ApplicativeDo #-}")))
table.insert(pragma_snippets, s("pragma_language_cpp", t("{-# LANGUAGE CPP #-}")))
table.insert(pragma_snippets, s("pragma_language_allow_ambiguous_types", t("{-# LANGUAGE AllowAmbiguousTypes #-}")))
table.insert(pragma_snippets, s("pragma_language_deriving_strategies", t("{-# LANGUAGE DerivingStrategies #-}")))
table.insert(pragma_snippets, s("pragma_language_deriving_via", t("{-# LANGUAGE DerivingVia #-}")))
table.insert(pragma_snippets, s("pragma_language_applying_via", t("{-# LANGUAGE ApplyingVia #-}")))
table.insert(pragma_snippets, s("pragma_language_block_arguments", t("{-# LANGUAGE BlockArguments #-}")))

return pragma_snippets