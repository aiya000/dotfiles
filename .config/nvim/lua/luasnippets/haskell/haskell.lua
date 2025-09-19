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

local haskell_snippets = {}

-- Data type snippets
table.insert(haskell_snippets, s("data", {
  t("data "), i(1, "Name"), t(" = "), i(1), i(0), t({"", "  deriving (Show, Eq)"})
}))

table.insert(haskell_snippets, s("prod", {
  t("data "), i(1, "Name"), t(" = "), i(1), i(2), t({"", "  { "}), i(0), t({"", "  } deriving (Show, Eq)"})
}))

table.insert(haskell_snippets, s("newtype", {
  t("newtype "), i(1, "Name"), t(" = "), i(1), i(2), t({"", "  { "}), i(0), t({"", "  } deriving (Show, Eq)"})
}))

table.insert(haskell_snippets, s("type", {t("type "), i(1, "Name"), t(" = "), i(0)}))

-- Deriving snippets
vim.list_extend(haskell_snippets, sm(
  {"deriving", "der", "deri"},
  {t("deriving ("), i(0, "here"), t(")")},
  {key = "deriving"}
))

table.insert(haskell_snippets, s("deriving_stock", {t("deriving stock ("), i(0, "here"), t(")")}))
table.insert(haskell_snippets, s("deriving_anyclass", {t("deriving anyclass ("), i(0, "here"), t(")")}))
table.insert(haskell_snippets, s("deriving_newtype", {t("deriving newtype ("), i(0, "here"), t(")")}))

table.insert(haskell_snippets, s("deriving_via", {t("deriving "), i(1), t(" via "), i(0)}))

-- Class and instance snippets
table.insert(haskell_snippets, s("class", {t("class "), i(1, "Name"), t(" where"), i(0)}))
table.insert(haskell_snippets, s("instance", {t("instance "), i(1, "Class"), t(" "), i(2, "Name"), t(" where"), i(0)}))

-- Control flow snippets
table.insert(haskell_snippets, s("case", {
  t("case "), i(1, "x"), t(" of"), t({"", "    "}), i(2, "pattern"), t(" -> "), i(0)
}))

table.insert(haskell_snippets, s("if", {
  t("if "), i(1, "cond"), t({"", "    then "}), i(2), t({"", "    else "}), i(0)
}))

-- Import snippets
vim.list_extend(haskell_snippets, sm(
  {"import", "imp"},
  {t("import "), i(1)},
  {key = "import"}
))

vim.list_extend(haskell_snippets, sm(
  {"import_qualified", "imq"},
  {t("import qualified "), i(1), t(" as "), i(2)},
  {key = "import_qualified"}
))

table.insert(haskell_snippets, s("hiding", {t("hiding ("), i(0, "here"), t(")")}))

-- Infix snippets
table.insert(haskell_snippets, s("infixl", {t("infixl "), i(1, "3"), t(" "), i(2, "<>")}))
table.insert(haskell_snippets, s("infixr", {t("infixr "), i(1, "3"), t(" "), i(2, "<>")}))

-- Other basic snippets
table.insert(haskell_snippets, s("default", {t("default ("), i(1, "Int"), t(")")}))
table.insert(haskell_snippets, s("module", {t("module "), i(1, "ModuleName"), t(" where"), i(0)}))

vim.list_extend(haskell_snippets, sm(
  {"to", "constraints_for"},
  t("=>"),
  {key = "to"}
))

table.insert(haskell_snippets, s("ar", t("->")))

-- Main function template
table.insert(haskell_snippets, s("main", {
  t({"main :: IO ()", "main = "}), i(0)
}))

-- Comment snippets
vim.list_extend(haskell_snippets, sm(
  {"comment_block", "com"},
  {t("{-"), i(1), t("-}")},
  {key = "comment_block"}
))

vim.list_extend(haskell_snippets, sm(
  {"document_comment", "haddock_comment", "doc"},
  t("-- |"),
  {key = "document_comment"}
))

vim.list_extend(haskell_snippets, sm(
  {"document_comment_back", "doc_back", "back_doc", "bak", "doca"},
  t("-- ^"),
  {key = "document_comment_back"}
))

table.insert(haskell_snippets, s("output", {
  t({"-- {output}", "-- "}), i(0)
}))

-- Liquid Haskell snippets
vim.list_extend(haskell_snippets, sm(
  {"liquid_refinement_type_func", "liquid_refinement_type"},
  {t("{-@ "), i(1, "funcName"), t(" :: "), i(2), t(" @-}")},
  {key = "liquid_refinement_type_func"}
))

table.insert(haskell_snippets, s("liquid_refinement_type_type", {t("{-@ type "), i(1, "typeName"), t(" = "), i(2), t(" @-}")}))

-- Unboxed sums
table.insert(haskell_snippets, s("unboxed_sums_type", {t("(# "), i(1, "X"), t(" | "), i(2, "Y"), t(" #)")}))

vim.list_extend(haskell_snippets, sm(
  {"unboxed_sums_pattern_first", "unboxed_sums_expression_first"},
  {t("#( "), i(0), t(" | #)")},
  {key = "unboxed_sums_pattern_first"}
))

vim.list_extend(haskell_snippets, sm(
  {"unboxed_sums_pattern_second", "unboxed_sums_expression_second"},
  {t("#( | "), i(0), t(" #)")},
  {key = "unboxed_sums_pattern_second"}
))

-- Doctest snippet
table.insert(haskell_snippets, s("doctest_block", {
  t({">>> :{", ""}), i(0), t({"", "-- >>> :}"})
}))

-- QuasiQuote snippets
vim.list_extend(haskell_snippets, sm(
  {"quasi_quote", "q"},
  {t("["), i(1, "r"), t("|"), i(0), t("|]")},
  {key = "quasi_quote"}
))

vim.list_extend(haskell_snippets, sm(
  {"quasi_quote_i", "qi", "i"},
  {t("[i|"), i(0), t("|]")},
  {key = "quasi_quote_i"}
))

vim.list_extend(haskell_snippets, sm(
  {"quasi_quote_r", "qr", "r"},
  {t("[r|"), i(0), t("|]")},
  {key = "quasi_quote_r"}
))

vim.list_extend(haskell_snippets, sm(
  {"quasi_quote_s", "qs", "s"},
  {t("[s|"), i(0), t("|]")},
  {key = "quasi_quote_s"}
))

vim.list_extend(haskell_snippets, sm(
  {"quasi_quote_here", "qhere", "here"},
  {t("[here|"), i(0), t("|]")},
  {key = "quasi_quote_here"}
))

table.insert(haskell_snippets, s("import_data_string_here_i", t("import Data.String.Here (i)")))

vim.list_extend(haskell_snippets, sm(
  {"here_string_interpolation", "interp"},
  {t("${"), i(1, "expr"), t("}"), i(0)},
  {key = "here_string_interpolation"}
))

-- Error and debug snippets
vim.list_extend(haskell_snippets, sm(
  {"raise_fatal_error", "fatal_error"},
  {t("["), i(1, "i"), t("|"), i(2, "funcName"), t(" at ${(__FILE__ :: String)}:L${(__LINE__ :: Int)}: fatal error! Sorry, please report an issue :(|]")},
  {key = "raise_fatal_error"}
))

vim.list_extend(haskell_snippets, sm(
  {"expand_debug_modules", "unsafe_debug", "debug_unsafe"},
  {t([[
--TODO: Remove these import
import qualified GHC.Stack as Debug
import qualified Debug.Trace as Debug
import qualified System.IO.Unsafe as Debug
--

--TODO: Remove this function
unsafePrint :: Show a => a -> ()
unsafePrint = Debug.unsafePerformIO . print

--TODO: Remove this function
ninjaOfUnsafe :: Show a => a -> a
ninjaOfUnsafe x = unsafePrint x `seq` x

--TODO: Remove this function
currentFuncName :: IO String
currentFuncName = Debug.currentCallStack >>= \xs -> case xs of
  [] -> error "no callstack is found (do you `stack build --profile`?)"
  _  -> return . head . words $ xs !! (length xs - 2)]])},
  {key = "expand_debug_modules"}
))

-- Utility snippets
vim.list_extend(haskell_snippets, sm(
  {"println_for", "println", "pr", "print"},
  {t("print $ "), i(0)},
  {key = "println_for"}
))

vim.list_extend(haskell_snippets, sm(
  {"putStrLn", "puts", "ps"},
  t("putStrLn "),
  {key = "putStrLn"}
))

-- Lens snippets
vim.list_extend(haskell_snippets, sm(
  {"lens_get", "get"},
  t("^."),
  {key = "lens_get"}
))

vim.list_extend(haskell_snippets, sm(
  {"lens_set", "set"},
  t(".~"),
  {key = "lens_set"}
))

vim.list_extend(haskell_snippets, sm(
  {"prism_get", "look", "lk"},
  t("^?"),
  {key = "prism_get"}
))

-- String utility functions
vim.list_extend(haskell_snippets, sm(
  {"trimMargins", "stripMargins"},
  {t([[
trimMargins :: String -> String
trimMargins = trim . unlines . map trim . lines
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')]])},
  {key = "trimMargins"}
))

table.insert(haskell_snippets, s("flattenMargins", {t([[
flattenMargins :: String -> String
flattenMargins = replace . unlines . filter (/= "") . map (dropWhile (== ' ')) . lines
    where
        replace [] = []
        replace ('\n' : xs) = ' ' : replace xs
        replace (x : xs) = x : replace xs]])}))

table.insert(haskell_snippets, s("undefined", t("Prelude.undefined")))

return haskell_snippets