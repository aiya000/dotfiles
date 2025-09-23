-- Note: This is a sample conversion of the emoji.snip file.
-- The original file is very long (contains hundreds of emoji snippets).
-- This includes the basic numeric and symbol emojis as examples.

local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node

local emoji_snippets = {
  -- Well-known emojis (Supported by GitHub Flavor Markdown)
  s('emoji_hash', t('#')),
  s('emoji_zero', t('0')),
  s('emoji_one', t('1')),
  s('emoji_two', t('2')),
  s('emoji_three', t('3')),
  s('emoji_four', t('4')),
  s('emoji_five', t('5')),
  s('emoji_six', t('6')),
  s('emoji_seven', t('7')),
  s('emoji_eight', t('8')),
  s('emoji_nine', t('9')),
  s('emoji_copyright', t('©')),
  s('emoji_registered', t('®')),

  -- Note: The original emoji.snip file contains many more emoji definitions.
  -- Add more emoji snippets here as needed, following the same pattern:
  -- s("emoji_name", t("emoji_character_or_code"))
}

return { snippets = emoji_snippets, autosnippets = {} }
