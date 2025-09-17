local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  s('hr', {
    t('//hr')
  }),

  s('h1', {
    t('= '), i(0, 'chapter_title')
  }),

  s('column', {
    t('====[column] '), i(1, 'column title'), t('\n'),
    t('\n'),
    i(0), t('\n'),
    t('\n'),
    t('====[/column]')
  }),

  s({'comment', 'com'}, {
    t('#@#')
  }),

  s({'tt', 't'}, {
    t('@<tt>{'), i(0, 'here'), t('}')
  }),

  s('b', {
    t('@<b>{'), i(0, 'here'), t('}')
  }),

  s('i', {
    t('@<i>{'), i(0, 'here'), t('}')
  }),

  s('u', {
    t('@<u>{'), i(0, 'here'), t('}')
  }),

  s('code', {
    t('@<code>{'), i(0, 'here'), t('}')
  }),

  s({'idx', 'index'}, {
    t('@<idx>{'), i(0, 'here'), t('}')
  }),

  s({'hidx', 'hindex'}, {
    t('@<hidx>{'), i(0, 'here'), t('}')
  }),

  s({'img', 'img_inline', 'inline_img', 'iimg'}, {
    t('@<img>{'), i(0, 'here'), t('}')
  }),

  s({'fn', 'link_to_footnote'}, {
    t('@<fn>{'), i(0, 'here'), t('}')
  }),

  s('ruby', {
    t('@<ruby>{'), i(1, 'value'), t(', '), i(0, 'ruby'), t('}')
  }),

  s('strong', {
    t('@<strong>{'), i(0, 'here'), t('}')
  }),

  s('href', {
    t('@<href>{'), i(1, 'URL'), t(', '), i(0, 'content'), t('}')
  }),

  s('href_same', {
    t('@<href>{'), i(1, 'URL'), t(', '), i(1), t('}')
  }),

  s({'list_inline', 'inline_list', 'ilist', 'listi'}, {
    t('@<list>{'), i(0, 'here'), t('}')
  }),

  s('mapfile', {
    t('#@mapfile('), i(1, 'filePath'), t(')'), t('\n'),
    t('#@end')
  }),

  s('mapoutput', {
    t('#@mapoutput('), i(1, 'cli-command'), t(')'), t('\n'),
    t('#@end')
  }),

  s('maprange', {
    t('#@maprange('), i(1, 'filePath'), t(', '), i(2, 'rangeName'), t(')'), t('\n'),
    t('#@end')
  }),

  s('footnote', {
    t('//footnote['), i(1, 'number'), t(']['), i(0, 'sentence_or_url_or_else'), t(']')
  }),

  s('image', {
    t('//image['), i(0, 'image_name_without_the_extension'), t(']['), i(1, 'name_for_display'), t(']')
  }),

  s({'numberlessimage', 'numberless_image'}, {
    t('//numberlessimage['), i(0, 'image_name_without_the_extension'), t(']['), i(1, 'name_for_display'), t(']')
  }),

  s('quote', {
    t('//quote{'), t('\n'),
    i(0, 'sentence'), t('\n'),
    t('//}')
  }),

  s('note', {
    t('//note['), i(1, 'subject'), t(']{'), t('\n'),
    i(0, 'sentence'), t('\n'),
    t('//}')
  }),

  s({'emlist', 'numberless_list'}, {
    t('//emlist['), i(1, 'subject'), t(']['), i(2, 'filetype'), t(']{'), t('\n'),
    i(0), t('\n'),
    t('//}')
  }),

  s({'list', 'bl'}, {
    t('//list['), i(1, 'identifier'), t(']['), i(2, 'subject'), t(']['), i(3, 'filetype'), t(']{'), t('\n'),
    i(0), t('\n'),
    t('//}')
  }),

  s({'list_haskell', 'code_block_haskell', 'blhs'}, {
    t('//list['), i(1, 'identifier'), t(']['), i(2, 'subject'), t('][haskell]{'), t('\n'),
    i(0), t('\n'),
    t('//}')
  }),

  s('graph', {
    t('//graph['), i(1, 'file-name'), t(']['), i(2, 'cmd-name'), t(']['), i(3, 'caption'), t(']{'), t('\n'),
    i(0), t('\n'),
    t('//}')
  }),

  s({'graph_graphviz', 'graphviz'}, {
    t('//graph['), i(1, 'file-name'), t('][graphviz]['), i(2, 'caption'), t(']{'), t('\n'),
    i(0), t('\n'),
    t('//}')
  }),

  s('table', {
    t('//table['), i(1, 'identifier'), t(']['), i(2, 'caption'), t(']{'), t('\n'),
    i(3, 'columns_split_by_hardtab'), t('\n'),
    t('------------'), t('\n'),
    i(0), t('\n'),
    t('//}')
  }),

  s('embed', {
    t('//embed{'), t('\n'),
    i(0), t('\n'),
    t('//}')
  })
}

return M