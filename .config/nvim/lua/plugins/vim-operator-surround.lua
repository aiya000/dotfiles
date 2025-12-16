local list = require('utils.list')

return {
  'rhysd/vim-operator-surround',
  dependencies = { 'kana/vim-operator-user' },
  init = function()
    -- Basic symbols excluding brackets () [] {} and ` for unique mappings
    local common_symbol_blocks =
      vim.iter(list.concat(list.char_range('!', "'"), { '*', '&', '_', '|', '~', ':', '/' }))
        :map(function(sym)
          return {
            block = { sym, sym },
            motionwise = { 'char', 'line', 'block' },
            keys = { sym },
          }
        end)
        :totable()
    local common_blocks = list.concat(common_symbol_blocks, {
      -- English
      { block = { '(', ')' }, motionwise = { 'char', 'line', 'block' }, keys = { '(', ')', 'p' } },
      { block = { '[', ']' }, motionwise = { 'char', 'line', 'block' }, keys = { ']', 'k' } },
      { block = { '{', '}' }, motionwise = { 'char', 'line', 'block' }, keys = { '{', '}', 'P' } },
      { block = { '<', '>' }, motionwise = { 'char', 'line', 'block' }, keys = { '<', '>', 'K' } },
      { block = { ' ', ' ' }, motionwise = { 'char', 'line', 'block' }, keys = { '  ' } },
      { block = { '`', '`' }, motionwise = { 'char', 'line', 'block' }, keys = { '`', 'b' } },
      -- 日本語
      { block = { '（', '）' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jp' } },
      { block = { '「', '」' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jk' } },
      { block = { '【', '】' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jK' } },
      { block = { '『', '』' }, motionwise = { 'char', 'line', 'block' }, keys = { 'j-k' } },
    })

    local common_markdown_blocks = {
      { block = { '**', '**' }, motionwise = { 'char', 'block' }, keys = { 'B' } },
      { block = { '~~', '~~' }, motionwise = { 'char', 'block' }, keys = { '~' } },
      -- { block = { '__', '__' }, motionwise = { 'char', 'block'  } } -- あんまり使わないから定義しないでおく
      { block = { '```', '```' }, motionwise = { 'line', 'block' }, keys = { '```' } },
    }

    local html_blocks = {
      { block = { '<p>', '</p>' }, motionwise = { 'char' }, keys = { '[p' } },
      { block = { '<a>', '</a>' }, motionwise = { 'char' }, keys = { '[a' } },
      { block = { '<div>', '</div>' }, motionwise = { 'char' }, keys = { '[d' } },
      { block = { '<span>', '</span>' }, motionwise = { 'char' }, keys = { '[s' } },
      { block = { '<h1>', '</h1>' }, motionwise = { 'char' }, keys = { '[h1' } },
      { block = { '<h2>', '</h2>' }, motionwise = { 'char' }, keys = { '[h2' } },
      { block = { '<h3>', '</h3>' }, motionwise = { 'char' }, keys = { '[h3' } },
      { block = { '<h4>', '</h4>' }, motionwise = { 'char' }, keys = { '[h4' } },
      { block = { '<h5>', '</h5>' }, motionwise = { 'char' }, keys = { '[h5' } },
      { block = { '<ol>', '</ol>' }, motionwise = { 'char' }, keys = { '[ol' } },
      { block = { '<ul>', '</ul>' }, motionwise = { 'char' }, keys = { '[ul' } },
      { block = { '<li>', '</li>' }, motionwise = { 'char' }, keys = { '[li' } },
    }

    vim.g['operator#surround#blocks'] = {
      ['-'] = common_blocks,
      markdown = list.concat(common_markdown_blocks, html_blocks, {
        -- これらはHTMLではご法度なので、Markdownのみで使う（README.mdなどで使うと便利）
        { block = { '<b>', '</b>' }, motionwise = { 'char' }, keys = { '[b' } },
        { block = { '<code>', '</code>' }, motionwise = { 'char' }, keys = { '[c' } },
      }),
      html = html_blocks,
      lua = {
        { block = { '[[', ']]' }, motionwise = { 'char', 'line', 'block' }, keys = { '[', ']' } },
      },
      review = {
        { block = { '@<b>{', '}' }, motionwise = { 'char' }, keys = { 'B' } },
        { block = { '@<i>{', '}' }, motionwise = { 'char' }, keys = { 'i' } },
        { block = { '@<u>{', '}' }, motionwise = { 'char' }, keys = { 'u' } },
        { block = { '@<tt>{', '}' }, motionwise = { 'char' }, keys = { 't' } },
        { block = { '@<idx>{', '}' }, motionwise = { 'char' }, keys = { 'x' } },
        { block = { '@<ruby>{', ', ruby}' }, motionwise = { 'char' }, keys = { 'r' } },
        { block = { '@<code>{', '}' }, motionwise = { 'char' }, keys = { 'c' } },
        { block = { '@<mathcode>{', '}' }, motionwise = { 'char' }, keys = { 'm' } },
        { block = { '@<img>{', '}' }, motionwise = { 'char' }, keys = { '[i' } },
        { block = { '@<list>{', '}' }, motionwise = { 'char' }, keys = { '[l' } },
      },
      vue = html_blocks,
      ['typescript.tsx'] = html_blocks,
    }
  end,
}
