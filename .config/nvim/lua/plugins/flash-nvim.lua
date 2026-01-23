local list = require('utils.list')

return {
  'folke/flash.nvim',
  event = 'VeryLazy',
  config = function()
    -- TODO: 下記[1]のTODO参照
    vim.keymap.set('n', '<leader>f.', 'f。', { remap = true })

    require('flash').setup({
      labels = 'asdfghjklqwertyuiozxcvbnm', -- 'p' is excluded because it forcely pastes text
      jump = {
        nohlsearch = true,
      },
      modes = {
        char = {
          char_actions = function()
            -- ftは常に右、FTは常に左
            return {
              ['f'] = 'right',
              ['F'] = 'left',
              ['t'] = 'right',
              ['T'] = 'left',
            }
          end,
        },
      },
    })
  end,
  keys = {
    {
      'g;',
      mode = { 'n', 'x', 'o' },
      function()
        require('flash').treesitter()
      end,
      desc = 'Flash Treesitter',
    },
    {
      ';;',
      mode = { 'n', 'x', 'o' },
      function()
        require('flash').jump()
      end,
      desc = 'Flash',
    },
    {
      ';:', -- kensaku-jump
      mode = { 'n', 'x', 'o' },
      function()
        require('flash').jump({
          search = {
            mode = function(str)
              return vim.fn['kensaku#query'](str)
            end,
          },
        })
      end,
      desc = 'Flash with Kensaku',
    },
    {
      'gl', -- Line jump
      mode = { 'n', 'x', 'o' },
      function()
        local col = vim.fn.col('.')
        require('flash').jump({
          search = { mode = 'search', max_length = 0 },
          label = {
            after = { 0, 0 },
            current = false,
            min_pattern_length = 0,
            style = 'overlay',
          },
          pattern = '\\%' .. col .. 'c', -- Show labels on current column position
          labels = table.concat(list.char_range('a', 'z'), ''),
        })
        vim.schedule(function()
          vim.fn.cursor(vim.fn.line('.'), col)
        end)
      end,
      desc = 'Flash Line Jump',
    },
    -- Interactive fmap selector
    {
      '<leader>f',
      mode = { 'n', 'x', 'o' },
      function()
        local fmap_chars = {
          -- TODO: [1] 多分だけどflash.nvimが<leader>f.をハンドリングしてしまっていて、こちらでハンドリングできない（直前にf{char}をしていれば再度f{char}が、そうでなければエラーが出てしまう）ので、今は'>'（Shift + '.'）で代用する。できるときにflash.nvimの<leader>f.（？）にハンドルされないようにする。↑で仮対応済み
          -- ['.'] = '。',
          [','] = '、',
          ['!'] = '！',
          ['?'] = '？',
          ['T'] = '・',
          ['p'] = '（',
          ['k'] = '「',
          ['K'] = '〈',
          [' '] = '　',
          ['tt'] = '…',
          ['-k'] = '『',
        }

        vim.api.nvim_echo({ { 'fmap: ', 'Question' } }, false, {})
        local stroke = ''
        local pattern = nil
        while true do
          local char_code = vim.fn.getchar()
          -- TODO: nvim.escaping_keysを参照したい
          if char_code == 27 or char_code == 12 then -- if <Esc>, <C-[>, or <C-l>
            break
          end

          stroke = stroke .. vim.fn.nr2char(char_code)
          if fmap_chars[stroke] ~= nil then
            pattern = fmap_chars[stroke]
            break
          end
        end

        if pattern then
          vim.fn.feedkeys('f' .. pattern)
        else
          vim.api.nvim_echo({ { ('Canceled (stroke = %s)'):format(stroke), 'ErrorMsg' } }, false, {})
        end
      end,
      desc = 'Interactive fmap selector with flash.nvim',
    },
  },
}
