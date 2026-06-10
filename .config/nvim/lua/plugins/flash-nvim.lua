local list = require('utils.list')

local function input_for_kensaku_jump()
  local Input = require('nui.input')
  local event = require('nui.utils.autocmd').event

  local input = Input({
    position = '50%',
    size = {
      width = 20,
    },
    border = {
      style = 'single',
      text = {
        top = ' Kensaku Flash ',
        top_align = 'center',
      },
    },
  }, {
    prompt = '> ',
    on_submit = function(inputted)
      vim.schedule(function()
        -- TODO: nui.input.Inputをsubmitしてからなんらかの文字1文字を入力しないと、なぜか候補が表示されないので、今はテキトーに1文字入力して運用する。もし何かわかれば直す（なお1文字を入力するタイミングがflash.jumpのsearch.modeでreturnした後なので、同期処理では無理だった）
        require('flash').jump({
          search = {
            mode = function()
              return vim.fn['kensaku#query'](inputted)
            end,
          },
          jump = {
            register = true,
            history = true,
          },
        })
      end)
    end,
  })

  input:mount()
  input:on(event.BufLeave, function()
    input:unmount()
  end)
end

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
      desc = 'Flash Treesitter',
      mode = { 'n', 'x', 'o' },
      function()
        require('flash').treesitter()
      end,
    },

    {
      ';:',
      desc = 'Flash with Kensaku',
      mode = { 'n', 'x', 'o' },
      input_for_kensaku_jump,
    },

    {
      ';;',
      desc = 'Flash',
      mode = { 'n', 'x', 'o' },
      function()
        require('flash').jump({
          jump = {
            register = true,
            history = true,
          },
        })
      end,
    },

    {
      'gl',
      desc = 'Flash Line Jump',
      mode = { 'n', 'x', 'o' },
      function()
        require('flash').jump({
          search = { mode = 'search', max_length = 0 },
          label = {
            after = { 0, 0 },
            current = false,
            min_pattern_length = 0,
            style = 'overlay',
          },
          pattern = '^', -- Show labels at the beginning of every line
          labels = table.concat(list.char_range('a', 'z'), ''),
        })
      end,
    },

    {
      '<leader>f',
      desc = 'Interactive fmap selector with flash.nvim',
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
    },
  },
}
