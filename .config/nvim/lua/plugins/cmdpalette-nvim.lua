local fn = require('utils.functions')
local nvim = require('nvim')

---cnoremapのcmdpalette版
local function define_cmdpalette_keymaps()
  nvim.replace_line(
    {
      ['l '] = 'lua ',
      ['lp'] = 'lua = ',
      ['h '] = 'Telescope help_tags<CR>',
      ['ev'] = ('e %s/'):format(InitLua.path_at_started),
      ['eb'] = function()
        return ('e %s/'):format(vim.fn.expand('#:p:h')) -- cmdpaletteを開く直前に開いていたバッファの親ディレクトリ
      end,
      ['eg'] = function()
        return ('e %s/'):format(InitLua.git_root) -- nvim.git_rootは代入が遅延されるので、評価も遅延
      end,
      [':'] = function()
        local backup = vim.opt.cmdheight
        return fn.try_finally(function()
          vim.opt.cmdheight = 2 -- `cmdheight = 1`だと`nvim.prompt()`が表示されないため
          local next_char = nvim.prompt("':' or other char")
          if next_char == ':' then
            -- See below <C-l><C-l> keymapping
            return '<C-l><C-l>q:%s/' -- :::でcmdpaletteに突入した場合、cmd-modeで:%s/に突入するように
          else
            return '<Esc>' .. next_char -- ::でcmdpaletteに突入した場合、normal-modeで突入するように
          end
        end, function()
          vim.opt.cmdheight = backup
        end)
      end,
    },
    vim.api.nvim_get_current_line(),
    function(rhs)
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(rhs, true, false, true), 'i', false)
    end
  )
end

return {
  'hachy/cmdpalette.nvim',
  config = function()
    require('cmdpalette').setup({
      buf = {
        filetype = 'cmdpalette', -- See 'nvim-cmp' section
      },
    })

    local augroup = vim.api.nvim_create_augroup('InitLuaPluginsCmdpalette', { clear = true })
    vim.api.nvim_create_autocmd('FileType', {
      group = augroup,
      pattern = 'cmdpalette',
      callback = function()
        vim.api.nvim_create_autocmd('TextChangedI', {
          group = augroup,
          buffer = 0,
          callback = define_cmdpalette_keymaps,
        })

        -- cmdpaletteバッファを閉じた後にエラーが出るので無効化
        vim.b.ale_enabled = 0

        -- - <C-l>だけだと間違えてEscapeすることがよくあるので<C-l><C-l>
        -- - さらなる誤爆Escape対策として@zにバックアップ
        vim.keymap.set('n', '<C-l><C-l>', '"yyy<Esc>', { remap = true, buffer = true })
        vim.keymap.set('n', '<C-j>', '<CR>', { remap = true, buffer = true })
        -- TODO: これで実行した場合、結果をnvim.open_buffer_to_execute()で開くようにする
        -- vim.keymap.set('n', '<C-k><C-j>', function()
        --   local line = vim.api.nvim_get_current_line()
        -- end, { remap = true, buffer = true })

        vim.keymap.set('i', '<C-j>', '<Esc><CR>', { remap = true, buffer = true }) -- <Esc> to hide completion menu
      end,
    })
  end,
  keys = {
    { ':', '<Cmd>Cmdpalette<CR>', desc = 'Open cmdpalette' },
  },
}
