let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<'
\ ])

setl tabstop=2 shiftwidth=2 expandtab

" In the context of <template>, <script>, and <style>, another contexts error is annoying
let b:ale_enabled = v:false
