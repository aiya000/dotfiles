let b:undo_ftplugin = 'setl ' . join([
\	'ts<',
\	'sw<',
\	'et<'
\])

setl ts=2 sw=2 et

" Use neoclojure.vim completion
setl omnifunc=neoclojure#complete#omni_auto
