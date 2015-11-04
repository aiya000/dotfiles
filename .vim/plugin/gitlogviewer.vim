" Inspired by ujihisa's vimrc
" And deris's code (http://deris.hatenablog.jp/entry/2013/05/10/003430)
command! -bar -nargs=* GitLogViewer call gitlogviewer#git_log_viewer(<q-args>)
