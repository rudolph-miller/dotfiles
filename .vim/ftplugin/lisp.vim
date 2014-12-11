map <C-n> :!sbcl --script %<CR>
map <C-b> :!sbcl --load %<CR>

" let g:paredit_mode=0
let g:slimv_repl_split = 4
let g:slimv_lisp='/usr/local/bin/ros -Q'
let g:slimv_swank_cmd = '!osascript ~/.swank/start-swank.scpt'
let g:slimv_clhs_root = 'file://$HOME/dev/HyperSpec/Body/'

map ,w ,c<CR>
map ,q :!osascript ~/.swank/stop-swank.scpt<CR><CR>
map ,a ,q,w
