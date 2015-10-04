map <C-n> :!ros %<CR>
map <C-m> :!ros %<CR>

let g:slimv_repl_split = 4
let g:slimv_lisp='/usr/local/bin/ros -Q'
let g:slimv_swank_cmd = '!osascript ~/.swank/start-swank.scpt'
let g:slimv_clhs_root = 'file://$HOME/dev/HyperSpec/Body/'

map ,w ,c<CR>
map ,q :!osascript ~/.swank/stop-swank.scpt<CR><CR>
map ,a ,q,w

let g:paredit_mode = 0
